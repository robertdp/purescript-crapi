module Crapi.Request where

import Prelude
import Apiary.Media (class MediaCodec, decodeMedia)
import Apiary.Url (class UrlParam, decodeUrlParam, encodeUrlParam)
import Control.Alt ((<|>))
import Control.Monad.Error.Class (throwError)
import Control.Monad.Except (lift, runExcept, runExceptT, withExceptT)
import Data.Array as Array
import Data.Either (Either(..), either)
import Data.Maybe (Maybe(..), maybe)
import Data.Nullable as Nullable
import Data.Symbol (class IsSymbol, SProxy(..), reflectSymbol)
import Data.Traversable (sequence)
import Effect.Aff (Aff, launchAff_)
import Effect.Aff.AVar as AVar
import Effect.Class (liftEffect)
import Effect.Exception (catchException)
import Foreign (F, Foreign, ForeignError(..), MultipleErrors, renderForeignError)
import Foreign.Object (Object)
import Foreign.Object as Object
import Node.Buffer (Buffer)
import Node.Buffer as Buffer
import Node.Encoding as Encoding
import Node.HTTP as HTTP
import Node.Stream as Stream
import Node.URL as URL
import Prim.Row (class Cons, class Lacks)
import Prim.RowList (class RowToList, Nil, Cons)
import Record.Builder (Builder)
import Record.Builder as Builder
import Simple.JSON (class ReadForeign, read', readJSON')
import Type.Data.RowList (RLProxy(..))
import Type.Proxy (Proxy)
import Unsafe.Coerce (unsafeCoerce)

type Request params query body
  = { params :: params
    , query :: query
    , headers :: Object String
    , body :: body
    }

type PathParams
  = Object String

type QueryParams
  = Object Foreign

class DecodePathParams params where
  decodePathParams :: PathParams -> F params

instance decodePathParamsRecord ::
  ( RowToList r rl
  , DecodePathParamRecord r rl
  ) =>
  DecodePathParams (Record r) where
  decodePathParams =
    decodePathParamRecord (RLProxy :: _ rl)
      >>> map (flip Builder.build {})

class DecodePathParamRecord r rl | rl -> r where
  decodePathParamRecord :: RLProxy rl -> PathParams -> F (Builder {} (Record r))

instance decodePathParamRecordNil :: DecodePathParamRecord () Nil where
  decodePathParamRecord _ _ = pure identity

instance decodePathParamRecordConsString ::
  ( Cons l String r_ r
  , DecodePathParamRecord r_ rl_
  , IsSymbol l
  , Lacks l r_
  ) =>
  DecodePathParamRecord r (Cons l String rl_) where
  decodePathParamRecord _ f = do
    builder <- decodePathParamRecord (RLProxy :: RLProxy rl_) f
    let
      l = reflectSymbol (SProxy :: SProxy l)
    a <- maybe (throwError (pure (ErrorAtProperty l (ForeignError "missing param")))) pure (Object.lookup l f)
    pure (builder >>> Builder.insert (SProxy :: SProxy l) a)
else instance decodePathParamRecordCons ::
  ( Cons l a r_ r
  , DecodePathParamRecord r_ rl_
  , IsSymbol l
  , Lacks l r_
  , ReadForeign a
  ) =>
  DecodePathParamRecord r (Cons l a rl_) where
  decodePathParamRecord _ f = do
    builder <- decodePathParamRecord (RLProxy :: RLProxy rl_) f
    let
      l = reflectSymbol (SProxy :: SProxy l)
    f_ <- maybe (throwError (pure (ErrorAtProperty l (ForeignError "missing param")))) pure (Object.lookup l f)
    a <- withExceptT (map (ErrorAtProperty l)) (readJSON' f_)
    pure (builder >>> Builder.insert (SProxy :: SProxy l) a)

class DecodeQueryParams params where
  decodeQueryParams :: QueryParams -> F params

instance decodeQueryParamsRecord ::
  ( DecodeQueryParamRecord r rl
    ) =>
  DecodeQueryParams (Record r) where
  decodeQueryParams =
    decodeQueryParamRecord (RLProxy :: _ rl)
      >>> map (flip Builder.build {})

class DecodeQueryParamRecord r rl | rl -> r where
  decodeQueryParamRecord :: RLProxy rl -> QueryParams -> F (Builder {} (Record r))

instance decodeQueryParamRecordNil :: DecodeQueryParamRecord () Nil where
  decodeQueryParamRecord _ _ = pure identity

instance decodeQueryParamRecordConsArray ::
  ( IsSymbol name
  , UrlParam value
  , Cons name (Array value) params' params
  , Lacks name params'
  , DecodeQueryParamRecord params' paramList
  ) =>
  DecodeQueryParamRecord params (Cons name (Array value) paramList) where
  decodeQueryParamRecord _ params = do
    let
      name = SProxy :: _ name

      prop = encodeUrlParam $ reflectSymbol name
    builder <- decodeQueryParamRecord (RLProxy :: _ paramList) params
    value <- case Object.lookup prop params of
      Nothing -> pure []
      Just a -> do
        values <- read' a <|> Array.singleton <$> read' a
        sequence $ decodeUrlParam <$> values
    pure $ Builder.insert name value <<< builder
else instance decodeQueryParamRecordCons ::
  ( IsSymbol name
  , UrlParam value
  , Cons name (Maybe value) params' params
  , Lacks name params'
  , DecodeQueryParamRecord params' paramList
  ) =>
  DecodeQueryParamRecord params (Cons name (Maybe value) paramList) where
  decodeQueryParamRecord _ params = do
    let
      name = SProxy :: _ name

      prop = encodeUrlParam $ reflectSymbol name
    builder <- decodeQueryParamRecord (RLProxy :: _ paramList) params
    value <- case Object.lookup prop params of
      Nothing -> pure Nothing
      Just a -> do
        str <- read' a
        Just <$> decodeUrlParam str
    pure $ Builder.insert name value <<< builder

readBodyAsBuffer :: HTTP.Request -> Aff Buffer
readBodyAsBuffer request = do
  let
    stream = HTTP.requestAsStream request
  bodyResult <- AVar.empty
  chunks <- AVar.new []
  fillResult <-
    liftEffect
      $ catchException (pure <<< Left) (Right <$> fillBody stream chunks bodyResult)
  body <- AVar.take bodyResult
  either throwError pure (fillResult *> body)
  where
  fillBody stream chunks bodyResult = do
    Stream.onData stream \chunk ->
      let
        modification = do
          v <- AVar.take chunks
          AVar.put (v <> [ chunk ]) chunks
      in
        launchAff_ modification
    Stream.onError stream
      $ launchAff_
      <<< flip AVar.put bodyResult
      <<< Left
    Stream.onEnd stream $ launchAff_
      $ AVar.take chunks
      >>= concat'
      >>= (pure <<< Right)
      >>= flip AVar.put bodyResult

  concat' = liftEffect <<< Buffer.concat

decodeRequest ::
  forall params query rep body.
  DecodePathParams params =>
  DecodeQueryParams query =>
  MediaCodec rep body =>
  Proxy rep ->
  PathParams ->
  HTTP.Request ->
  Aff (Either { params :: Array String, query :: Array String, body :: Array String } (Request params query body))
decodeRequest rep pathParams request =
  runExceptT do
    requestBody <- lift $ liftEffect <<< Buffer.toString Encoding.UTF8 =<< readBodyAsBuffer request
    let
      url = URL.parse $ HTTP.requestURL request

      queryParams = maybe Object.empty (coerceQuery <<< URL.parseQueryString) $ Nullable.toMaybe url.query

      decodedParams = runExcept $ decodePathParams pathParams

      decodedQuery = runExcept $ decodeQueryParams queryParams

      headers = HTTP.requestHeaders request

      decodedBody = runExcept $ decodeMedia rep requestBody
    case decodedParams, decodedQuery, decodedBody of
      Right params, Right query, Right body -> pure { params, query, headers, body }
      _, _, _ ->
        throwError
          { params: extractErrors decodedParams
          , query: extractErrors decodedQuery
          , body: extractErrors decodedBody
          }
  where
  coerceQuery = unsafeCoerce :: URL.Query -> QueryParams

  extractErrors :: forall a. Either MultipleErrors a -> Array String
  extractErrors = either (Array.fromFoldable >>> map renderForeignError) (const mempty)

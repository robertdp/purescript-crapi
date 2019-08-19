module Crapi where

import Prelude

import Control.Comonad (extract)
import Control.Monad.Except (mapExcept, runExcept, throwError)
import Data.Bifunctor (bimap, lmap)
import Data.Either (Either(..), either)
import Data.Maybe (maybe)
import Data.Newtype (class Newtype, unwrap)
import Data.Symbol (class IsSymbol, SProxy(..), reflectSymbol)
import Effect (Effect)
import Effect.Aff (Aff, message, runAff_)
import Effect.Aff.Class (class MonadAff)
import Effect.Class (class MonadEffect)
import Foreign (F, ForeignError(..), renderForeignError)
import Foreign.Generic (class Decode, class Encode, decodeJSON, encodeJSON)
import Foreign.Object (Object)
import Foreign.Object as Object
import Node.Encoding as Encoding
import Node.HTTP (Request, Response)
import Node.HTTP as HTTP
import Node.Stream as Stream
import Prim.Row (class Cons, class Lacks)
import Prim.RowList (class RowToList, Nil, Cons)
import Record.Builder (Builder)
import Record.Builder as Builder
import Type.Data.RowList (RLProxy(..))
import Type.Proxy (Proxy(..))


class Servable server where
  serveWith :: server -> Request -> Response -> Object String -> Effect Unit

class IsResponse response where
  encodeResponse :: response -> String
  responseType :: Proxy response -> String

instance isResponseString :: IsResponse String where
  encodeResponse = identity
  responseType _ = "text/plain"

class IsRequest request where
  decodeRequest :: String -> Either String request
  requestType :: Proxy request -> String

instance isRequestString :: IsRequest String where
  decodeRequest = Right
  requestType _ = "text/plain"

newtype JSON a = JSON a

derive instance newtypeJSON :: Newtype (JSON a) _

instance isResponseJSON :: Encode a => IsResponse (JSON a) where
  encodeResponse =
    encodeResponse
    <<< encodeJSON
    <<< unwrap
  responseType _ = "application/json"

instance isRequestJSON :: Decode a => IsRequest (JSON a) where
  decodeRequest =
    bimap (renderForeignError <<< extract) JSON
    <<< runExcept
    <<< decodeJSON
    <=< decodeRequest
  requestType _ = "application/json"

newtype Handler response = Handler (Aff response)

derive instance newtypeMethod :: Newtype (Handler response) _

derive newtype instance functorMethod :: Functor Handler
derive newtype instance applyMethod :: Apply Handler
derive newtype instance applicativeMethod :: Applicative Handler
derive newtype instance bindMethod :: Bind Handler
derive newtype instance monadMethod :: Monad Handler
derive newtype instance monadEffectMethod :: MonadEffect Handler
derive newtype instance monadAffMethod :: MonadAff Handler

instance servableHandler :: IsResponse response => Servable (Handler response) where
  serveWith respond req res params = do
    let outputStream = HTTP.responseAsStream res

        handleError = sendError res 500 "Internal server error" <<< message

        handleResponse r = do
          HTTP.setHeader res "Content-Type" (responseType (Proxy :: Proxy response))
          _ <- Stream.writeString outputStream Encoding.UTF8 (encodeResponse r) (pure unit)
          Stream.end outputStream (pure unit)
    runAff_ (either handleError handleResponse) (unwrap respond)

sendError
  :: Response
  -> Int
  -> String
  -> String
  -> Effect Unit
sendError res code msg body = do
  let outputStream = HTTP.responseAsStream res
  HTTP.setHeader res "Content-Type" "text/plain"
  HTTP.setStatusCode res code
  HTTP.setStatusMessage res msg
  _ <- Stream.writeString outputStream Encoding.UTF8 body (pure unit)
  Stream.end outputStream (pure unit)

newtype Params params = Params params

instance servableParams
    :: (Servable service, RowToList params paramList, DecodeParams params paramList)
    => Servable (Params (Record params) -> service) where
  serveWith read req res params =
    case runExcept (decodeParams (RLProxy :: RLProxy paramList) params) of
      Left err ->
        sendError res 400 "Bad Request" (renderForeignError (extract err))
      Right builder ->
        let decoded = Builder.build builder {}
        in serveWith (read (Params decoded)) req res params

class DecodeParams r rl | rl -> r where
  decodeParams :: RLProxy rl -> Object String -> F (Builder {} (Record r))

instance decodeParamsNil :: DecodeParams () Nil where
  decodeParams _ _ = pure identity

instance decodeParamsConsString
    :: ( Cons l String r_ r
       , DecodeParams r_ rl_
       , IsSymbol l
       , Lacks l r_
       )
    => DecodeParams r (Cons l String rl_)
  where
    decodeParams _ f = do
      builder <- decodeParams (RLProxy :: RLProxy rl_) f
      let l = reflectSymbol (SProxy :: SProxy l)
      a <- maybe (throwError (pure (ErrorAtProperty l (ForeignError "missing param")))) pure (Object.lookup l f)
      pure (builder >>> Builder.insert (SProxy :: SProxy l) a)
else instance decodeParamsCons
    :: ( Cons l a r_ r
       , DecodeParams r_ rl_
       , IsSymbol l
       , Lacks l r_
       , Decode a
       )
    => DecodeParams r (Cons l a rl_)
  where
    decodeParams _ f = do
      builder <- decodeParams (RLProxy :: RLProxy rl_) f
      let l = reflectSymbol (SProxy :: SProxy l)
      f_ <- maybe (throwError (pure (ErrorAtProperty l (ForeignError "missing param")))) pure (Object.lookup l f)
      a <- mapExcept (lmap (map (ErrorAtProperty l))) (decodeJSON f_)
      pure (builder >>> Builder.insert (SProxy :: SProxy l) a)

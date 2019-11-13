module Crapi.Response where

import Prelude
import Apiary.Media (class MediaCodec, encodeMedia, mediaType)
import Apiary.Status (class ResponseStatus, Status(..), toStatus)
import Control.Monad.Indexed.Qualified as Ix
import Crapi.Handler (Handler(..))
import Data.Foldable (class Foldable, traverse_)
import Data.MediaType (MediaType)
import Data.Newtype (unwrap)
import Data.Symbol (class IsSymbol, SProxy(..))
import Data.Tuple (Tuple, uncurry)
import Effect.Class (class MonadEffect, liftEffect)
import Node.Encoding as Encoding
import Node.HTTP as HTTP
import Node.Stream as Stream
import Prim.Row (class Cons, class Lacks)
import Prim.RowList (class RowToList, kind RowList, Cons, Nil)
import Record.Builder (Builder)
import Record.Builder as Builder
import Type.Data.RowList (RLProxy(..))
import Type.Proxy (Proxy(..))

data StatusLineOpen

data HeadersOpen

data BodyOpen

data ResponseEnded

type Header
  = Tuple String String

writeStatus :: forall res m. MonadEffect m => Status -> Handler res m StatusLineOpen HeadersOpen Unit
writeStatus (Status { code, reason }) =
  Handler \res ->
    liftEffect do
      HTTP.setStatusCode res code
      HTTP.setStatusMessage res reason

writeHeader :: forall res m. MonadEffect m => String -> String -> Handler res m HeadersOpen HeadersOpen Unit
writeHeader name value =
  Handler \res ->
    liftEffect do
      HTTP.setHeader res name value

closeHeaders :: forall res m. Monad m => Handler res m HeadersOpen BodyOpen Unit
closeHeaders = Handler \_ -> pure unit

headers :: forall f res m. Foldable f => MonadEffect m => f Header -> Handler res m HeadersOpen BodyOpen Unit
headers hs = Ix.do
  traverse_ (uncurry writeHeader) hs
  closeHeaders

contentType :: forall res m. MonadEffect m => MediaType -> Handler res m HeadersOpen HeadersOpen Unit
contentType mediaType = writeHeader "Content-Type" (unwrap mediaType)

withResponseStream ::
  forall res m a.
  MonadEffect m =>
  (Stream.Writable () -> m a) ->
  Handler res m BodyOpen ResponseEnded a
withResponseStream f =
  Handler \res -> do
    let
      s = HTTP.responseAsStream res
    a <- f s
    liftEffect do Stream.end s mempty
    pure a

send :: forall res m. MonadEffect m => String -> Handler res m BodyOpen ResponseEnded Unit
send str =
  withResponseStream \stream ->
    void
      $ liftEffect do
          Stream.writeString stream Encoding.UTF8 str mempty

respondWithMedia ::
  forall m rep a res.
  MediaCodec rep a =>
  MonadEffect m =>
  Status ->
  Proxy rep ->
  a ->
  Handler res m StatusLineOpen ResponseEnded Unit
respondWithMedia status rep response = Ix.do
  writeStatus status
  traverse_ contentType (mediaType rep)
  closeHeaders
  send (encodeMedia rep response)

class BuildResponder rep a | rep -> a where
  buildResponder :: Proxy rep -> a

instance buildResponders ::
  ( RowToList responses responseList
  , MonadEffect m
  , BuildResponderRecord responseList m responders
  ) =>
  BuildResponder { | responses } { | responders } where
  buildResponder _ = Builder.build (buildResponderRecord (RLProxy :: _ responseList)) {}

class BuildResponderRecord (responses :: RowList) (m :: Type -> Type) (responders :: #Type) | responses -> m responders where
  buildResponderRecord :: RLProxy responses -> Builder {} { | responders }

instance buildResponderRecordNil :: BuildResponderRecord Nil m () where
  buildResponderRecord _ = identity

instance buildResponderRecordCons ::
  ( IsSymbol status
  , ResponseStatus status
  , MediaCodec responseRep response
  , Cons status responseRep res' res
  , MonadEffect m
  , Lacks status responders'
  , Cons status (response -> Handler res m StatusLineOpen ResponseEnded Unit) responders' responders
  , BuildResponderRecord responseList m responders'
  ) =>
  BuildResponderRecord (Cons status responseRep responseList) m responders where
  buildResponderRecord _ = Builder.insert status responder <<< buildResponderRecord (RLProxy :: _ responseList)
    where
    status = SProxy :: _ status

    responder = respondWithMedia (toStatus status) (Proxy :: _ responseRep)

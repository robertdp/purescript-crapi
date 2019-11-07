module Crapi.Response where

import Prelude
import Control.Monad.Indexed.Qualified as Ix
import Crapi.Handler (Handler(..))
import Crapi.Media (class IsMedia, encodeMedia, mediaType)
import Crapi.Status (Status(..))
import Crapi.Status as Status
import Data.Foldable (class Foldable, traverse_)
import Data.MediaType (MediaType)
import Data.Newtype (unwrap)
import Data.Tuple (Tuple, uncurry)
import Effect.Class (class MonadEffect, liftEffect)
import Node.Encoding as Encoding
import Node.HTTP as HTTP
import Node.Stream as Stream
import Type.Proxy (Proxy(..))

data StatusLineOpen

data HeadersOpen

data BodyOpen

data ResponseEnded

type Header
  = Tuple String String

writeStatus :: forall res m. MonadEffect m => Status -> Handler res m StatusLineOpen HeadersOpen Unit
writeStatus (Status { code, reasonPhrase }) =
  Handler \res ->
    liftEffect do
      HTTP.setStatusCode res code
      HTTP.setStatusMessage res reasonPhrase

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
  forall a res m.
  IsMedia a =>
  MonadEffect m =>
  Status ->
  a ->
  Handler res m StatusLineOpen ResponseEnded Unit
respondWithMedia status response = Ix.do
  writeStatus status
  contentType (mediaType (Proxy :: _ a))
  closeHeaders
  send (encodeMedia response)

respondOK ::
  forall a res m.
  IsMedia a =>
  MonadEffect m =>
  a ->
  Handler ( ok :: a | res ) m StatusLineOpen ResponseEnded Unit
respondOK = respondWithMedia Status.statusOK

respondCreated ::
  forall a res m.
  IsMedia a =>
  MonadEffect m =>
  a ->
  Handler ( created :: a | res ) m StatusLineOpen ResponseEnded Unit
respondCreated = respondWithMedia Status.statusCreated

respondNoContent ::
  forall m res a.
  IsMedia a =>
  MonadEffect m =>
  a ->
  Handler ( noContent :: a | res ) m StatusLineOpen ResponseEnded Unit
respondNoContent = respondWithMedia Status.statusNoContent

respondBadRequest ::
  forall m res a.
  IsMedia a =>
  MonadEffect m =>
  a ->
  Handler ( badRequest :: a | res ) m StatusLineOpen ResponseEnded Unit
respondBadRequest = respondWithMedia Status.statusBadRequest

respondUnauthorized ::
  forall m res a.
  IsMedia a =>
  MonadEffect m =>
  a ->
  Handler ( unauthorized :: a | res ) m StatusLineOpen ResponseEnded Unit
respondUnauthorized = respondWithMedia Status.statusUnauthorized

respondForbidden ::
  forall m res a.
  IsMedia a =>
  MonadEffect m =>
  a ->
  Handler ( forbidden :: a | res ) m StatusLineOpen ResponseEnded Unit
respondForbidden = respondWithMedia Status.statusForbidden

respondNotFound ::
  forall m res a.
  IsMedia a =>
  MonadEffect m =>
  a ->
  Handler ( notFound :: a | res ) m StatusLineOpen ResponseEnded Unit
respondNotFound = respondWithMedia Status.statusNotFound

respondConflict ::
  forall m res a.
  IsMedia a =>
  MonadEffect m =>
  a ->
  Handler ( conflict :: a | res ) m StatusLineOpen ResponseEnded Unit
respondConflict = respondWithMedia Status.statusConflict

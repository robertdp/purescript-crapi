module Crapi.Handler where

import Prelude
import Control.Applicative.Indexed (class IxApplicative, iapply, imap, ipure)
import Control.Apply.Indexed (class IxApply)
import Control.Bind.Indexed (class IxBind, ibind)
import Control.Monad.Indexed (class IxMonad)
import Control.Monad.Indexed.Trans (class IxMonadTrans)
import Data.Functor.Indexed (class IxFunctor)
import Node.HTTP (Response)

newtype Handler (res :: #Type) m from to a
  = Handler (Response -> m a)

runHandler :: forall res m from to a. Handler res m from to a -> Response -> m a
runHandler (Handler f) = f

instance ixFunctorHandler :: Monad m => IxFunctor (Handler res m) where
  imap f a = Handler \res -> map f (runHandler a res)

instance ixApplyHandler :: Monad m => IxApply (Handler res m) where
  iapply f a = Handler \res -> apply (runHandler f res) (runHandler a res)

instance ixApplicativeHandler :: Monad m => IxApplicative (Handler res m) where
  ipure a = Handler \res -> pure a

instance ixBindHandler :: Monad m => IxBind (Handler res m) where
  ibind ma f =
    Handler \res -> do
      a <- runHandler ma res
      case f a of
        Handler k -> k res

instance ixMonadHandler :: Monad m => IxMonad (Handler res m)

instance ixMonadTransHandler :: Monad m => IxMonadTrans (Handler res) where
  ilift ma = Handler \_ -> ma

instance functorHandler :: Monad m => Functor (Handler res m x x) where
  map = imap

instance applyHandler :: Monad m => Apply (Handler res m x x) where
  apply = iapply

instance applicativeHandler :: Monad m => Applicative (Handler res m x x) where
  pure = ipure

instance bindHandler :: Monad m => Bind (Handler res m x x) where
  bind = ibind

instance monadHandler :: Monad m => Monad (Handler res m x x)

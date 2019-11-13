## Module Crapi.Handler

#### `Handler`

``` purescript
newtype Handler (res :: # Type) m from to a
  = Handler (Response -> m a)
```

##### Instances
``` purescript
(Monad m) => IxFunctor (Handler res m)
(Monad m) => IxApply (Handler res m)
(Monad m) => IxApplicative (Handler res m)
(Monad m) => IxBind (Handler res m)
(Monad m) => IxMonad (Handler res m)
(Monad m) => IxMonadTrans (Handler res)
(Monad m) => Functor (Handler res m x x)
(Monad m) => Apply (Handler res m x x)
(Monad m) => Applicative (Handler res m x x)
(Monad m) => Bind (Handler res m x x)
(Monad m) => Monad (Handler res m x x)
```

#### `runHandler`

``` purescript
runHandler :: forall res m from to a. Handler res m from to a -> Response -> m a
```



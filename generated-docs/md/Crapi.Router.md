## Module Crapi.Router

#### `Router`

``` purescript
newtype Router a
```

##### Instances
``` purescript
Functor Router
Apply Router
Applicative Router
Bind Router
Monad Router
MonadEffect Router
```

#### `createServer`

``` purescript
createServer :: forall m. MonadEffect m => (Request -> Response -> Effect Unit) -> Router Unit -> m Server
```

#### `on`

``` purescript
on :: Method -> Path -> (Request -> Response -> PathParams -> Effect Unit) -> Router Unit
```



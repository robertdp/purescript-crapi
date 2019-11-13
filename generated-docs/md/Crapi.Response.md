## Module Crapi.Response

#### `StatusLineOpen`

``` purescript
data StatusLineOpen
```

#### `HeadersOpen`

``` purescript
data HeadersOpen
```

#### `BodyOpen`

``` purescript
data BodyOpen
```

#### `ResponseEnded`

``` purescript
data ResponseEnded
```

#### `Header`

``` purescript
type Header = Tuple String String
```

#### `writeStatus`

``` purescript
writeStatus :: forall res m. MonadEffect m => Status -> Handler res m StatusLineOpen HeadersOpen Unit
```

#### `writeHeader`

``` purescript
writeHeader :: forall res m. MonadEffect m => String -> String -> Handler res m HeadersOpen HeadersOpen Unit
```

#### `closeHeaders`

``` purescript
closeHeaders :: forall res m. Monad m => Handler res m HeadersOpen BodyOpen Unit
```

#### `headers`

``` purescript
headers :: forall f res m. Foldable f => MonadEffect m => f Header -> Handler res m HeadersOpen BodyOpen Unit
```

#### `contentType`

``` purescript
contentType :: forall res m. MonadEffect m => MediaType -> Handler res m HeadersOpen HeadersOpen Unit
```

#### `withResponseStream`

``` purescript
withResponseStream :: forall res m a. MonadEffect m => (Writable () -> m a) -> Handler res m BodyOpen ResponseEnded a
```

#### `send`

``` purescript
send :: forall res m. MonadEffect m => String -> Handler res m BodyOpen ResponseEnded Unit
```

#### `respondWithMedia`

``` purescript
respondWithMedia :: forall a res m. IsMedia a => MonadEffect m => Status -> a -> Handler res m StatusLineOpen ResponseEnded Unit
```

#### `respondOK`

``` purescript
respondOK :: forall a res m. IsMedia a => MonadEffect m => a -> Handler (ok :: a | res) m StatusLineOpen ResponseEnded Unit
```

#### `respondCreated`

``` purescript
respondCreated :: forall a res m. IsMedia a => MonadEffect m => a -> Handler (created :: a | res) m StatusLineOpen ResponseEnded Unit
```

#### `respondNoContent`

``` purescript
respondNoContent :: forall m res a. IsMedia a => MonadEffect m => a -> Handler (noContent :: a | res) m StatusLineOpen ResponseEnded Unit
```

#### `respondBadRequest`

``` purescript
respondBadRequest :: forall m res a. IsMedia a => MonadEffect m => a -> Handler (badRequest :: a | res) m StatusLineOpen ResponseEnded Unit
```

#### `respondUnauthorized`

``` purescript
respondUnauthorized :: forall m res a. IsMedia a => MonadEffect m => a -> Handler (unauthorized :: a | res) m StatusLineOpen ResponseEnded Unit
```

#### `respondForbidden`

``` purescript
respondForbidden :: forall m res a. IsMedia a => MonadEffect m => a -> Handler (forbidden :: a | res) m StatusLineOpen ResponseEnded Unit
```

#### `respondNotFound`

``` purescript
respondNotFound :: forall m res a. IsMedia a => MonadEffect m => a -> Handler (notFound :: a | res) m StatusLineOpen ResponseEnded Unit
```

#### `respondConflict`

``` purescript
respondConflict :: forall m res a. IsMedia a => MonadEffect m => a -> Handler (conflict :: a | res) m StatusLineOpen ResponseEnded Unit
```



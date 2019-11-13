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
respondWithMedia :: forall m rep a res. MediaCodec rep a => MonadEffect m => Status -> Proxy rep -> a -> Handler res m StatusLineOpen ResponseEnded Unit
```

#### `BuildResponder`

``` purescript
class BuildResponder rep a | rep -> a where
  buildResponder :: Proxy rep -> a
```

##### Instances
``` purescript
(RowToList responses responseList, MonadEffect m, BuildResponderRecord responseList m responders) => BuildResponder (Record responses) (Record responders)
```

#### `BuildResponderRecord`

``` purescript
class BuildResponderRecord (responses :: RowList) (m :: Type -> Type) (responders :: # Type) | responses -> m responders where
  buildResponderRecord :: RLProxy responses -> Builder (Record ()) (Record responders)
```

##### Instances
``` purescript
BuildResponderRecord Nil m ()
(IsSymbol status, ResponseStatus status, MediaCodec responseRep response, Cons status responseRep res' res, MonadEffect m, Lacks status responders', Cons status (response -> Handler res m StatusLineOpen ResponseEnded Unit) responders' responders, BuildResponderRecord responseList m responders') => BuildResponderRecord (Cons status responseRep responseList) m responders
```



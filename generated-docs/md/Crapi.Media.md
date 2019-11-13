## Module Crapi.Media

#### `IsMedia`

``` purescript
class IsMedia a  where
  decodeMedia :: String -> Either String a
  encodeMedia :: a -> String
  mediaType :: Proxy a -> MediaType
```

##### Instances
``` purescript
IsMedia Unit
IsMedia String
(ReadForeign a, WriteForeign a) => IsMedia (JSON a)
```

#### `JSON`

``` purescript
newtype JSON a
  = JSON a
```

##### Instances
``` purescript
(ReadForeign a, WriteForeign a) => IsMedia (JSON a)
```



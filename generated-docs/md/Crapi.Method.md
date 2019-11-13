## Module Crapi.Method

#### `IsMethod`

``` purescript
class IsMethod method  where
  reflectMethod :: Proxy method -> String
```

##### Instances
``` purescript
IsMethod GET
IsMethod POST
IsMethod PUT
IsMethod PATCH
IsMethod DELETE
IsMethod OPTIONS
```

#### `GET`

``` purescript
data GET
```

##### Instances
``` purescript
IsMethod GET
```

#### `POST`

``` purescript
data POST
```

##### Instances
``` purescript
IsMethod POST
```

#### `PUT`

``` purescript
data PUT
```

##### Instances
``` purescript
IsMethod PUT
```

#### `PATCH`

``` purescript
data PATCH
```

##### Instances
``` purescript
IsMethod PATCH
```

#### `DELETE`

``` purescript
data DELETE
```

##### Instances
``` purescript
IsMethod DELETE
```

#### `OPTIONS`

``` purescript
data OPTIONS
```

##### Instances
``` purescript
IsMethod OPTIONS
```



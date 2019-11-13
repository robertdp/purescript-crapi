## Module Crapi.Status

#### `Status`

``` purescript
newtype Status
  = Status { code :: Int, reasonPhrase :: String }
```

#### `status`

``` purescript
status :: Int -> String -> Status
```

#### `statusCode`

``` purescript
statusCode :: Status -> Int
```

#### `statusOK`

``` purescript
statusOK :: Status
```

#### `statusCreated`

``` purescript
statusCreated :: Status
```

#### `statusNoContent`

``` purescript
statusNoContent :: Status
```

#### `statusNotModified`

``` purescript
statusNotModified :: Status
```

#### `statusBadRequest`

``` purescript
statusBadRequest :: Status
```

#### `statusUnauthorized`

``` purescript
statusUnauthorized :: Status
```

#### `statusForbidden`

``` purescript
statusForbidden :: Status
```

#### `statusNotFound`

``` purescript
statusNotFound :: Status
```

#### `statusConflict`

``` purescript
statusConflict :: Status
```

#### `statusInternalServerError`

``` purescript
statusInternalServerError :: Status
```

#### `statusMaintenanceInProgress`

``` purescript
statusMaintenanceInProgress :: Status
```



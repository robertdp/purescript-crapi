## Module Crapi.Request

#### `PathParams`

``` purescript
type PathParams = Object String
```

#### `QueryParams`

``` purescript
type QueryParams = Object Foreign
```

#### `DecodePathParams`

``` purescript
class DecodePathParams params  where
  decodePathParams :: PathParams -> F params
```

##### Instances
``` purescript
(RowToList r rl, DecodePathParamRecord r rl) => DecodePathParams (Record r)
```

#### `DecodePathParamRecord`

``` purescript
class DecodePathParamRecord r rl | rl -> r where
  decodePathParamRecord :: RLProxy rl -> PathParams -> F (Builder (Record ()) (Record r))
```

##### Instances
``` purescript
DecodePathParamRecord () Nil
(Cons l String r_ r, DecodePathParamRecord r_ rl_, IsSymbol l, Lacks l r_) => DecodePathParamRecord r (Cons l String rl_)
(Cons l a r_ r, DecodePathParamRecord r_ rl_, IsSymbol l, Lacks l r_, ReadForeign a) => DecodePathParamRecord r (Cons l a rl_)
```

#### `DecodeQueryParams`

``` purescript
class DecodeQueryParams params  where
  decodeQueryParams :: QueryParams -> F params
```

##### Instances
``` purescript
(RowToList r rl, DecodeQueryParamRecord r rl) => DecodeQueryParams (Record r)
```

#### `DecodeQueryParamRecord`

``` purescript
class DecodeQueryParamRecord r rl | rl -> r where
  decodeQueryParamRecord :: RLProxy rl -> QueryParams -> F (Builder (Record ()) (Record r))
```

##### Instances
``` purescript
DecodeQueryParamRecord () Nil
(IsSymbol name, UrlParam value, Cons name (Array value) params' params, Lacks name params', DecodeQueryParamRecord params' paramList) => DecodeQueryParamRecord params (Cons name (Array value) paramList)
(IsSymbol name, UrlParam value, Cons name (Maybe value) params' params, Lacks name params', DecodeQueryParamRecord params' paramList) => DecodeQueryParamRecord params (Cons name (Maybe value) paramList)
```



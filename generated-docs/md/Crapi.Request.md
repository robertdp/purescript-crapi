## Module Crapi.Request

#### `RouterParams`

``` purescript
type RouterParams = Object String
```

#### `DecodeParams`

``` purescript
class DecodeParams params  where
  decodeParams :: RouterParams -> Either String params
```

##### Instances
``` purescript
DecodeParams Unit
(RowToList r rl, DecodeParamRecord r rl) => DecodeParams (Record r)
```

#### `DecodeParamRecord`

``` purescript
class DecodeParamRecord r rl | rl -> r where
  decodeParamRecord :: RLProxy rl -> RouterParams -> E (Builder (Record ()) (Record r))
```

##### Instances
``` purescript
DecodeParamRecord () Nil
(Cons l String r_ r, DecodeParamRecord r_ rl_, IsSymbol l, Lacks l r_) => DecodeParamRecord r (Cons l String rl_)
(Cons l a r_ r, DecodeParamRecord r_ rl_, IsSymbol l, Lacks l r_, ReadForeign a) => DecodeParamRecord r (Cons l a rl_)
```



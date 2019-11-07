module Crapi.Request where

import Prelude
import Control.Comonad (extract)
import Control.Monad.Error.Class (throwError)
import Data.Bifunctor (bimap, lmap)
import Data.Either (Either)
import Data.Maybe (maybe)
import Data.Symbol (class IsSymbol, SProxy(..), reflectSymbol)
import Foreign (ForeignError(..), renderForeignError)
import Foreign.Object (Object)
import Foreign.Object as Object
import Prim.Row (class Cons, class Lacks)
import Prim.RowList (class RowToList, Nil, Cons)
import Record.Builder (Builder)
import Record.Builder as Builder
import Simple.JSON (class ReadForeign, E, readJSON)
import Type.Data.RowList (RLProxy(..))

type RouterParams
  = Object String

class DecodeParams params where
  decodeParams :: RouterParams -> Either String params

instance decodeParamsUnit :: DecodeParams Unit where
  decodeParams _ = pure unit

instance decodeParamsRecord :: (RowToList r rl, DecodeParamRecord r rl) => DecodeParams (Record r) where
  decodeParams =
    decodeParamRecord (RLProxy :: _ rl)
      >>> bimap (extract >>> renderForeignError) (flip Builder.build {})

class DecodeParamRecord r rl | rl -> r where
  decodeParamRecord :: RLProxy rl -> RouterParams -> E (Builder {} (Record r))

instance decodeParamRecordNil :: DecodeParamRecord () Nil where
  decodeParamRecord _ _ = pure identity

instance decodeParamRecordConsString ::
  ( Cons l String r_ r
  , DecodeParamRecord r_ rl_
  , IsSymbol l
  , Lacks l r_
  ) =>
  DecodeParamRecord r (Cons l String rl_) where
  decodeParamRecord _ f = do
    builder <- decodeParamRecord (RLProxy :: RLProxy rl_) f
    let
      l = reflectSymbol (SProxy :: SProxy l)
    a <- maybe (throwError (pure (ErrorAtProperty l (ForeignError "missing param")))) pure (Object.lookup l f)
    pure (builder >>> Builder.insert (SProxy :: SProxy l) a)
else instance decodeParamRecordCons ::
  ( Cons l a r_ r
  , DecodeParamRecord r_ rl_
  , IsSymbol l
  , Lacks l r_
  , ReadForeign a
  ) =>
  DecodeParamRecord r (Cons l a rl_) where
  decodeParamRecord _ f = do
    builder <- decodeParamRecord (RLProxy :: RLProxy rl_) f
    let
      l = reflectSymbol (SProxy :: SProxy l)
    f_ <- maybe (throwError (pure (ErrorAtProperty l (ForeignError "missing param")))) pure (Object.lookup l f)
    a <- lmap (map (ErrorAtProperty l)) (readJSON f_)
    pure (builder >>> Builder.insert (SProxy :: SProxy l) a)

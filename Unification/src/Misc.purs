module Misc (module Misc,
             module Data.Maybe,
             module MyList,
             module MyTuple,
             module Data.Traversable,
             module Prelude,
             module Data.Pair) where

import Prelude
import Data.String.CodeUnits (fromCharArray, singleton)
import Control.Semigroupoid
import Data.Maybe
import Data.List
import Data.Tuple (Tuple(..))
import Data.Tuple (Tuple(..)) as MyTuple
import Data.Pair
import Data.Traversable (class Foldable, foldMap, or, and, all, any)
import Data.List hiding (foldM)
import Data.List hiding (foldM) as MyList

eitherOf :: Pair Boolean -> Boolean
eitherOf = or

bothOf :: Pair Boolean -> Boolean
bothOf = and

popList :: forall a. List a -> Maybe (Tuple a (List a))
popList Nil = Nothing
popList (Cons a rest) = Just (Tuple a rest)

fromChars :: forall f. Foldable f => f Char -> String
fromChars = foldMap singleton

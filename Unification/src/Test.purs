module Test where

import Prelude (Unit)
import Data.Pair
import Data.Tuple
import Data.List

-- | # Level one heading
-- | This is some text directly under the heading.

-- | This line is meant to document `Silly`, but it will be merged
-- | with the previous line, while `Silly` will be lifted over the
-- | heading. This is a little baffling in my opinion.
type Silly = Pair Unit

-- | This content will be attached to `Sillier`, which makes sense.
type Sillier = Tuple Unit Unit

-- | This content will be attached to `Silliest`, which makes sense.
type Silliest = List Unit

-- | ## Level two heading
-- | This is some text directly under the level two heading.

-- | Just like before, this line is meant to document `bignum`, but
-- | instead it will be merged with the previous line, while `bignum`
-- | will be lifted over the heading.
bignum :: Int
bignum = 3

-- | This content will be attached to `biggernum`, which makes sense.
biggernum :: Int
biggernum = 4

-- | This content will be attached to `biggestnum`, which makes sense.
biggestnum :: Int
biggestnum = 5

module Main where

import Prelude (void, discard, Unit, bind, map, unit, ($), (<>), (=<<))
import Data.List (List(..), (:))
import Data.Pair (Pair(..))
import Data.Maybe (Maybe(..))
import Expr (Expr(..), ex1, ex2, ex3)
import Effect (Effect)
import Effect.Exception (throw)
import React.Basic.DOM as R
import React.Basic.DOM.Client (createRoot, renderRoot)
import Unification
import Web.DOM.NonElementParentNode (getElementById)
import Web.HTML (window)
import Web.HTML.HTMLDocument (toNonElementParentNode)
import Web.HTML.Window (document)

-- F (x, G (x, y))
ex11 :: Expr
ex11 = Node "F" (Var "x" : (Node "G" (Var "x" : Var "y" : Nil)) : Nil)

-- F (G (F (y, v, u)), G (G (F (y, v, u)), H (c)))
ex22 :: Expr
ex22 = Node "F" (ex33 : Node "G" (ex33 : Node "H" (Var "c" : Nil) : Nil) : Nil)

ex33 :: Expr
ex33 = Node "G" (Node "F" (Var "y" : Var "v" : Var "u" : Nil) : Nil)

state :: MGU_State
state = { assignments: Nil,
          equations: (Pair ex1 ex1) : (Pair ex3 ex2) : (Pair ex2 ex1) : Nil}

state2 :: Equations
state2 = (Pair ex11 ex22 : Nil)

foreign import hello :: Effect Unit

main :: Effect Unit
main = do
  container <- getElementById "unification-root" =<< (map toNonElementParentNode $ document =<< window)
  void $ hello
  case container of
    Nothing -> throw "Container element not found."
    Just c  -> do
      reactroot <- createRoot c
      renderRoot reactroot (renderUnificationDemo unit)


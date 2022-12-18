module RewriteLive.Types where

import Prelude
import Data.List (List, elem)
import RewriteLive.Expr (Expr, class Substitute, substitute)

-- | An `Equation` is a pair of expressions.
newtype Equation = Equation { left :: Expr, right :: Expr }

-- | An `Equations` is a list of `Equation`s.
type Equations = List Equation

-- | Map a substitution over an `Equation`.
instance Substitute Equation where
  substitute s eqn =
    case eqn of
      Equation {left, right} ->
        Equation { left: substitute s left
                 , right: substitute s right }

-- | An `Axiom` is a named `Equation`
newtype Axiom = Axiom { name :: String, statement :: Equation }

-- | An `Axioms` is a list of `Axiom`s
type Axioms = List Axiom

data RwDir = RwLeftToRight | RwRightToLeft

derive instance eqRwDir :: Eq RwDir

data LorR = OnLeft | OnRight

instance Show LorR where
  show OnLeft = "left"
  show OnRight = "right"

data RewriteCmd = RewriteCmd String RwDir LorR Int

nameIsFresh :: String -> Axioms -> Boolean
nameIsFresh name axioms =
  not (name `elem` (map (\(Axiom ax) -> ax.name) axioms))

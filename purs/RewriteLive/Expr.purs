module RewriteLive.Expr where

import Prelude
import Data.List (List(..), (:), length, intercalate)
import Data.Foldable (any)
import Data.Tuple (Tuple(..))
import React.Basic.Classic (JSX)
import React.Basic.DOM as R

-- | # Abstract syntax for expressions

data Expr
  = Var String
  | Node String (List Expr)

derive instance eqExpr :: Eq Expr

-- | Expression where each node is annotated by some 'a' value
data AnnExpr a
  = AnnVar String
  | AnnNode String (List (AnnExpr a)) a

-- | ## Substitution and occurrence in expressions
type Sub = String -> Expr

-- | Typeclass for types that support a single-variable substitution
class Substitute t where
  substitute  :: Sub -> t -> t

instance (Substitute a, Functor f) => Substitute (f a) where
  substitute s fa = substitute s <$> fa

-- | Decorated map over a list, starting from a supplied value
fmapdList :: forall a b. Int -> (Int -> a -> b) -> List a -> List b
fmapdList _ _ Nil = Nil
fmapdList start f (Cons a rest) = Cons (f start a) (fmapdList (start + 1) f rest)

-- | Assign a unique number to each expression
decorateNodes_rec :: Int -> Expr -> Tuple Int (AnnExpr Int)
decorateNodes_rec start (Var v) = Tuple start (AnnVar v)
decorateNodes_rec start (Node fn args) =
  let Tuple out args' = go (start + 1) args
  in  Tuple out (AnnNode fn args' start)
  where
    go :: Int -> List Expr -> Tuple Int (List (AnnExpr Int))
    go n args = case args of
      Nil -> Tuple n Nil
      Cons arg0 rest ->
            let Tuple next arg0'  = decorateNodes_rec n arg0
                Tuple next' args' = go next rest
            in Tuple next' (arg0' : args')

-- | Assign a unique number to each expression
extractNodes :: forall a. AnnExpr a -> Expr
extractNodes (AnnVar v) = Var v
extractNodes (AnnNode fn args _) = Node fn (extractNodes <$> args)

-- | Assign a unique number to each expression
decorateNodes :: Expr -> AnnExpr Int
decorateNodes expr =
  let Tuple _out annexpr = decorateNodes_rec 0 expr
  in  annexpr

undecorateNodes :: forall a. AnnExpr a -> Expr
undecorateNodes (AnnVar v) = Var v
undecorateNodes (AnnNode fn args _) = Node fn (undecorateNodes <$> args)

-- | Make a singleton `Sub`
mkSub :: String -> Expr -> Sub
mkSub v e = \str ->
  if str == v then e
  else Var str

substitute1 :: forall t. (Substitute t) => String -> Expr -> t -> t
substitute1 v e = substitute (mkSub v e)

instance Substitute Expr where
  substitute sub e = case e of
    Var v -> sub v
    Node f args -> Node f (substitute sub <$> args)

occurs_in :: String -> Expr -> Boolean
occurs_in v (Node _ rest) = any (occurs_in v) rest
occurs_in v (Var x) = v == x

-- | ## Displaying expressions

-- | Convert an `Expr` into a `String` in the simplest way possible
instance showExpr :: Show Expr where
  show expr = case expr of
    Node fn args ->
      if length args > 0
      then fn <> " (" <> (intercalate ", " (show <$> args)) <> ")"
      else fn
    Var v -> v

-- | Render an `Expr`
renderExpr :: Expr -> JSX
renderExpr expr =
  R.span {className: "expr-inline",
         children: [go expr]}
  where
    go (Var x) =
      R.span { className: "var",
               children: [R.text x] }
    go (Node f args) =
      R.span { className: "node",
               children: [ R.text $ f <>
                           (if length args > 0
                            then "(" <> (intercalate ", " (show <$> args)) <> ")"
                            else "") ]}

-- | # Example values

-- | F (x, y)
ex0 :: Expr
ex0 = Node "F" (Var "x" : Var "y" : Nil)

-- | F (y, x)
ex1 :: Expr
ex1 = Node "F" (Var "y" : Var "x" : Nil)

-- | F (x, F (y, v))
ex2 :: Expr
ex2 = Node "F" (Var "x" : Node "F" (Var "y" : Var "v" : Nil) : Nil)

-- | F (G(x), F(C, y))
ex3 :: Expr
ex3 = Node "F" (Node "G" (Var "x" : Nil) : Node "F" (Node "C" Nil : Var "y" : Nil) : Nil)

module Expr where

import Misc
import Control.Lazy (fix)
import Control.Alternative ((<|>))
import Data.Either (Either(..))
import Data.Identity(Identity(..))
import Data.String.CodeUnits (fromCharArray, singleton)
import Parsing (Parser)
import Parsing as P
import Parsing.Combinators as PC
import Parsing.String as PS
import Parsing.String.Basic as PS
import React.Basic.Classic (JSX)
import React.Basic.DOM as R

-- | # Abstract syntax for expressions

data Expr
  = Var String
  | Node String (List Expr)

data AnnExpr a
  = AVar String
  | ANode String (List (AnnExpr a)) a

lweird :: forall a b. (Int -> a -> b) -> Int -> List a -> List b
lweird f n Nil = Nil
lweird f n (Cons x rest) = Cons (f n x) (lweird f (n + 1) rest)

weird :: Int -> Expr -> AnnExpr Int
weird n (Var v) = AVar v
weird n (Node fn args) = ANode fn (lweird weird (n + 1) args) n

derive instance eqExpr :: Eq Expr

-- | ## Example values

ex1 :: Expr
ex1 = Node "f" (Var "x" : Var "y" : Nil)

ex2 :: Expr
ex2 = Node "g" (Node "f" (Var "x" : Var "y" : Var "z" : Nil) : Nil)

ex3 :: Expr
ex3 = Node "g" (Node "h" (Var "w" : (Node "f" (Var "x" : Var "y" : Nil)) : Nil) : Nil)

-- | ## Substitution and occurrence in expressions

type Sub = String -> Expr

expr_subFn :: Sub -> Expr -> Expr
expr_subFn sub e = case e of
  Var v -> sub v
  Node f args -> Node f (expr_subFn sub <$> args)

sub_one :: String -> Expr -> Sub
sub_one v e = \str ->
  if str == v then e
  else Var str

expr_sub :: String -> Expr -> Expr -> Expr
expr_sub v e_v e = expr_subFn (sub_one v e_v) e

occurs_in :: String -> Expr -> Boolean
occurs_in v (Node _ rest) = any (occurs_in v) rest
occurs_in v (Var x) = v == x

-- | ## Displaying expressions

instance showExpr :: Show Expr where
  show expr = case expr of
    Node fn args ->
      if length args > 0
      then fn <> " (" <> (intercalate ", " (show <$> args)) <> ")"
      else fn
    Var v -> v

toElementExpr :: Expr -> JSX
toElementExpr expr =
  let mkFn fn = R.div {className: "fn", children : [ R.text fn ] }
      mkNode fn args =
        R.div {className: "node",
               children: [mkFn fn, R.text "(", foldMap toElementExpr args, R.text ")"]}
      mkVar v = R.div {className: "var", children: [R.text v]}
  in case expr of
    Node fn args -> mkNode fn args
    Var v -> mkVar v

exprToJSX :: Expr -> JSX
exprToJSX expr =
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

-- | ## Parsing expressions from strings

-- | Attach a `Char` to the front of a `String`
cons :: Char -> String -> String
cons = append <<< singleton

-- | Read as many consecutive alphanumeric characters as possible
token :: Parser String String
token = fromChars <$> (PC.many PS.alphaNum)

-- | Parse the parenthesized arguments to a function call. The first
-- argument is a parser for expressions, which we take as an argument
-- in the open recursion style.
parseArgsWith :: Parser String Expr ->
                 Parser String (List Expr)
parseArgsWith pExpr = do
  void $ PS.char '('
  PS.skipSpaces
  args <- PC.sepBy expr comma
  void $ PS.char ')'
  pure args
    where
      expr = do
        PS.skipSpaces
        e <- pExpr
        PS.skipSpaces
        pure e
      comma = do void $ PS.char ','

-- | Parse a token that begins with a lowercase character.
parseVariable :: Parser String Expr
parseVariable = do
  c <- PS.lower
  rest <- token
  pure (Var $ c `cons` rest)

-- | Parse a function call
parseFunctionWith :: Parser String Expr ->
                     Parser String Expr
parseFunctionWith pExpr = do
  fn <- parseHeadConstant
  PS.skipSpaces
  args <- (PC.option Nil (parseArgsWith pExpr))
  PS.skipSpaces
  pure (Node fn args)

-- | Parse a token that begins with an uppercase character.
parseHeadConstant :: Parser String String
parseHeadConstant = do
  c <- PS.upper
  rest <- token
  pure (c `cons` rest)

-- | Parse an arbitrary expression
parseExpr :: Parser String Expr
parseExpr = fix (\self ->
  parseVariable <|> parseFunctionWith self)

type ParserOutput = Record ( parserLeftover :: String,
                             parserResult :: Either P.ParseError Expr)

parseExpression :: String -> ParserOutput
parseExpression input =
  let Identity (Tuple out st) = P.runParserT' (P.ParseState input P.initialPos false) parseExpr
  in { parserResult: out,
       parserLeftover: case st of P.ParseState rest _ _ -> rest }

tryParseExpression :: String -> Maybe Expr
tryParseExpression input =
  let {parserResult} = parseExpression input
  in case parserResult of
       Left _ -> Nothing
       Right expr -> Just expr

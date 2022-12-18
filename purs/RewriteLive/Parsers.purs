module RewriteLive.Parsers
       ( parseExpr
       , parseExpression
       , tryParseExpression
       , parseAxiomName
       , parseAxiomStmt
       , runParserState
       , fromChars
       ) where

import Prelude
import Control.Alternative ((<|>))
import Control.Lazy (fix)
import Data.Either (Either(..))
import Data.Identity (Identity(..))
import Data.List (List(..))
import Data.Maybe (Maybe(..))
import Data.String hiding (singleton) as Str
import Data.String.CodeUnits (singleton)
import Data.Traversable (class Foldable, foldMap)
import Data.Tuple (Tuple(..))
import Parsing (Parser)
import Parsing as P
import Parsing.Combinators as PC
import Parsing.String (char, eof)
import Parsing.String.Basic as PS
import RewriteLive.Expr (Expr(..))
import RewriteLive.Types (Equation(..))

-- | Fold a collection of `Char`s into a `String`
fromChars :: forall f. Foldable f => f Char -> String
fromChars = foldMap singleton

-- | A more detailed function than the one exported by `purescript-parsing`
runParserState :: forall s a. s -> Parser s a -> Tuple (Either P.ParseError a) s
runParserState s0 parser =
  let Identity (Tuple eout (P.ParseState rest _pos _)) = P.runParserT' initialState parser
  in Tuple eout rest
  where
    initialState :: P.ParseState s
    initialState = P.ParseState s0 P.initialPos false

eatAll :: forall m a. P.ParserT String m a -> P.ParserT String m a
eatAll parser = do
  out <- parser
  _ <- eof
  pure out

-- | Attempt to parse a `String` using some `Parser`. To get error
-- | information, see `runParser`.
tryParser :: forall a. P.Parser String a -> String -> Maybe a
tryParser parser input =
  case P.runParser input parser of
       Left _ -> Nothing
       Right a -> Just a

-- | Attempt to parse a `String` using some `Parser`. Fails if not all the input is consumed.
tryParserAll :: forall a. P.Parser String a -> String -> Maybe a
tryParserAll parser input = tryParser (eatAll parser) input

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
  void $ char '('
  PS.skipSpaces
  args <- PC.sepBy expr comma
  void $ char ')'
  pure args
    where
      expr = do
        PS.skipSpaces
        e <- pExpr
        PS.skipSpaces
        pure e
      comma = do void $ char ','

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

-- | `Parser` for converting `String`s to `Expr`s
parseExpr :: Parser String Expr
parseExpr = fix (\self ->
  parseVariable <|> parseFunctionWith self)

-- | Attempt to parse a `String` into an `Expr`.
parseExpression :: String -> Either P.ParseError Expr
parseExpression str = P.runParser str (eatAll parseExpr)

-- | Attempt to parse a `String` into an `Expr`. To get error
-- | information, see `parseExpression`
tryParseExpression :: String -> Maybe Expr
tryParseExpression = fromEither <<< parseExpression

-- | Attempt to parse a `String` into an axiom name.
parseAxiomName :: String -> Either P.ParseError String
parseAxiomName str = P.runParser str (eatAll parser)
  where
    parser :: P.Parser String String
    parser = fromChars <$> (PC.many (PS.alphaNum <|> char '_'))

-- | Attempt to parse a `String` into an `Equation`.
parseAxiomStmt :: String -> Either P.ParseError Equation
parseAxiomStmt str = P.runParser str (eatAll eqnParser)
  where
    eqnParser :: P.Parser String Equation
    eqnParser = do
      e1 <- parseExpr
      PS.skipSpaces
      void $ char '='
      PS.skipSpaces
      e2 <- parseExpr
      pure (Equation {left: e1, right: e2})

fromEither :: forall a b. Either a b -> Maybe b
fromEither (Left _) = Nothing
fromEither (Right b) = Just b

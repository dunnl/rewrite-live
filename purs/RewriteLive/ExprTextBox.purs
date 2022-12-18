module RewriteLive.ExprTextBox where


{-
import Prelude
import Data.Either (Either(..))
import Data.Maybe (Maybe, fromMaybe)
import React.Basic.DOM as R
import React.Basic.Classic (Component, JSX, createComponent, makeStateless)
import React.Basic.Events (EventHandler)
import RewriteLive.DOM (simpleDiv)
import RewriteLive.Expr (Expr)
import RewriteLive.Parsers (parseExpression)

data TextBoxState a =
    BadInput String
  | GoodInput a
  | NoInput

-- | Display error text about the state of the input
errorText :: TextBoxState Expr -> JSX
errorText state =
  case state of
    NoInput -> mempty
    BadInput err -> R.text $ "Error: " <> err
    GoodInput res -> R.text $ "Parsed input as: " <> show res

-- | Given a string, compute the associated state of the textbox
readValueExpr :: String -> TextBoxState Expr
readValueExpr input =
  if input == ""
  then NoInput
  else case parseExpression input of
    Left err ->
      BadInput $ "Parse error: " <> show err
    Right expr ->
      GoodInput expr

-- | Specification for a simple textbox
type SimpleTextBoxSpec a =
  Record ( label :: Maybe String
         , value :: String
         , placeholder :: String
         , handleValue :: String -> TextBoxState a
         , onChange :: EventHandler
         )

-- | A component for entering a expression
exprTextBox :: Component (SimpleTextBoxSpec Expr)
exprTextBox = createComponent "exprTextBox"

-- | Render a textbox for parsing an `Expr`
renderExprTextBox :: SimpleTextBoxSpec Expr -> JSX
renderExprTextBox = makeStateless exprTextBox render
  where
    render spec =
      simpleDiv "field exprTextBox"
      [ R.label { className: "label"
                , children: [R.text (fromMaybe "" spec.label)]
                }
      , simpleDiv "control has-icons-right"
        [ R.input { className: ("input " <> classes)
                  , type: "text"
                  , placeholder: spec.placeholder
                  , onChange: spec.onChange
                  , value: spec.value
                  }
        , icon
        ]
      , errorText state
      ]
      where
        state = spec.handleValue spec.value
        classes = case state of
          BadInput _ -> "is-danger"
          GoodInput _ -> "is-success"
          _ -> ""
        icon = case state of
          NoInput -> mempty
          BadInput _ ->
            R.span { className: "icon is-small is-right"
                   , children: [R.i {className: "fas fa-triangle-exclamation"}]}
          GoodInput _ ->
            R.span { className: "icon is-small is-right"
                   , children: [R.i {className: "fas fa-check"}]}
-}

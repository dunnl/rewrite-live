module ExprTextBox where

import Misc
import Expr (Expr, parseExpression)
import Data.Either (Either(..))
import React.Basic.DOM as R
import React.Basic.Classic (Component, JSX, createComponent, make)
import React.Basic.Events (EventHandler)

data TextBoxState a =
    BadInput String
  | GoodInput a
  | NoInput

type TextBoxProp =
  Record (label :: String, value :: String, onChange :: EventHandler)

-- | A component for entering a expression
exprTextBoxC :: Component TextBoxProp
exprTextBoxC = createComponent "exprTextBox"

-- | Given a string, compute the associated state of the textbox
readInput :: String -> TextBoxState Expr
readInput input =
  if input == ""
  then NoInput
  else let {parserLeftover, parserResult} = parseExpression input
       in case parserResult of
         Left err ->
           BadInput $ "parse error: " <> show err
         Right expr ->
           if parserLeftover /= ""
           then BadInput $ "The parser left this input unprocessed: " <> parserLeftover
           else GoodInput expr

-- | A component
exprTextBox :: TextBoxProp -> JSX
exprTextBox = make exprTextBoxC { initialState, render }
  where
    initialState = {}
    render self =
      R.div { className: "field exprTextBox",
              children:
              [ R.label { className: "label"
                        , children: [R.text self.props.label]
                        }
              , R.div { className: "control has-icons-right"
                      , children: [ R.input { className: ("input " <> classes)
                                            , type: "text"
                                            , placeholder: "Text input"
                                            , onChange: self.props.onChange
                                            , value: self.props.value
                                            }
                                   , icon
                                   ]
                      }
              , maybe_error
              ]
            }
      where
        output = readInput self.props.value
        classes = case output of
          BadInput _ -> "is-danger"
          GoodInput _ -> "is-success"
          _ -> ""
        icon = case output of
          NoInput -> mempty
          BadInput _ ->
            R.span { className: "icon is-small is-right"
                   , children: [R.i {className: "fas fa-triangle-exclamation"}]}
          GoodInput _ ->
            R.span { className: "icon is-small is-right"
                   , children: [R.i {className: "fas fa-check"}]}
        maybe_error = case output of
          NoInput -> mempty
          BadInput err -> R.text err
          GoodInput res -> R.text $ "Parsed input as: " <> show res


module RewriteLive.DOM where

import Prelude
import Data.String as Str
import React.Basic.Classic (JSX)
import React.Basic.DOM as DOM
import React.Basic.Events (EventHandler)
import React.Icons (icon_)
import React.Icons.Fa (faCheck, faExclamationTriangle)

-- | Concatenate two class attributes. This will add a separating
-- | space if needed.
appendClasses :: String -> String -> String
appendClasses orig rest =
  (if Str.length rest > 0
   then orig <> " " <> rest
   else orig)

-- | Render some content as a `div`
simpleDiv :: String -> Array JSX -> JSX
simpleDiv classes children =
    DOM.div { className: classes
            , children
            }

-- | Render some content as a `div .container`
simpleContainer :: String -> Array JSX -> JSX
simpleContainer classes children =
    DOM.div { className: appendClasses "container" classes
            , children
            }

-- | Render some content as a `section`
simpleSection :: String -> Array JSX -> JSX
simpleSection classes children =
    DOM.section { className: appendClasses "section" classes
                , children
                }

-- | Render some content as a `div .column`.
simpleColumn :: String -> Array JSX -> JSX
simpleColumn classes children =
  simpleDiv (appendClasses "column" classes) children

-- | Display an array of `div .column` values inside a single `div .columns`.
simpleColumns :: String -> Array JSX -> JSX
simpleColumns classes children =
    simpleDiv (appendClasses "columns" classes) children

para :: String -> String -> JSX
para classes msg =
  DOM.p { className: classes
        , children: [DOM.text msg] }

-- | State of an HTML textbox input
data TextBoxState e a =
    BoxGood a
  | BoxError e
  | BoxEmpty

-- | Specification for a simple textbox
type TextBoxSpec e a =
  Record ( value :: String
         , placeholder :: String
         , state :: TextBoxState e a
         , onChange :: EventHandler
         )

-- | Render a textbox
renderTextBox :: forall e a. TextBoxSpec e a -> JSX
renderTextBox spec =
  simpleDiv "control has-icons-right"
        [ DOM.input { className: ("input " <> classes)
                    , type: "text"
                    , placeholder: spec.placeholder
                    , onChange: spec.onChange
                    , value: spec.value
                    }
        , icon
        ]
  where
    classes = case spec.state of
      BoxGood  _ -> "is-success"
      BoxError _ -> "is-danger"
      BoxEmpty   -> ""
    icon = case spec.state of
      BoxGood _ ->
        DOM.span { className: "icon is-small is-right"
                 , children: [icon_ faCheck]
                 }
      BoxError _ ->
        DOM.span { className: "icon is-small is-right"
                 , children: [icon_ faExclamationTriangle]
                 }
      BoxEmpty -> mempty

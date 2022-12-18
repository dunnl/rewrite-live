module RewriteLive.Goal
       ( renderGoalArea
       , GoalState (..)
       )
       where

import Prelude
import Data.Array (fromFoldable, intersperse)
import Data.List (length)
import Data.Either (Either(..))
import Data.Maybe (Maybe(..), isNothing)
import Data.Tuple (Tuple (..))
import Effect (Effect)
import Parsing as P
import React.Basic.Classic (Component, JSX, Self, createComponent, make)
import React.Basic.Events (EventHandler, handler_)
import React.Basic.DOM as DOM
import React.Basic.DOM.Events (capture_)
import RewriteLive.DOM (simpleDiv, simpleColumn, para, TextBoxState(..), renderTextBox, appendClasses)
import RewriteLive.Events (capturingTargetValue)
import RewriteLive.Expr (Expr(..), AnnExpr(..))
import RewriteLive.Parsers (parseExpression)
import RewriteLive.Types (Equation(..), LorR(..))
import Foreign.Object as FO

-- | State of the goal
data GoalState =
    GoalEmpty
  | GoalPlain Expr Expr
  | GoalDec (AnnExpr (Tuple Int (Maybe Expr)))
            (AnnExpr (Tuple Int (Maybe Expr)))

-- | Passed-down props from on high
type GoalProps = { goalState :: GoalState
                 , handleClick :: LorR -> Int -> Expr -> Effect Unit
                 , handleSubmit :: Equation -> Effect Unit
                 , runDemo :: Effect Unit
                 }

-- | Internal state of the goal form
type GoalFormState =
  { goalLeft :: String
  , goalRight :: String
  , leftState :: TextBoxState P.ParseError Expr
  , rightState :: TextBoxState P.ParseError Expr
  }

initialGoalFormState :: GoalFormState
initialGoalFormState =
  { goalLeft: ""
  , goalRight: ""
  , leftState: BoxEmpty
  , rightState: BoxEmpty
  }

-- | Render a plain goal expression in a `span .expr-hi`
renderGoalExpr :: Expr -> JSX
renderGoalExpr (Var x) = DOM.text x
renderGoalExpr (Node fn args) =
      DOM.span { className: "expr-hi"
               , children:
                 [ DOM.text fn ] <>
                 if length args == 0 then []
                 else [DOM.text " ("]
                      <> intersperse (DOM.text ", ") (fromFoldable $ map renderGoalExpr args)
                      <> [DOM.text ")"]
               }
-- | Render a decorated goal expression in a `span .expr-hi`
renderGoalAnnExpr :: (Int -> Expr -> Effect Unit)
                     -> AnnExpr (Tuple Int (Maybe Expr))
                     -> JSX
renderGoalAnnExpr _ (AnnVar x) = DOM.text x
renderGoalAnnExpr handleClick_ (AnnNode fn args (Tuple uid replacement)) =
      DOM.span { className: appendClasses "expr-hi" color
               , children:  [ DOM.text fn ] <> renderArgs
               , onClick: handleClick
               }
  where
    color =
      case replacement of
        Nothing -> ""
        Just _ -> uidToColorClass uid
    renderArgs =
      if length args == 0 then []
      else [DOM.text " ("]
           <> intersperse (DOM.text ", ") (fromFoldable $ map (renderGoalAnnExpr handleClick_) args)
           <> [DOM.text ")"]
    handleClick =
      case replacement of
        -- The clicked `span` is not a match, do nothing.
        -- Use `handler_` instead of `capture_` so outer spans can catch the event
        Nothing -> handler_ $ pure unit
        -- The clicked `span` is a match and should be replaced with `new`
        Just new -> capture_ (handleClick_ uid new)

uidToColorClass :: Int -> String
uidToColorClass i =
  go remainder
  where
    remainder = i `mod` 6
    go 0 = "c1"
    go 1 = "c2"
    go 2 = "c3"
    go 3 = "c3"
    go 4 = "c4"
    go 5 = "c5"
    go 6 = "c6"
    go _ = "cxxx"

renderGoalState :: (LorR -> Int -> Expr -> Effect Unit) -> GoalState -> JSX
renderGoalState _ (GoalPlain left right) =
  simpleDiv "expr-goal"
  [ renderGoalExpr left
  , DOM.span { className: "expr-hi"
             , children: [DOM.text "="] }
  , renderGoalExpr right
  ]
renderGoalState handleClick (GoalDec left right) =
  simpleDiv "expr-goal"
  [ renderGoalAnnExpr (handleClick OnLeft) left
  , DOM.span { className: "expr-hi"
             , children: [DOM.text "="] }
  , renderGoalAnnExpr (handleClick OnRight) right
  ]
renderGoalState _ GoalEmpty = mempty
  {-
  simpleDiv "expr-no-goal"
  [ DOM.text "No goal entered." ]
-}

foreign import killnotice :: Effect Unit

-- | ## Rendering the axiom area
goalArea :: Component GoalProps
goalArea = createComponent "GoalArea"

renderGoalArea :: GoalProps -> JSX
renderGoalArea =
  make goalArea
  { initialState : initialGoalFormState
  , render: \self ->
  renderGoalState self.props.handleClick self.props.goalState
  <> renderGoalForm self
  }

notice :: JSX
notice =
  DOM.div { id: "notice"
          , className: "columns is-centered"
          , children:
            [ simpleColumn "is-half"
              [ simpleDiv "main-notice block"
                [ DOM.article
                  { className: "message is-warning"
                  , children:
                    [ simpleDiv "message-header"
                      [ para "" "Notice"
                      , DOM.button
                        { className: "delete"
                        , children: []
                        , onClick: handler_ killnotice
                        }
                      ]
                    , simpleDiv "message-body"
                      [ para "" "This page is a static mockup of Rewrite Live! Most functionality is not currently implemented."]
                    ]
                  }
                ]
              ]
            ]
          }







-- | ## Rendering the axiom form

-- | Given a `GoalFormState`, attempt to extract two expressions,
-- | otherwise fail.
formReady :: GoalFormState -> Maybe Equation
formReady { leftState, rightState } =
  go leftState rightState
  where
    go (BoxGood left) (BoxGood right) =
      Just (Equation {left, right})
    go _ _ = Nothing

-- | Compute the state of the textboxes from the name and statement value
mkState :: String -> String -> GoalFormState
mkState left right =
  { goalLeft: left
  , goalRight: right
  , leftState: go left
  , rightState: go right
  }

  where
    go text =
      if text == ""
      then BoxEmpty
      else case (parseExpression text) of
        Left sterr -> BoxError sterr
        Right expr -> BoxGood expr

-- | Render a form for submitting a new axiom.
renderGoalForm :: (Self GoalProps GoalFormState) -> JSX
renderGoalForm self =
  case self.props.goalState of
    GoalEmpty ->
      DOM.form { className: "block"
               , children: [ formLabel
                           , simpleDiv "field has-addons"
                             [ simpleDiv "control" [ leftInput ]
                             , simpleDiv "control" [ equalsButton ]
                             , simpleDiv "control" [ rightInput ]
                             , simpleDiv "control" [ sbmtButton ]
                             , simpleDiv "control" [ demoButton ]
                             ]
                           , help
                           , errors
                           ]
               , onSubmit: submit
               }
    _ -> mempty
  where
    submit :: EventHandler
    submit =
      case formReady self.state of
        Just eqn -> capture_ $ do
          self.props.handleSubmit eqn
          self.setState (\_ -> mkState "" "")
        Nothing ->
          capture_ $ pure unit
    handleLeft =
      capturingTargetValue self.setState (\left state -> mkState left state.goalRight)
    handleRight =
      capturingTargetValue self.setState (\right state -> mkState state.goalLeft right)
    formLabel =
      DOM.label { className: "label"
                , children: [ DOM.text "Set a goal" ]
                }
    leftInput = renderTextBox { value: self.state.goalLeft
                              , placeholder: "expr1"
                              , state: self.state.leftState
                              , onChange: handleLeft
                              }
    rightInput = renderTextBox { value: self.state.goalRight
                              , placeholder: "expr2"
                              , state: self.state.rightState
                              , onChange: handleRight
                              }
    equalsButton =
      DOM.button { className: "button is-static"
                 , type: "button"
                 , children: [ DOM.text "=" ]
                 }
    sbmtButton =
      DOM.button { className: "button is-info"
                 , type: "submit"
                 , disabled: isNothing (formReady self.state)
                 , _data:
                   if isNothing (formReady self.state)
                   then FO.singleton "tooltip" "Please fix errors first"
                   else FO.empty
                 , children: [ DOM.text "Add goal" ]
                 }
    demoButton =
      DOM.button { className: "button is-danger"
                 , children: [ DOM.text "Demo" ]
                 , onClick: handler_ self.props.runDemo
                 }
    helpText = "Type in two expressions to get started."
    help = para "help" helpText
    leftError =
      case self.state.leftState of
        BoxError perr ->
          Just $ "Parser error in left expression: " <> show perr
        _ -> Nothing
    rightError =
      case self.state.rightState of
        BoxError perr ->
          Just $ "Parser error in right expression: " <> show perr
        _ -> Nothing
    errors =
      simpleDiv "errors"
      [ DOM.ol_
        [ case leftError of
             Nothing -> DOM.text ""
             Just err -> DOM.li_ [ DOM.text err]
        , case rightError of
             Nothing -> DOM.text ""
             Just err -> DOM.li_ [ DOM.text err]
        ]
      ]

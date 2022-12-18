module RewriteLive.Axioms
       ( renderAxiomsArea
       , AxiomsProps
       )
       where

import Prelude
import Data.Array (fromFoldable)
import Data.Foldable (fold)
import Data.List (reverse)
import Data.Either (Either(..))
import Data.Maybe (Maybe(..), isJust, fromMaybe)
import Effect (Effect)
import React.Basic.Classic (Component, Self, JSX, createComponent, make)
import React.Basic.Events (EventHandler)
import React.Basic.DOM as DOM
import React.Basic.DOM.Events (capture_)
import Parsing as P
import RewriteLive.Events (capturingTargetValue)
import RewriteLive.DOM (simpleDiv, para, TextBoxState(..), renderTextBox)
import RewriteLive.Expr (renderExpr)
import RewriteLive.Parsers (parseAxiomName, parseAxiomStmt)
import RewriteLive.Types (Equation(..), RwDir(..), Axiom(..), Axioms, nameIsFresh)
import Foreign.Object as FO

-- | # State types

-- | Internal state of the axioms form
type AxiomsState =
  { name :: String
  , statement :: String
  }

-- | Default blank state of the axioms form
initialAxiomsState :: AxiomsState
initialAxiomsState = { name: ""
                     , statement: ""
                     }

type NameState = TextBoxState (Maybe P.ParseError) String

type StmtState = TextBoxState P.ParseError Equation

-- | Compute the state of the axiom name textbox from the current
-- | value of `name`.
nameToTextboxState :: Axioms -> String -> NameState
nameToTextboxState axioms name =
  if name == ""
  then BoxEmpty
  else case (parseAxiomName name) of
    Left nmerr ->
      BoxError (Just nmerr)
    Right name_out ->
      if not (nameIsFresh name_out axioms)
      then BoxError Nothing
      else BoxGood name

-- | Compute the state of the axiom statement textbox from the current
-- | value of `name`.
stmtToTextboxState :: String -> StmtState
stmtToTextboxState stmt =
  if stmt == ""
  then BoxEmpty
  else case (parseAxiomStmt stmt) of
    Left nmerr -> BoxError nmerr
    Right eqn  -> BoxGood eqn

-- | Given a `AxiomsState`, attempt to extract a name and equation
-- | from the current values, otherwise fail.
tryReadingFormState :: Self AxiomsProps AxiomsState -> Maybe Axiom
tryReadingFormState self =
  go (nameToTextboxState self.props.axioms self.state.name)
  (stmtToTextboxState self.state.statement)
  where
    go (BoxGood nm) (BoxGood eqn) =
      Just $ Axiom {name: nm, statement: eqn}
    go _ _ = Nothing

-- | # Rendering

-- | ## Rendering axioms

-- | Render a single `Axiom` alongside two buttons for
-- | selecting a rewrite in either direction.
-- |
-- | - `onButtonClick` should handle clicks on the rewrite buttons
renderAxiom :: (RwDir -> Effect Unit) -> Axiom -> JSX
renderAxiom onButtonClick (Axiom axiom) =
  simpleDiv "axiom"
  [ simpleDiv "axiom_name"
    [ DOM.text axiom.name ]
  , simpleDiv "axiom_stmt"
    [ renderExpr left
    , DOM.text " = "
    , renderExpr right
    ]
  , mkButton RwLeftToRight
  , mkButton RwRightToLeft
  ]
  where
    left = case axiom.statement of
      Equation eqn -> eqn.left
    right = case axiom.statement of
      Equation eqn -> eqn.right
    mkIcon :: RwDir -> JSX
    mkIcon dir = DOM.span
                 { className: "icon"
                 , children:
                   [ DOM.i { className: case dir of
                                RwLeftToRight -> "fas fa-right-long"
                                RwRightToLeft -> "fas fa-left-long"
                           }
                   ]
                 }
    mkButton :: RwDir -> JSX
    mkButton dir =
      DOM.button
      { className: "button rw-button is-small is-primary"
      , children: [ mkIcon dir]
      , onClick: capture_ (onButtonClick dir)
      }

-- | Render a set of `Axioms` as an `ol`
-- |
-- | - `onButtonClick` is used handle clicks on the rewrite buttons.
renderAxioms :: (Axiom -> RwDir -> Effect Unit) -> Axioms -> JSX
renderAxioms onButtonClick axioms =
  DOM.ol { className: "axioms_list"
         , children: fromFoldable $ map mkItem (reverse axioms)
         }
  where
    mkItem :: Axiom -> JSX
    mkItem axiom =
      DOM.li_ [renderAxiom (onButtonClick axiom) axiom]

-- | ## Rendering the axiom form

-- | Render a form for submitting a new axiom.
renderAxiomForm :: Self AxiomsProps AxiomsState -> JSX
renderAxiomForm self =
  DOM.form { className: "block"
           , children: [ formLabel
                       , simpleDiv "field has-addons"
                         [ simpleDiv "control" [ nameInput ]
                         , simpleDiv "control" [ stmtInput ]
                         , simpleDiv "control" [ sbmtButton ]
                         ]
                       , help
                       , errors
                       ]
           , onSubmit: trySubmit
           }
  where
    updateName =
      capturingTargetValue self.setState (\name state -> state { name = name })
    updateStmt =
      capturingTargetValue self.setState (\statement state -> state { statement = statement })
    trySubmit :: EventHandler
    trySubmit =
      case tryReadingFormState self of
        Just axiom -> capture_ $ do
          self.props.handleSubmit axiom
          self.setState $ \_ -> initialAxiomsState
        Nothing -> capture_ $ pure unit
    formLabel =
      DOM.label { className: "label"
                , children: [ DOM.text "Add a new equational axiom" ]
                }
    nameState = nameToTextboxState self.props.axioms self.state.name
    stmtState = stmtToTextboxState self.state.statement
    nameInput = renderTextBox { value: self.state.name
                              , placeholder: "axiom name"
                              , state: nameState
                              , onChange: updateName
                              }
    stmtInput = renderTextBox { value: self.state.statement
                              , placeholder: "expr1 = expr2"
                              , state: stmtState
                              , onChange: updateStmt
                              }
    ready = isJust $ tryReadingFormState self
    sbmtButton =
      DOM.button { className: "button is-info"
                 , type: "submit"
                 , disabled: not ready
                 , _data:
                   if not ready
                   then FO.singleton "tooltip" "Please fix errors first"
                   else FO.empty
                 , children: [ DOM.text "Add" ]
                 }
    helpText = "Add an axiom in the form expr1 = expr2. Make sure to give it a meaningful name."
    help = para "help" helpText
    nameError =
      case nameState of
        BoxError err ->
          case err of
            Nothing -> Just $ "Axiom name " <> self.state.name <> " is not fresh."
            Just perr -> Just $ "Parser error in name: " <> show perr
        _ -> Nothing
    stmtError =
      case stmtState of
        BoxError perr -> Just $ "Parser error in equation: " <> show perr
        _ -> Nothing
    maybeError :: Maybe String -> JSX
    maybeError merr = fromMaybe mempty ((\err -> DOM.li_ [ DOM.text err]) <$> merr)
    errors =
      simpleDiv "errors"
      [ DOM.ul
        { className: ""
        , children: [ maybeError nameError, maybeError stmtError ]
        }
      ]

type AxiomsProps =
  { axioms :: Axioms
  , handleSubmit :: Axiom -> Effect Unit
  , handleRewrite :: Axiom -> RwDir -> Effect Unit
  }

-- | ## Rendering the axiom area
axiomsArea :: Component AxiomsProps
axiomsArea = createComponent "AxiomsArea"

renderAxiomsArea :: AxiomsProps -> JSX
renderAxiomsArea = make axiomsArea
    { initialState: initialAxiomsState
    , render: \self ->
    fold [ mkTitle
         , mkList self
         , renderAxiomForm self
         ]
    }
  where
    mkTitle =
        DOM.h3 { className: "title is-3"
               , children: [ DOM.text "Axioms" ]
               }
    mkList self =
        DOM.div { className: "block"
                , children: [ DOM.label
                              { className: "label title is-5"
                              , children: [ DOM.text "Current axiom set" ]
                              }
                            , renderAxioms self.props.handleRewrite self.props.axioms
                            ]
                }

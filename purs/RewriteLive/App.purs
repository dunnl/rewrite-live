module RewriteLive.App
       ( renderApp
       , AppState (..)
       , initialAppState
       , demoAppState
       )
       where

import Prelude
import Data.List (List(..), (:), reverse, head, tail)
import Data.Maybe (Maybe(..))
import Data.Tuple (Tuple(..))
import Data.Traversable (foldMap, traverse)
import Effect (Effect)
import Effect.Console (log)
import React.Basic.Classic (Component, Self, JSX, createComponent, make)
import React.Basic.DOM as DOM
import RewriteLive.Expr (Expr(..), AnnExpr(..), extractNodes)
import RewriteLive.Goal ( renderGoalArea
                        , GoalState (..)
                        )
import RewriteLive.DOM ( simpleSection
                       , simpleColumns
                       , simpleColumn
                       , simpleContainer
                       , simpleDiv
                       )
import RewriteLive.Types ( Equation(..)
                         , Axiom(..)
                         , Axioms
                         , RwDir(..)
                         , LorR(..)
                         , RewriteCmd(..)
                         )
import RewriteLive.Axioms ( renderAxiomsArea )
import RewriteLive.Unification (searchForAxioms)

newtype AppState = AppState { goal :: GoalState
                            , axioms :: Axioms
                            , focus  :: Maybe (Tuple String RwDir)
                            , leftStart :: Maybe Expr
                            , rightStart :: Maybe Expr
                            , leftHistory :: List (Tuple Expr String)
                            , rightHistory :: List (Tuple Expr String)
                            , cmdHistory :: List RewriteCmd
                            , error :: Maybe String
                            }

initialAppState :: AppState
initialAppState =
  AppState { goal: GoalEmpty
           , axioms: Nil
           , focus: Nothing
           , leftStart: Nothing
           , rightStart: Nothing
           , leftHistory: Nil
           , rightHistory: Nil
           , cmdHistory: Nil
           , error: Nothing
           }

demoAppState :: AppState
demoAppState =
  AppState { goal: GoalPlain demo_l demo_r
           , focus: Nothing
           , axioms: (comm : assoc : Nil)
           , leftStart: Just demo_l
           , rightStart: Just demo_r
           , leftHistory: Nil
           , rightHistory: Nil
           , cmdHistory: Nil
           , error: Nothing
           }
  where
    comm_l = Node "P" (Var "x" : Var "y" : Nil)
    comm_r = Node "P" (Var "y" : Var "x" : Nil)
    assoc = Axiom { name: "assoc"
                 , statement: Equation { left: assoc_l
                                       , right: assoc_r }}
    comm = Axiom { name: "comm"
                 , statement: Equation { left: comm_l
                                       , right: comm_r }}
    assoc_l = Node "P" (Node "P" (Var "x" : Var "y" : Nil) : Var "z" : Nil)
    assoc_r = Node "P" (Var "x" : Node "P" (Var "y" : Var "z" : Nil) : Nil)
    demo_l = Node "P" (Var "a" : Node "P" (Var "b" : Var "c" : Nil) : Nil)
    demo_r = Node "P" (Var "c" : Node "P" (Var "a" : Var "b" : Nil) : Nil)

-- | Try to accept a new axiom
handleSubmit :: Self Unit AppState -> Axiom -> Effect Unit
handleSubmit self ax@(Axiom {name, statement: Equation {left, right}}) = do
    log ("Registering a new axiom '" <> name <> "'. Statement " <> show left <> " = " <> show right <> ".")
    self.setState (\(AppState state) -> AppState (state {axioms = Cons ax state.axioms}))

-- | Focus on an axiom in the current goal
handleSelect :: Self Unit AppState -> Axiom -> RwDir -> Effect Unit
handleSelect self ax@(Axiom {name}) rwdir = do
    log ("Axiom '" <> name <> "' was clicked in the " <> dir <> " direction.")
    self.setState (selectAxiom ax rwdir)
  where
    dir = if rwdir == RwLeftToRight then "left-to-right" else "right-to-left"

-- | Perform an actual rewrite
performRewrite :: Int -> Expr -> AnnExpr (Tuple Int (Maybe Expr)) -> Maybe Expr
performRewrite target replacement expr =
  case expr of
    AnnVar v -> Just (Var v)
    AnnNode fn args (Tuple uid _) ->
      if uid == target
      then Just replacement
      else Just (Node fn) <*> (traverse (performRewrite target replacement) args)

-- | Deal with a user clicking on matching box
handleRewrite :: Self Unit AppState -> LorR -> Int -> Expr -> Effect Unit
handleRewrite self side target new = do
  void $ log $ "Clicked " <> show target <> " on " <> show side
    <> ". Would replace with " <> show new <> "."
  let AppState st = self.state
  case st.focus of
    Nothing -> log "This is impossible"
    Just (Tuple name dir) ->
      self.setState $ \(AppState st0) ->
      case st0.goal of
        GoalEmpty -> AppState st0
        GoalPlain _ _ -> AppState st0
        GoalDec left right ->
          case side of
            OnLeft -> case performRewrite target new left of
              Nothing -> AppState (st0 {error = Just "WTF"})
              Just res ->
                AppState (st0 { goal = GoalPlain res (extractNodes right)
                              , cmdHistory = (RewriteCmd name dir side target) : st0.cmdHistory
                              , leftHistory = (Tuple res name : st0.leftHistory)
                              , focus = Nothing
                              })
            OnRight -> case performRewrite target new right of
              Nothing -> AppState (st0 {error = Just "WTF"})
              Just res ->
                AppState (st0 { goal = GoalPlain (extractNodes left) res
                              , cmdHistory = (RewriteCmd name dir side target) : st0.cmdHistory
                              , rightHistory = (Tuple res name : st0.rightHistory)
                              , focus = Nothing
                              })

type AppProps = Unit

appComponent :: Component AppProps
appComponent = createComponent "App"

foreign import typeset :: Effect Unit

-- | # Actions on `AppState`

{-
-- | Deselect an axiom if one is under focus
unselect :: AppState -> AppState
unselect (AppState state) =
  case state.goal of
    GoalEmpty -> AppState state
    GoalPlain _ _ -> AppState state
    GoalDec l r -> AppState (state { goal = GoalPlain (extractNodes l) (extractNodes r)
                                   , focus = Nothing
                                   })
-}

-- | In the goal, highlight all matching subexpressions where `Axiom`
-- can be applied in direction `RwDir`. This is the identity operation
-- if goal is `GoalEmpty`.
selectAxiom :: Axiom -> RwDir -> AppState -> AppState
selectAxiom (Axiom {name, statement}) dir (AppState state) =
  case state.goal of
    GoalEmpty -> AppState state
    GoalPlain left right ->
      AppState (state { goal = GoalDec (decorate left) (decorate right)
                      , focus = Just (Tuple name dir)
                      })
    GoalDec left right ->
      AppState (state { goal = GoalDec (decorate (extractNodes left)) (decorate (extractNodes right))
                      , focus = Just (Tuple name dir)
                      })
  where
    decorate = searchForAxioms statement dir

renderApp :: Unit -> JSX
renderApp = make appComponent
  { initialState: initialAppState
  , render: \self -> render self
  , didUpdate: \_ _ -> typeset
  , didMount: \_ -> typeset
  }
  where
    render self =
      let AppState state = self.state in
      simpleSection ""
      [ simpleContainer ""
        [ simpleColumns "is-centered"
          [ simpleColumn "is-two-thirds"
            [ DOM.h3 { className: "title is-3"
                     , children: [DOM.text "Goal"]
                     }
            , renderGoalArea ({ goalState: state.goal
                              , handleClick: handleRewrite self
                              , handleSubmit: \(Equation {left, right}) ->
                              self.setState (\(AppState state) ->
                                              AppState (state { goal = GoalPlain left right
                                                              , leftStart = Just left
                                                              , rightStart = Just right
                                                              }))
                              ,  runDemo: self.setState (\_ -> demoAppState)
                              })
            ]
          ]
        ]
      ] <>
      simpleSection ""
      [ simpleContainer ""
        [ simpleColumns ""
          [ simpleColumn ""
            [ renderAxiomsArea { axioms: state.axioms
                               , handleSubmit: handleSubmit self
                               , handleRewrite: handleSelect self
                               }
            ]
          , simpleColumn ""
            [ DOM.h3 { className: "title is-3"
                     , children: [DOM.text "Command history"]
                     }
            , renderScript self.state
            ]
          , simpleColumn ""
            [ DOM.h3 { className: "title is-3"
                     , children: [DOM.text "LaTeX Preview"]
                     }
            , renderLaTeX self.state
            ]
          ]
        ]
      ]

renderScript :: AppState -> JSX
renderScript (AppState {cmdHistory}) =
  DOM.ol { className: "script no-bullets"
         , children: case cmdHistory of
           Nil -> [DOM.text "Begin proving to see a proof script."]
           _ -> [foldMap renderCmd (reverse cmdHistory)] }
  where
    arrow RwLeftToRight = "→"
    arrow RwRightToLeft = "←"
    renderCmd (RewriteCmd name dir side loc) =
      DOM.li_ [DOM.text $ "rewrite " <> name <> " " <> arrow dir
               <> " at " <> show loc <> " on " <> show side <> "."]


renderLaTeX :: AppState -> JSX
renderLaTeX (AppState state) =
  case state.leftStart of
    Nothing ->
      simpleDiv "block"
      [ DOM.text "Enter a goal to see a LaTeX preview." ]
    Just left ->
      case state.rightStart of
           Nothing ->
             simpleDiv "block"
             [ DOM.text "Enter a goal to see a LaTeX preview." ]
           Just right ->
             simpleDiv "latex-preview" [ DOM.text (go left right) ]
  where
    open = """\[\begin{aligned}"""
    close = """\end{aligned}\]"""
    onRight string = " &= " <> string <> "\\\\"
    onRightNote string note =
      " &= " <> string <>
      " &&" <> """\text{\small """ <> note <> "} \\\\"
    mkStep :: Tuple Expr String -> String
    mkStep (Tuple expr axiom_name) =
      onRightNote (show expr) ("by " <> axiom_name)
    firstLine :: Expr -> String
    firstLine left =
      case head (reverse state.leftHistory) of
        Nothing ->
          show left <> onRight "\\ldots"
        Just step ->
          show left <> mkStep step
    leftSteps :: String
    leftSteps =
      case tail (reverse state.leftHistory) of
        Nothing ->
          mempty
        Just steps ->
          foldMap mkStep steps
          <> onRight """\ldots"""
    rightSteps :: String
    rightSteps =
      foldMap mkStep state.rightHistory
    lastLine :: Expr -> String
    lastLine right = """ &= """ <> show right
    go :: Expr -> Expr -> String
    go left right = open
                    <> firstLine left
                    <> leftSteps
                    <> rightSteps
                    <> lastLine right
                    <> close

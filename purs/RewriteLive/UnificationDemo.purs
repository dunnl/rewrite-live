module RewriteLive.UnificationDemo where

{-
-- | ## Rendering values

-- | Render a `MGU_State` as a HTML `div .box`
renderMguState :: MGU_State -> JSX
renderMguState state =
  simpleDiv "box"
  [ inRowOfColumns "mgustate"
    [ inColumn "is-three-fifths"
      [ DOM.h5 { className: "has-text-weight-bold",
                 children: [DOM.text "Potential disagreement pairs"]}
      , renderDsgps state.equations]
    , inColumn ""
      [ DOM.h5 { className: "has-text-weight-bold",
                 children: [DOM.text "Current m.g.u. \\(\\sigma\\)"] }
      , renderAsgns state.assignments
      ]
    ]
  ]


-- | Given a set of `Equations` used as the input for unification,
-- | render an HTML `div .columns` displaying information about the
-- | start of the problem.
renderMguStart :: Equations -> JSX
renderMguStart eqns =
  inRowOfColumns "is-centered"
  [ inColumn "" [update]
  , inColumn "" [state]
  ]
  where
    start_msg = "Attempting to compute a most general unifier for the following system."
    update =  simpleDiv "update" [DOM.text start_msg]
    state = renderMguState
            { assignments: Nil
            , equations: eqns
            }


-- | Render the `MGU_Result` resulting from a run of the unification problem.
-- | render an HTML `div .columns` displaying information about the
-- | conclusion of the problem.
renderMguResult :: MGU_Result -> JSX
renderMguResult res =
  inRowOfColumns "is-centered" $
  [ inColumn "" $
    [ simpleDiv "update"
      [ case res of
           Solvable _ ->
             DOM.text "The equation is solvable according to the displayed unifier."
           Unsolvable _ ->
             DOM.text "The system cannot be unified."
      ]
    ]
  ]

-- | Render the `MGU_Result` resulting from a run of the unification problem.
-- | as a nice summary.
renderMguSummary :: MGU_Result -> JSX
renderMguSummary res =
  inRowOfColumns "is-centered" $
  [ inColumn "is-half" $
    [ case res of
         Solvable asgns ->
           DOM.article { className: "message is-success"
                       , children:
                         [ simpleDiv "message-header" [para "Success"]
                         , simpleDiv "message-body has-text-centered"
                           [ para "This problem has the following most general unifier:"
                           , renderAsgns asgns
                           ]
                         ]
                       }

         Unsolvable _ ->
           DOM.article { className: "message is-danger"
                       , children:
                         [ simpleDiv "message-header" [para "Failure"]
                         , simpleDiv "message-body has-text-centered"
                           [ para "This problem is not unifiable." ]
                         ]
                       }
    ]
  ]


renderMguUpdate :: MGU_Update -> JSX
renderMguUpdate {msg, next} =
  simpleDiv "columns is-centered"
  [ simpleDiv "column" [simpleDiv "update" [msg]]
  , simpleDiv "column" [renderMguState next]
  ]

renderTrace :: Trace -> JSX
renderTrace trace =
  renderMguSummary trace.final <>
  renderMguStart trace.start <>
  foldMap renderMguUpdate trace.history <>
  renderMguResult trace.final

-- | # The unification app
type State =  Record (e1 :: String, e2 :: String)

-- | Compute whether the `State` constitutes a well-formed `Equation`
stateEquation :: State -> Maybe Equation
stateEquation {e1, e2} = do
  e1' <- tryParseExpression e1
  e2' <- tryParseExpression e2
  pure (Equation {left: e1', right: e2'})


-- | The initial state of the app is a set of two empty textboxes.
initialState :: State
initialState =
   { e1: ""
   , e2: ""
   }

-- | Render the `State` of the app
renderState :: State -> JSX
renderState st =
  inRowOfColumns "is-centered"
  [ inColumn ""
    [ case stateEquation st of
         Nothing ->
           DOM.text "Enter two expressions to compute their most general unifier, if there is one."
         Just eqn ->
           renderTrace (solveUnificationProblem eqn)
    ]
  ]

foreign import typeset :: Effect Unit

renderUnificationDemo :: Unit -> JSX
renderUnificationDemo = make (createComponent "UnificationDemo")
  { initialState: initialState
  , didUpdate: \_ _ -> typeset
  , render: \self ->
    inRowOfColumns "is-centered"
    [ inColumn "is-half"
      [ renderExprTextBox { label: Just "Expression 1"
                          , value: self.state.e1
                          , placeholder: "expr1"
                          , handleValue: readValueExpr
                          , onChange: capture targetValue $ \targetValue ->
                          self.setState $ \s -> s { e1 = fromMaybe "" targetValue }
                          }
      , renderExprTextBox { label: Just "Expression 2"
                          , value: self.state.e2
                          , placeholder: "expr2"
                          , handleValue: readValueExpr
                          , onChange: capture targetValue $ \targetValue ->
                          self.setState $ \s -> s { e2 = fromMaybe "" targetValue }
                          }
      ]
    ] <>
    renderState self.state
  }
-}

{-

-- | ## JSX for equations and assignments

-- | Render an `Equation` as an HTML `div .eqn`.
renderEqn :: Equation -> JSX
renderEqn (Equation {left, right}) =
  DOM.div { className: "eqn",
          children: [ renderExpr left
                    , DOM.text " = "
                    , renderExpr right
                    ]
        }

-- | Render a disagreeing pair as an HTML `div .expr-inline`.  This is
-- | pretty-printed per common convention, as a pair between angle
-- | brackets.
renderDsgp :: Equation -> JSX
renderDsgp (Equation {left, right}) =
  DOM.div { className: "expr-inline",
          children: [ DOM.text "\\(\\langle\\)",
                      renderExpr left,
                      DOM.text ", ",
                      renderExpr right,
                      DOM.text "\\(\\rangle\\)"
                      ]
        }

-- | Convert a set of `Equations` into an HTML ordered list.
renderDsgps :: Equations -> JSX
renderDsgps eqns =
  DOM.ol_ (fromFoldable $ (\x -> DOM.li {children: [x]}) <$> (renderDsgp <$> eqns))

-- | Render an `Assignment` into HTML.
renderAsgn :: Assignment -> JSX
renderAsgn (Assignment {variable: v, assignment: ex2}) =
  simpleDiv "eqn"
  [ renderExpr (Var v)
  , DOM.text " \\(\\mapsto\\) "
  , renderExpr ex2
  ]

-- | Render a set of `Assignments` an HTML `ol`.
renderAsgns :: Assignments -> JSX
renderAsgns asgns =
  DOM.ol_ (fromFoldable $ (renderAsgn <$> asgns))

-}




{-
   trivialCase :: String -> String -> JSX
   trivialCase x y = DOM.span_
         [ DOM.text "Both variables of "
         , renderDsgp (Equation {left: Var x, right: Var y})
         , DOM.text " are equal, so this is not a disagreement and we can discard this constraint. "
         ]
   successMsg :: Expr -> Expr -> String
   successMsg n1 n2 =
     "Unification of " <> show n1 <> " with " <> show n2
      <> " induces new potential disagreement pairs for corresponding arguments. "
      <> "We push these new constraints back onto the stack."
   unequalHeads :: Expr -> Expr -> String
   unequalHeads n1 n2 = "There is no way to unify " <> show n1 <> " with " <> show n2 <> " because their heads are not equal."
   unequalArgLen :: Expr -> Expr -> String
   unequalArgLen n1 n2 =  "There is no way to unify " <> show n1 <> " with " <> show n2
             <> "because they are applied to different numbers of arguments."
-}
{-
    (DOM.text $ "This disagreement pair is unified by the following substitution:") <>
    (DOM.text $ "\\[" <> v <> "\\mapsto " <> show e <> "\\]") <>
    (DOM.text $ "We apply this substitution to the m.g.u. and to the remaining disagreement pairs on the stack."),
-}

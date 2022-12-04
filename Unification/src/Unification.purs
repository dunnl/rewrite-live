module Unification where

import Data.Array(fromFoldable)
import Data.Either (Either(..))
import Effect (Effect)
import Expr (Expr(..), expr_sub, exprToJSX, occurs_in, tryParseExpression, AnnExpr(..), lweird)
import ExprTextBox (exprTextBox)
import Misc hiding (fromFoldable)
import React.Basic.Classic (Component, JSX, createComponent, make)
import React.Basic.DOM as R
import React.Basic.DOM.Events (capture, targetValue)


-- | # Equations and assignments


-- | An `Equation` is a pair of expressions.
type Equation = Pair Expr

-- | A set of `Equation`s.
type Equations = List Equation

-- | An `Assignment` is a variable paired with an `Expression` to
-- | replace it with.
type Assignment = Record (variable :: String, assignment :: Expr)

-- | A set of `Assignment`s.
type Assignments = List Assignment

-- | Lift an `Assignment` to an `Equation` by applying `Var` to the
-- | variable.
asgnToEqn :: Assignment -> Equation
asgnToEqn asgn = Pair (Var asgn.variable) asgn.assignment

-- | ## Substitution and occurrence in equations and assignments

-- | Apply a single-variable substitution to the right-hand side of an
-- | `Assignment`.
asgn_sub :: String -> Expr -> Assignment -> Assignment
asgn_sub v e asgn = asgn {assignment = expr_sub v e asgn.assignment}

-- | Compute whether a given variable occurs anywhere in a set of
-- | equations
occurs_in_eqns :: String -> Equations -> Boolean
occurs_in_eqns v eqns =
  any (\p -> eitherOf (occurs_in v <$> p)) eqns

-- | Apply a single-variable substitution to a set of equations.  Both
-- | sides of each equation are substituted.
varElim_eqns :: String -> Expr -> Equations -> Equations
varElim_eqns v e eqns = (<$>) (expr_sub v e) <$> eqns

-- | Apply a single-variable substitution to a set of an assignments.
-- | Subsitution applies to the right-hand side (the assigned
-- | expression) only, ignoring the left-hand side (the variable).
varElim_asgns :: String -> Expr -> Assignments -> Assignments
varElim_asgns v e asgns =
  (\asgn -> asgn {assignment = expr_sub v e asgn.assignment}) <$> asgns

-- | ## JSX for equations and assignments

-- | Render an `Equation` as an HTML `div`.
eqnToJSX :: Equation -> JSX
eqnToJSX (Pair ex1 ex2) =
  R.div { className: "eqn",
          children: [ exprToJSX ex1,
                      R.text " = ",
                      exprToJSX ex2 ]
        }

-- | Render an `Equation` as an HTML `div`, formatting it as a
-- | disagreement pair for unification. This follows the common
-- | convention, displaying the expressions as a pair with angle
-- | brackets.
dsgpToJSX :: Equation -> JSX
dsgpToJSX (Pair ex1 ex2) =
  R.div { className: "expr",
          children: [ R.text "\\(\\langle\\)",
                      exprToJSX ex1,
                      R.text ", ",
                      exprToJSX ex2,
                      R.text "\\(\\rangle\\)"
                      ]
        }

-- | Convert a set of `Equations` into a list of rendered HTML.
eqnsToJSX :: Equations -> List JSX
eqnsToJSX eqns = dsgpToJSX <$> eqns

-- | Convert a set of `Equations` into an HTML ordered list.
eqnsToJSX' :: Equations -> JSX
eqnsToJSX' eqns =
  R.ol_ (fromFoldable $ (\x -> R.li {children:[x]}) <$> (eqnsToJSX eqns))

-- | Render an `Assignment` into HTML.
asgnToJSX :: Assignment -> JSX
asgnToJSX ({variable: v, assignment: ex2}) =
  R.div { className: "eqn",
          children: [ exprToJSX (Var v),
                      R.text " \\(\\mapsto\\) ",
                      exprToJSX ex2 ]
        }

-- | Convert a set of `Assignments` into a list of rendered HTML.
asgnsToJSX :: Assignments -> List JSX
asgnsToJSX asgns = asgnToJSX <$> asgns

-- | Convert a set of `Assignments` into an HTML ordered list.
asgnsToJSX' :: Assignments -> JSX
asgnsToJSX' asgns =
  R.ol_ (fromFoldable $ asgnsToJSX asgns)

-- | # Most general unifier engine
-- | Can I put something here?

-- | A state value maintained during unification.  The `Equations`
-- | represent unprocessed disagreement pairs. (Pedantically speaking
-- | they are only "potential" disagreement pairs, as the unprocessed
-- | pairs may contain pairs of equal variables, which would then be
-- | discarded by the unification engine.)  The `Assignments` describe
-- | the most general unifier (m.g.u.) as calculated thus far. Note
-- | that the m.g.u. can be updated as we process more disagreement
-- | pairs, and indeed we may find there is no m.g.u. at all if we
-- | find a disagreement pair we cannot resolve.
-- |
-- | **Invariant**: If `equations` is empty, the `assignments` should
-- | describe the m.g.u. of the original problem. If we find the
-- | problem is unsolvable, no `MGU_State` value should be computed.
type MGU_State = { assignments :: Assignments
                 , equations :: Equations
                 }

-- | The result of unification. If the problem can be solved, an
-- | m.g.u. is provided as a set of `Assignments`. If the problem
-- | cannot be solved, a text value is provided which should explain
-- | the nature of the unresolvable conflict encountered during unification.
data MGU_Result =
    Solvable Assignments
  | Unsolvable String

-- | An update message generated by one successful step in the
-- | unification procedure. `msg` should contain rendered HTML
-- | describing the actions taken during this step. `next` should
-- | contain the next state value to be fed to the next iteration.
type MGU_Update = Record (msg :: JSX, next :: MGU_State)

-- | Given a current state, take one step in the unification
-- | procedure. Operationally, we pop off a disagreement pair from the
-- | stack of unprocessed equations (if no such disagreement pairs
-- | exist,
processOneEquation :: MGU_State -> Either MGU_Result MGU_Update
processOneEquation st =
  case (popList st.equations) of
    Nothing -> -- There are no non-assignment equations left, we are done
      Left (Solvable st.assignments)
    Just (Tuple eqn rest) -> -- We have a new equation to consider
      step eqn (st {equations = rest})

-- The state argument should not contain equation
step :: Equation -> MGU_State ->  Either MGU_Result MGU_Update
step eqn st =
  case eqn of
    input@(Pair (Var x) (Var y)) ->
      if x == y
      then -- This constraint is trivial, we can completely ignore it
        Right { next: st,
                msg: R.span { children:
                              [ R.text "Both variables of ",
                                dsgpToJSX input,
                                R.text " are equal, so this is not a disagreement and we can discard this constraint. "
                              ]}}
      else -- We have a var=var constraint, substitute and hang on to it
        Right $ pushNewAssignment x (Var y) st
    Pair (Node f args) (Var v) ->
      step (Pair (Var v) (Node f args)) st
    Pair (Var v) (Node f args) ->
      Right $ pushNewAssignment v (Node f args) st
    Pair n1@(Node f1 args1) n2@(Node f2 args2) ->
      if (f1 /= f2)
      then Left (Unsolvable $ "There is no way to unify " <> show n1 <> " with " <> show n2 <> " because their heads are not equal.")
      else if (length args1 /= length args2) then
             Left $ Unsolvable $ "There is no way to unify " <> show n1 <> " with " <> show n2
             <> ". The heads match but they are applied to different numbers of arguments"
           else Right { msg: R.text $
                        "Unification of " <> show n1 <> " with " <> show n2
                        <> " induces new potential disagreement pairs for corresponding arguments. "
                        <> "We push these new constraints back onto the stack.",
                        next: st {equations = (zipWith Pair args1 args2) <> st.equations}}

pushNewAssignment :: String -> Expr -> MGU_State -> MGU_Update
pushNewAssignment v e st =
  { msg:
    (R.text $ "This disagreement pair is unified by the following substitution:") <>
    (R.text $ "\\[" <> v <> "\\mapsto " <> show e <> "\\]") <>
    (R.text $ "We apply this substitution to the m.g.u. and to the remaining disagreement pairs on the stack."),
    next:
    { assignments: Cons {variable: v, assignment: e} (varElim_asgns v e st.assignments),
      equations: varElim_eqns v e st.equations
    }}

type Trace =
  Record (start :: Equations,
          history :: List MGU_Update,
          final :: MGU_Result)

solvingStep :: MGU_State -> Equations -> List MGU_Update -> Trace
solvingStep st start history =
  case processOneEquation st of
    Left result -> { start, history, final: result }
    Right {msg, next} ->
      solvingStep next start (history <> (Cons {msg, next} Nil))

trySolving :: Equations -> Trace
trySolving eqns =
  solvingStep {assignments: Nil, equations: eqns} eqns Nil

tryUnifying :: Equation -> MGU_Result
tryUnifying = _.final <<< trySolving <<< singleton

unifyAll :: Expr -> Int -> Expr -> AnnExpr (Tuple Int MGU_Result)
unifyAll e_uni n e_main =
  case e_main of
    Var v -> AVar v
    Node fn args ->
      let mgu_here = tryUnifying (Pair e_uni e_main)
      in ANode fn (lweird (unifyAll e_uni) (n+1) args) (Tuple n mgu_here)

weird :: Int -> Expr -> AnnExpr Int
weird n (Var v) = AVar v
weird n (Node fn args) = ANode fn (lweird weird (n + 1) args) n

-- | ## React component
mguStateComponent :: Component (Record (state :: MGU_State))
mguStateComponent = createComponent "State"

mguResultComponent :: Component (Record (res :: MGU_Result))
mguResultComponent = createComponent "Result"

mguTraceComponent :: Component (Record (trace :: Trace))
mguTraceComponent = createComponent "Trace"

stateJSX :: Record (state :: MGU_State) -> JSX
stateJSX {state} =
  R.div { className: "box",
          children:
          [R.div { className: "columns mgustate",
                   children:
                   [ R.div { className: "column is-three-fifths",
                             children: [ R.h5 { className: "has-text-weight-bold",
                                                children: [R.text "Potential disagreement pairs"]}
                                       , eqnsToJSX' state.equations]},
                     R.div { className: "column",
                             children: [ R.h5 { className: "has-text-weight-bold",
                                                children: [R.text "Current m.g.u. \\(\\sigma\\)"] }
                                       , asgnsToJSX' state.assignments]}]}]}

startJSX :: Equations -> JSX
startJSX eqns =
  R.div { className: "columns is-centered",
          children:
          [ R.div { className: "column",
                    children: [R.div { className: "update",
                                       children: [R.text "Attempting to compute a most general unifier for the following system."]}]},
            R.div { className: "column is-three-fifths",
                    children: [stateJSX {state: {assignments: Nil, equations: eqns}}]}]}

finalJSX :: MGU_Result -> JSX
finalJSX res =
  R.div { className: "columns is-centered",
          children:
          [ R.div { className: "column"
                  , children: [R.div { className: "update",
                                       children: [
                                         case res of
                                           Solvable asgns -> R.text "The equation is solvable according to the displayed unifier."
                                           Unsolvable str -> R.text "The system cannot be unified."
                                         ]
                                     }
                              ]
                    }
          , R.div { className: "column is-three-fifths"
                  , children: []
                  }
          ]
        }

updateJSX :: MGU_Update -> JSX
updateJSX {msg, next} =
  R.div { className: "columns is-centered",
          children:
          [ R.div { className: "column",
                    children: [R.div { className: "update",
                                       children: [msg]}]},
            R.div { className: "column is-three-fifths",
                    children: [stateJSX {state: next}]}]}

traceJSX :: Trace -> JSX
traceJSX trace =
  startJSX trace.start <>
  (fold $ updateJSX <$> trace.history) <>
  finalJSX trace.final

stateEquation :: State -> Maybe Equation
stateEquation {e1, e2} = do
  e1' <- tryParseExpression e1
  e2' <- tryParseExpression e2
  pure (Pair e1' e2')

-- | ## Unification engine demo

demoComponent :: Component Unit
demoComponent = createComponent "UnificationDemo"

type State =
  Record (e1 :: String, e2 :: String, trace :: Maybe Trace)

initialState :: State
initialState =
   { e1: "",
     e2: "",
     trace: Nothing
   }

stateToTrace :: State -> JSX
stateToTrace st =
  case stateEquation st of
    Nothing -> R.text "Enter two expressions to compute their most general unifier, if there is one."
    Just eqn ->  R.div { className: "columns is-centered",
                         children:
                         [ R.div { className: "column",
                                   children: [traceJSX (trySolving (singleton eqn))] }
                         ]}

foreign import typeset :: Effect Unit

demoJSX :: Unit -> JSX
demoJSX = make demoComponent
  { initialState: initialState
  , didUpdate: \_ _ -> typeset
  , render: \self ->
        R.div { className: "columns is-centered",
                children:
                [ R.div { className: "column is-half",
                          children:
                          [ exprTextBox { label: "Expression 1",
                                          value: self.state.e1,
                                          onChange: capture targetValue $ \targetValue ->
                                           self.setState $ \s -> s { e1 = fromMaybe "" targetValue } },
                            exprTextBox { label: "Expression 2",
                                          value: self.state.e2,
                                          onChange: capture targetValue $ \targetValue ->
                                           self.setState $ \s -> s { e2 = fromMaybe "" targetValue } }
                          ]
                        }
                ]
              }
        <> stateToTrace self.state
  }

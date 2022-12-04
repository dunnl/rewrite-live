module PS.HelloWorld (jsHelloWorld) where
import Prelude
import Data.Interpolate (i)
import React.Basic.DOM (div_, p_, text)
import React.Basic.Hooks (Component, JSX, component)
import React.Basic.Hooks as Hooks
import Effect.Unsafe (unsafePerformEffect)
type Props = { msg :: String }


jsHelloWorld :: Props -> JSX
jsHelloWorld = unsafePerformEffect mkHelloWorld
mkHelloWorld :: Component Props
mkHelloWorld =
   component "HelloWorld" \props -> Hooks.do
       pure do
           div_
               [ p_ [ text $ i "Hello World, " props.msg ] ]

main :: forall e. Eff (dom :: DOM, console :: CONSOLE | e) Unit
main = do
  doc <- getDocument
  content <- unsafeFromJust <$> getElementById' "content" doc
  setInnerHTML ("<h1>" <> greetingWords <> "</h1>") content
  log greetingWords

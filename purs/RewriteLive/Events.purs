module RewriteLive.Events where

import Prelude
import Data.Maybe (fromMaybe)
import Effect (Effect)
import React.Basic.Events (EventHandler)
import React.Basic.DOM.Events (capture, targetValue)

-- | Create that sets the state using the target value of the event
capturingTargetValue :: forall state.
                        ((state -> state) -> Effect Unit) ->
                        (String -> state -> state) ->
                        EventHandler
capturingTargetValue setState go =
  capture targetValue $ \targetValue ->
  setState $ go (fromMaybe "" targetValue)

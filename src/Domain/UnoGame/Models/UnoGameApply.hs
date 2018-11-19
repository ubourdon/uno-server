module Domain.UnoGame.Models.UnoGameApply where

import Domain.UnoGame.Events.UnoGameEvents (UnoGameEvent(..))
import Domain.UnoGame.Models.UnoGameState (State(..))

apply :: State -> UnoGameEvent -> State
apply state (GameStarted players) = State players
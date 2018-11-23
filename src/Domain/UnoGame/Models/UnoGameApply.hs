module Domain.UnoGame.Models.UnoGameApply where

import Domain.UnoGame.Events.UnoGameEvents (UnoGameEvent(..))
import Domain.UnoGame.Models.UnoGameState (State(State), StateUid(StateUid))

import Domain.Common.EventMetadata (AggregateUid(AggregateUid))

apply :: State -> UnoGameEvent -> State
apply state (GameStarted _ (AggregateUid uid) players) = State (StateUid uid) players
module Domain.UnoGame.Models.UnoGameApply where

import Domain.UnoGame.Events.UnoGameEvents (UnoGameEvent(..))
import Domain.UnoGame.Models.UnoGameState (State(State), StateUid(StateUid))

import Domain.Common.EventMetadata (AggregateUid(AggregateUid))

apply :: State -> UnoGameEvent -> State
apply _ (GameStarted _ (AggregateUid uid) players _ _) = State (StateUid uid) players
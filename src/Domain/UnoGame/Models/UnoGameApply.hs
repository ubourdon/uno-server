module Domain.UnoGame.Models.UnoGameApply where

import Domain.UnoGame.Events.UnoGameEvents (UnoGameEvent(..))
import Domain.UnoGame.Models.UnoGameState (State(State), StateUid(StateUid), LastPlayer(..))
import Domain.UnoGame.Models.Player (Player, Player)

import Domain.Common.EventMetadata (AggregateUid(AggregateUid))

apply :: State -> UnoGameEvent -> State
apply _ (GameStarted _ (AggregateUid uid) players _ _) = State (StateUid uid) players (getFirstPlayer players)

-- TODO FAUX STUB ! A changer. Récupérer le player à la position 0
getFirstPlayer :: [Player] -> LastPlayer
getFirstPlayer players = LastPlayer (head players)
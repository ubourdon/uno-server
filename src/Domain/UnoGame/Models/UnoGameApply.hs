module Domain.UnoGame.Models.UnoGameApply where

import Domain.UnoGame.Events.UnoGameEvents (UnoGameEvent(..))
import Domain.UnoGame.Models.UnoGameState (State(State), StateUid(StateUid), LastPlayer(..))
import Domain.UnoGame.Models.Player (Player, Player)
import qualified Data.List.NonEmpty as NEL

import Domain.Common.EventMetadata (AggregateUid(AggregateUid))

apply :: State -> UnoGameEvent -> State
apply _ (GameStarted _ (AggregateUid uid) players _ cardPackage) =
  State (StateUid uid) players (getFirstPlayer players) (drawFirstCard cardPackage)

-- TODO FAUX STUB ! A changer. Récupérer le player à la position 0
getFirstPlayer :: NEL.NonEmpty Player -> LastPlayer
getFirstPlayer players = LastPlayer (NEL.head players)

drawFirstCard :: NEL.NonEmpty a -> NEL.NonEmpty a
drawFirstCard (x NEL.:| xs@(h:_)) = NEL.fromList xs
drawFirstCard (x NEL.:| []) = error "ça ne devrait pas arriver => gérer ça par des types"
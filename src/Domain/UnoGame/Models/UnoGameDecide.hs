module Domain.UnoGame.Models.UnoGameDecide (decide) where

import Domain.UnoGame.Commands.UnoGameCommands (UnoGameCommand(..))
import Domain.UnoGame.Events.UnoGameEvents (UnoGameEvent(..))
import Domain.UnoGame.Models.UnoGameState (State(..), LastPlayer(..))
import Domain.UnoGame.Events.UnoGameErrors (UnoGameError(..))
import Domain.UnoGame.Models.Player (Player(Player), PlayerUid, PlayerPosition(PlayerPosition))
import qualified Data.List.NonEmpty as NEL
import Data.List
import Data.Maybe
import Utils.Thrush ((|>))

decide :: State -> UnoGameCommand -> Either UnoGameError [UnoGameEvent]
decide state (StartGame pUid aUid players cardsPackage) =
  ifEmptyState state [GameStarted pUid aUid players (NEL.head cardsPackage) cardsPackage]

decide state (PlayCard pUid aUid playerUid card) = Right (
    case (playAtWrongTurn state playerUid [CardPlayedAtWrongTurn pUid aUid playerUid card]) of
      [] -> [CardPlayed pUid aUid playerUid card]
      [x] -> [x]
  )

ifEmptyState :: State -> [UnoGameEvent] -> Either UnoGameError [UnoGameEvent]
ifEmptyState EmptyState evts = Right evts
ifEmptyState (State _ _ _) _ = Left GameIsAlreadyStarted

-- gameUpdate
-- ColorChanged
-- DirectionChanged

-- penalties

playAtWrongTurn :: State -> PlayerUid -> [UnoGameEvent] -> [UnoGameEvent]
playAtWrongTurn (State _ players (LastPlayer (Player _ _ (PlayerPosition lastPlayerPosition)))) nextPlayerUid events
  | lastPlayerPosition == lastPositionOnList = undefined -- position == 0
  | otherwise = case ((nextPlayerPosition - lastPlayerPosition) == 1) of
    True -> []
    False -> events
  where lastPositionOnList = (length players) -1
        nextPlayerPosition = (find (\p -> (getPlayerUid p) == nextPlayerUid) players) |> fmap (getPlayerPosition) |> fromMaybe 42
-- playAtWrongTurn EmptyState _ = ERROR

--getPositionValue (PlayerPosition p) = p
getPlayerPosition (Player _ _ (PlayerPosition p)) = p
getPlayerUid (Player uid _ _) = uid

{-
nextplayer doit avoir la prochaine position
  Si 3 joueurs
    1 > 0
    2 > 1
    3 > 2
    0 > 3

    0 -> 1 -> 2 -> 3

    1 cas spécifiques :
      - last player position = (lengh players) -1
-}

--suffleCards :: [Card] -> [Card]
--suffleCards cards = cards
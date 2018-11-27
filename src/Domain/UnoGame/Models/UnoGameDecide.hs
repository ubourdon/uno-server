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
      [x] -> [x] ++ (givePenaltys x state)
  )

ifEmptyState :: State -> [UnoGameEvent] -> Either UnoGameError [UnoGameEvent]
ifEmptyState EmptyState evts = Right evts
ifEmptyState (State _ _ _ _) _ = Left GameIsAlreadyStarted

-- gameUpdate
-- ColorChanged
-- DirectionChanged

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
playAtWrongTurn :: State -> PlayerUid -> [UnoGameEvent] -> [UnoGameEvent]
playAtWrongTurn (State _ players (LastPlayer (Player _ _ (PlayerPosition lastPlayerPosition))) _) nextPlayerUid events
  | lastPlayerPosition == lastPositionOnList = undefined -- TODO position == 0
  | otherwise = case ((nextPlayerPosition - lastPlayerPosition) == 1) of
    True -> []
    False -> events
  where lastPositionOnList = (length players) -1
        nextPlayerPosition = (find (\p -> (getPlayerUid p) == nextPlayerUid) players) |> fmap (getPlayerPosition) |> fromMaybe 42
playAtWrongTurn EmptyState _ _ = error "impossible d'avoir empty state ici : playAtWrongTurn"

givePenaltys :: UnoGameEvent -> State -> [UnoGameEvent]
givePenaltys evt@(CardPlayed _ _ _ _) _ = [evt]
givePenaltys evt@(CardPlayedAtWrongTurn pUid aUid playerUid card) (State _ _ _ deck) =
  [PenaltyGaveToPlayer pUid aUid playerUid (pickNCard 2 1 deck)]

getPlayerPosition (Player _ _ (PlayerPosition p)) = p
getPlayerUid (Player uid _ _) = uid

pickNCard :: Int -> Int -> NEL.NonEmpty a -> NEL.NonEmpty a
pickNCard cardNumber counter (x NEL.:| xs)
  | counter < cardNumber = x NEL.<| (pickNCard cardNumber (counter +1) (NEL.fromList xs)) -- xs TODO ne doit pas être vide ici
  | otherwise = x NEL.:| []

--pickNCard _ _ (_ NEL.:| []) = error "ça ne devrait pas arriver => gérer ça via les types => deck doit avoir assez de carte pour tous les penalties"
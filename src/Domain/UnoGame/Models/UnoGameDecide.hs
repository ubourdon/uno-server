module Domain.UnoGame.Models.UnoGameDecide (decide) where

import Domain.UnoGame.Commands.UnoGameCommands (UnoGameCommand(..))
import Domain.UnoGame.Events.UnoGameEvents (UnoGameEvent(..))
import Domain.UnoGame.Models.UnoGameState (State(..))
import Domain.UnoGame.Events.UnoGameErrors (UnoGameError(..))
import qualified Data.List.NonEmpty as NEL

decide :: State -> UnoGameCommand -> Either UnoGameError [UnoGameEvent]
decide state (StartGame pUid aUid players cardsPackage) =
  ifEmptyState state [GameStarted pUid aUid players (NEL.head cardsPackage) cardsPackage]

ifEmptyState :: State -> [UnoGameEvent] -> Either UnoGameError [UnoGameEvent]
ifEmptyState EmptyState evts = Right evts
ifEmptyState (State _ _) _ = Left GameIsAlreadyStarted

--suffleCards :: [Card] -> [Card]
--suffleCards cards = cards
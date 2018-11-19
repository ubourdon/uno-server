module Domain.UnoGame.Models.UnoGameDecide (decide) where

import Domain.UnoGame.Commands.UnoGameCommands (UnoGameCommand(..))
import Domain.UnoGame.Events.UnoGameEvents (UnoGameEvent(..))
import Domain.UnoGame.Models.UnoGameState (State(..))
import Domain.UnoGame.Events.UnoGameErrors (UnoGameError(..))
import Data.Either

decide :: State -> UnoGameCommand -> Either UnoGameError [UnoGameEvent]
decide state (StartGame players) = ifEmptyState state [GameStarted players]

ifEmptyState :: State -> [UnoGameEvent] -> Either UnoGameError [UnoGameEvent]
ifEmptyState EmptyState evts = Right evts
ifEmptyState (State _) _ = Left GameIsAlreadyStarted
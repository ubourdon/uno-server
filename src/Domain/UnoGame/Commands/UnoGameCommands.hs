module Domain.UnoGame.Commands.UnoGameCommands (UnoGameCommand(..), Player) where

import Domain.UnoGame.Events.UnoGameEvents (Player)
import Domain.Common.EventMetadata (ProcessUid, AggregateUid)

data UnoGameCommand = StartGame ProcessUid AggregateUid  [Player] deriving (Show)
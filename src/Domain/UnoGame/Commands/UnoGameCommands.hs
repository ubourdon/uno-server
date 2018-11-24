module Domain.UnoGame.Commands.UnoGameCommands (UnoGameCommand(..), Player) where

import Domain.UnoGame.Models.Player (Player)
import Domain.Common.EventMetadata (ProcessUid, AggregateUid)
import Domain.UnoGame.Models.Cards (CardsPackage)

data UnoGameCommand = StartGame ProcessUid AggregateUid [Player] CardsPackage deriving (Show)
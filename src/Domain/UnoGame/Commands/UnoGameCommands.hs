module Domain.UnoGame.Commands.UnoGameCommands (UnoGameCommand(..), Player) where

import Domain.UnoGame.Models.Player (Player, PlayerUid)
import Domain.Common.EventMetadata (ProcessUid, AggregateUid)
import Domain.UnoGame.Models.Cards (CardsPackage, Card)
import Data.List.NonEmpty

data UnoGameCommand =
  StartGame ProcessUid AggregateUid (NonEmpty Player) CardsPackage |
  PlayCard ProcessUid AggregateUid PlayerUid Card
  deriving (Show)
module Domain.UnoGame.Events.UnoGameEvents (UnoGameEvent(..), Player(..)) where

import Domain.Common.EventMetadata (ProcessUid, AggregateUid)
import Domain.UnoGame.Models.Player (Player)

data UnoGameEvent = GameStarted ProcessUid AggregateUid [Player] deriving (Eq, Show)
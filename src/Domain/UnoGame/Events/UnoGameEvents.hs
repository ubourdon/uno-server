module Domain.UnoGame.Events.UnoGameEvents (UnoGameEvent(..), Player(..)) where

import Domain.Common.EventMetadata (ProcessUid, AggregateUid)
import Domain.UnoGame.Models.Player (Player)

data UnoGameEvent = GamePrepared ProcessUid AggregateUid [Player] deriving (Eq, Show)
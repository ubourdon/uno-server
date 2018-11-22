module Domain.UnoGame.Events.UnoGameEvents (UnoGameEvent(..), Player(..)) where

import Domain.Common.EventMetadata (ProcessUid, AggregateUid)

data UnoGameEvent = GameStarted ProcessUid AggregateUid [Player] deriving (Eq, Show)

newtype Player = Player Int deriving (Show, Eq)
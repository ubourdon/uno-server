module Domain.UnoGame.Models.UnoGameState (State (..), StateUid(..)) where

import Domain.UnoGame.Events.UnoGameEvents (Player)

import Data.UUID
import Data.List.NonEmpty

data State = EmptyState | State StateUid [Player] deriving (Show, Eq)

newtype StateUid = StateUid UUID deriving (Show, Eq)
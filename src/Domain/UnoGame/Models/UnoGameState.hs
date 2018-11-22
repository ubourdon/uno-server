module Domain.UnoGame.Models.UnoGameState (State (..)) where

import Domain.UnoGame.Events.UnoGameEvents (Player)

import Data.UUID

data State = EmptyState | State UUID [Player] deriving (Show, Eq)
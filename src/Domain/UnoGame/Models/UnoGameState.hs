module Domain.UnoGame.Models.UnoGameState (State (..)) where

import Domain.UnoGame.Events.UnoGameEvents (Player)

data State = EmptyState | State [Player] deriving (Show, Eq)
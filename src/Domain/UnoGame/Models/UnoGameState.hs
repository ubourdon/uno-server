module Domain.UnoGame.Models.UnoGameState (State (..), StateUid(..)) where

import Domain.UnoGame.Models.Player (Player)
--import Domain.UnoGame.Models.Cards (Card)

import Data.UUID

data State = EmptyState | State StateUid [Player] deriving (Show, Eq)

newtype StateUid = StateUid UUID deriving (Show, Eq)

--type LastCardPlayed = Card
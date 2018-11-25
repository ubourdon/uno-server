module Domain.UnoGame.Models.UnoGameState (State (..), StateUid(..), LastPlayer(..)) where

import Domain.UnoGame.Models.Player (Player, PlayerUid)
--import Domain.UnoGame.Models.Cards (Card)

import Data.UUID

data State = EmptyState | State StateUid [Player] LastPlayer deriving (Show, Eq)

newtype StateUid = StateUid UUID deriving (Show, Eq)

newtype LastPlayer = LastPlayer Player deriving (Show, Eq)

--type LastCardPlayed = Card
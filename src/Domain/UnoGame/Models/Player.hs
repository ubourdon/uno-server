module Domain.UnoGame.Models.Player (Player(..), PlayerUid(..), PlayerName(..), PlayerPosition(..)) where

import Data.UUID
import Data.Text

data Player = Player PlayerUid PlayerName PlayerPosition deriving (Show, Eq)

newtype PlayerUid = PlayerUid UUID deriving (Show, Eq)
newtype PlayerName = PlayerName Text deriving (Show, Eq)
newtype PlayerPosition = PlayerPosition Int deriving (Show, Eq)
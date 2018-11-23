module Domain.UnoGame.Models.Player (Player(..), PlayerUid(..), PlayerName(..)) where

import Data.UUID
import Data.Text

data Player = Player PlayerUid PlayerName deriving (Show, Eq)

newtype PlayerUid = PlayerUid UUID deriving (Show, Eq)
newtype PlayerName = PlayerName Text deriving (Show, Eq)

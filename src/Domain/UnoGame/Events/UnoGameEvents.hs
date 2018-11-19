module Domain.UnoGame.Events.UnoGameEvents (UnoGameEvent(..), Player(..)) where

data UnoGameEvent = GameStarted [Player] deriving (Eq, Show)

newtype Player = Player Int deriving (Show, Eq)
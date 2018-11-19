module Domain.UnoGame.Events.UnoGameEvents (UnoGameEvent, Player) where

data UnoGameEvent = GameStarted [Player]

newtype Player = Player Int
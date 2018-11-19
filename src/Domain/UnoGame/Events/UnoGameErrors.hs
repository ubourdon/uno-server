module Domain.UnoGame.Events.UnoGameErrors(UnoGameError(..)) where

data UnoGameError = GameIsAlreadyStarted deriving (Show, Eq)

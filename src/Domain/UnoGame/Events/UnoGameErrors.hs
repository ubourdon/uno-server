module Domain.UnoGame.Events.UnoGameErrors(UnoGameError(..)) where

data UnoGameError = GameIsAlreadyPrepared deriving (Show, Eq)

module Domain.UnoGame.Commands.UnoGameCommands (UnoGameCommand(..), Player) where

import Domain.UnoGame.Events.UnoGameEvents (Player)

data UnoGameCommand = StartGame [Player]
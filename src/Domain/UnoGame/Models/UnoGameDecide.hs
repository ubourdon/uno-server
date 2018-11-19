--module Application.Controllers.UsersController (UsersAPI, usersController, genAuthServerContext) where
module Domain.UnoGame.Models.UnoGameDecide (decide) where

import Domain.UnoGame.Commands.UnoGameCommands (UnoGameCommand)
import Domain.UnoGame.Events.UnoGameEvents (UnoGameEvent)
import Domain.UnoGame.Models.UnoGameState (State)

decide :: UnoGameCommand -> State -> [UnoGameEvent]
decide command state = []
{-# LANGUAGE AllowAmbiguousTypes #-}


module Domain.UnoGame.UnoGameSpec where

import Test.Hspec
import Utils.EventsourcingTestFramework (_Given, _When, _Then)
import Domain.UnoGame.Commands.UnoGameCommands (UnoGameCommand(..))
import Domain.UnoGame.Events.UnoGameEvents (UnoGameEvent(..), Player(..))
import Domain.UnoGame.Models.UnoGameState (State(..))
import Domain.UnoGame.Events.UnoGameErrors (UnoGameError(GameIsAlreadyStarted))
import Domain.UnoGame.DomainEventModule
import Data.Either
import Utils.Thrush ((|>))

spec :: Spec
spec = do
  describe "Should start the game" $ do
    it "Given EmptyState, When try to StartGame, Then should GameStarted" $ do
      _Given [] |> _When _StartGame |> _Then _ExpectedGameStarted

    it "Given an already  started game, When try to StartGame, Then should return []" $ do
      _Given [_GameStarted] |> _When _StartGame |> _Then _GameIsAlreadyStarted

_GameStarted = GameStarted [Player 0]
_ExpectedGameStarted = (Right [_GameStarted]) :: Either UnoGameError [UnoGameEvent]

_StartGame = StartGame [Player 0]

_GameIsAlreadyStarted = (Left GameIsAlreadyStarted) :: Either UnoGameError [UnoGameEvent]
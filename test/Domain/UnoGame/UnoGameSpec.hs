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
import qualified Data.UUID.V4 as SafeUUID (nextRandom)
import Data.UUID
import Domain.Common.EventMetadata (ProcessUid(..), AggregateUid(..))
import Utils.Thrush ((|>))

spec :: Spec
spec = do
  describe "Should start the game" $ do
    it "Given EmptyState, When try to StartGame, Then should GameStarted" $ do
      pUid <- fmap (\uid -> ProcessUid uid) SafeUUID.nextRandom
      aUid <- fmap (\uid -> AggregateUid uid) SafeUUID.nextRandom
      _Given [] |> _When (_StartGame pUid aUid) |> _Then (_ExpectedGameStarted pUid aUid)

    it "Given an already  started game, When try to StartGame, Then should return GameIsAlreadyStarted error" $ do
      pUid <- fmap (\uid -> ProcessUid uid) SafeUUID.nextRandom
      aUid <- fmap (\uid -> AggregateUid uid) SafeUUID.nextRandom
      _Given [_GameStarted pUid aUid] |> _When (_StartGame pUid aUid) |> _Then _GameIsAlreadyStarted


_GameStarted pUid aUid = GameStarted pUid aUid [Player 0]
_ExpectedGameStarted pUid aUid = (Right [_GameStarted pUid aUid]) :: Either UnoGameError [UnoGameEvent]

_StartGame pUid aUid = StartGame pUid aUid [Player 0]

_GameIsAlreadyStarted = (Left GameIsAlreadyStarted) :: Either UnoGameError [UnoGameEvent]
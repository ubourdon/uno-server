{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE OverloadedStrings #-}

module Domain.UnoGame.UnoGameSpec where

import Test.Hspec
import Utils.EventsourcingTestFramework (_Given, _When, _Then)
import Domain.UnoGame.Commands.UnoGameCommands (UnoGameCommand(..))
import Domain.UnoGame.Events.UnoGameEvents (UnoGameEvent(..))
import Domain.UnoGame.Models.Player (Player(..), PlayerUid(..), PlayerName(..), PlayerPosition(..))
import Domain.UnoGame.Models.UnoGameState (State(..))
import Domain.UnoGame.Events.UnoGameErrors (UnoGameError(GameIsAlreadyPrepared))
import Domain.UnoGame.DomainEventModule
import Data.Either
import qualified Data.UUID.V4 as SafeUUID (nextRandom)
import Data.UUID
import Domain.Common.EventMetadata (ProcessUid(..), AggregateUid(..))
import Utils.Thrush ((|>))

spec :: Spec
spec = do
  describe "Should start the game" $ do
    it "Given EmptyState, When try to PrepareGame, Then should GamePrepared" $ do
      testData <- initTestData
      let _PrepareGame = prepareGame testData
      let _ExpectedGamePrepared = expectedGamePrepared testData

      _Given [] |> _When (_PrepareGame) |> _Then (_ExpectedGamePrepared)


    it "Given an already prepared game, When try to PrepareGame, Then should return GameIsAlreadyPrepared error" $ do
      testData <- initTestData
      let _GamePrepared = gamePrepared testData
      let _PrepareGame = prepareGame testData
      let _GameIsAlreadyPrepared = gameIsAlreadyPrepared testData

      _Given [_GamePrepared] |> _When (_PrepareGame) |> _Then _GameIsAlreadyPrepared

    {-
       GamePrepared FirstPlayerUid
       GameStarted Card

       CardPlayed Card

       DirectionChanged
       ColorChanged Color
       WrongCardPlayed Card
       CardPlayedAtWrongTurn Card
       PenaltyGaveToPlayer PlayerUid (NonEmpty Card)
    -}


-- TODO il faut plutot le faire pour chaque données, plutot que de toutes les regrouper (car plusieurs cas de test)
-- TODO ex: _StartGame <- startGame
data InitialDataForTest = InitData {
  prepareGame :: UnoGameCommand
  , gamePrepared :: UnoGameEvent
  , expectedGamePrepared :: Either UnoGameError [UnoGameEvent]
  , gameIsAlreadyPrepared :: Either UnoGameError [UnoGameEvent]
  } deriving (Show)

initTestData :: IO InitialDataForTest
initTestData = do
  pUid <- fmap (\uid -> ProcessUid uid) SafeUUID.nextRandom
  aUid <- fmap (\uid -> AggregateUid uid) SafeUUID.nextRandom
  playerUid <- fmap (\uid -> PlayerUid uid) SafeUUID.nextRandom
  return InitData {
    prepareGame = (PrepareGame pUid aUid [buildPlayer playerUid])
    , gamePrepared = (buildGamePrepared pUid aUid playerUid)
    , expectedGamePrepared = (Right [buildGamePrepared pUid aUid playerUid]) :: Either UnoGameError [UnoGameEvent]
    , gameIsAlreadyPrepared = (Left GameIsAlreadyPrepared) :: Either UnoGameError [UnoGameEvent]
  }

buildGamePrepared pUid aUid playerUid = GamePrepared pUid aUid [buildPlayer playerUid]
buildPlayer playerUid = Player playerUid (PlayerName "toto") (PlayerPosition 0)
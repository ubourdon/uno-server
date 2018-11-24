{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE OverloadedStrings #-}

module Domain.UnoGame.UnoGameSpec where

import Test.Hspec
import Utils.EventsourcingTestFramework (_Given, _When, _Then)
import Domain.UnoGame.Commands.UnoGameCommands (UnoGameCommand(..))
import Domain.UnoGame.Events.UnoGameEvents (UnoGameEvent(..))
import Domain.UnoGame.Models.Player (Player(..), PlayerUid(..), PlayerName(..), PlayerPosition(..))
import Domain.UnoGame.Events.UnoGameErrors (UnoGameError(GameIsAlreadyStarted))
import qualified Data.UUID.V4 as SafeUUID (nextRandom)
import Domain.UnoGame.Models.Cards (Card(NumericCard), Color(Red), ValueCard(Zero))
import Data.List.NonEmpty (NonEmpty((:|)))
import Domain.Common.EventMetadata (ProcessUid(..), AggregateUid(..))
import Utils.Thrush ((|>))

spec :: Spec
spec = do
  describe "Should start the game" $ do
    it "Given EmptyState, When try to StartGame, Then should GameStarted" $ do
      testData <- initTestData
      let _StartGame = startGame testData
      let _ExpectedGameStarted = expectedGameStarted testData

      _Given [] |> _When (_StartGame) |> _Then (_ExpectedGameStarted)


    it "Given an already started game, When try to StartGame, Then should return GameIsAlreadyStarted error" $ do
      testData <- initTestData
      let _GameStarted = gameStarted testData
      let _StartGame = startGame testData
      let _GameIsAlreadyStarted = gameIsAlreadyStarted testData

      _Given [_GameStarted] |> _When (_StartGame) |> _Then _GameIsAlreadyStarted

    {-
       GameStarted FirstCard CardPackage

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
  startGame :: UnoGameCommand
  , gameStarted :: UnoGameEvent
  , expectedGameStarted :: Either UnoGameError [UnoGameEvent]
  , gameIsAlreadyStarted :: Either UnoGameError [UnoGameEvent]
  } deriving (Show)

initTestData :: IO InitialDataForTest
initTestData = do
  pUid <- fmap (\uid -> ProcessUid uid) SafeUUID.nextRandom
  aUid <- fmap (\uid -> AggregateUid uid) SafeUUID.nextRandom
  playerUid <- fmap (\uid -> PlayerUid uid) SafeUUID.nextRandom
  return InitData {
    startGame = (StartGame pUid aUid [buildPlayer playerUid] cardPackage)
    , gameStarted = (buildGameStarted pUid aUid playerUid)
    , expectedGameStarted = (Right [buildGameStarted pUid aUid playerUid]) :: Either UnoGameError [UnoGameEvent]
    , gameIsAlreadyStarted = (Left GameIsAlreadyStarted) :: Either UnoGameError [UnoGameEvent]
  }

buildGameStarted pUid aUid playerUid = GameStarted pUid aUid [buildPlayer playerUid] card cardPackage
buildPlayer playerUid = Player playerUid (PlayerName "toto") (PlayerPosition 0)

cardPackage :: NonEmpty Card
cardPackage = (card) :| []

card = NumericCard Zero Red
{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Domain.UnoGame.UnoGameSpec where

import Test.Hspec
import Utils.EventsourcingTestFramework (_Given, _When, _Then)
import Domain.UnoGame.Commands.UnoGameCommands (UnoGameCommand(..))
import Domain.UnoGame.Events.UnoGameEvents (UnoGameEvent(..))
import Domain.UnoGame.Models.Player (Player(..), PlayerUid(..), PlayerName(..), PlayerPosition(..))
import Domain.UnoGame.Events.UnoGameErrors (UnoGameError(GameIsAlreadyStarted))
import qualified Data.UUID.V4 as SafeUUID (nextRandom)
import Domain.UnoGame.Models.Cards (Card(NumericCard), Color(Red, Blue), ValueCard(Zero, One))
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

      _Given [] |> _When _StartGame |> _Then _ExpectedGameStarted


    it "Given an already started game, When try to StartGame, Then should return GameIsAlreadyStarted error" $ do
      testData <- initTestData
      let _GameStarted = gameStarted_1 testData
      let _StartGame = startGame testData
      let _GameIsAlreadyStarted = gameIsAlreadyStarted testData

      _Given [_GameStarted] |> _When _StartGame |> _Then _GameIsAlreadyStarted

    -- TODO check position are coherent (start at 0, follow each one, no gap)
    -- TODO NonEmpty Player instead of [Player] -- même 2 minimum

  describe "Should play a card" $ do
    it "Given a started game, When try to play a card, Then should CardPlayed" $ do
      testData <- initDataForPlayCard
      let _GameStarted = gameStarted_2 testData
      let _PlayCard = playCard_2 testData
      let _ExpectedCardPlayed = expectedCardPlayed_2 testData

      _Given [_GameStarted] |> _When _PlayCard |> _Then _ExpectedCardPlayed

    it "Given a started game, When try to play a card, But player play at wrong turn, Then should return [CardPlayedAtWrongTurn, PenaltyGaveToPlayer]" $ do
      testData <- initDataForPlayCard
      let _GameStarted = gameStarted_2 testData
      let _PlayCard = playCardAtWrongTurn testData
      let _CardPlayedAtWrongTurn = expectedCardPlayedAtWrongTurn testData

      _Given [_GameStarted] |> _When _PlayCard |> _Then _CardPlayedAtWrongTurn
     -- TODO le player dans la commande existe-t-il
     -- TODO les player ne sont pas rangé dans "l'ordre" dans gameStarted

    -- TODO si ce n'est pas la bonne carte                      => Event
    -- TODO la carte n'est pas disponible dans le jeu du joueur => ERROR
    {-
       GameStarted FirstCard CardPackage              DONE

       CardPlayed PlayerUid Card
       CardPlayedAtWrongTurn PlayerUid Card
       WrongCardPlayed PlayerUid Card

       DirectionChanged
       ColorChanged Color
       PenaltyGaveToPlayer PlayerUid (NonEmpty Card)
    -}

-- TODO il faut plutot le faire pour chaque données, plutot que de toutes les regrouper (car plusieurs cas de test)
-- TODO ex: _StartGame <- startGame
data InitialDataForTest = InitData {
  startGame :: UnoGameCommand
  , playCard_1 :: UnoGameCommand
  , gameStarted_1 :: UnoGameEvent
  , expectedGameStarted :: Either UnoGameError [UnoGameEvent]
  , expectedCardPlayed :: Either UnoGameError [UnoGameEvent]
  , gameIsAlreadyStarted :: Either UnoGameError [UnoGameEvent]
  } deriving (Show)

initTestData :: IO InitialDataForTest
initTestData = do
  pUid <- fmap (\uid -> ProcessUid uid) SafeUUID.nextRandom
  aUid <- fmap (\uid -> AggregateUid uid) SafeUUID.nextRandom
  playerUid <- fmap (\uid -> PlayerUid uid) SafeUUID.nextRandom
  return InitData {
    startGame = StartGame pUid aUid [uniqPlayer playerUid] cardPackage
    , playCard_1 = PlayCard pUid aUid playerUid card
    , gameStarted_1 = buildGameStarted pUid aUid [uniqPlayer playerUid]
    , expectedGameStarted = (Right [buildGameStarted pUid aUid [uniqPlayer playerUid]]) :: Either UnoGameError [UnoGameEvent]
    , expectedCardPlayed = (Right [buildCardPlayed pUid aUid playerUid]) :: Either UnoGameError [UnoGameEvent]
    , gameIsAlreadyStarted = (Left GameIsAlreadyStarted) :: Either UnoGameError [UnoGameEvent]
  }

data DataForPlayCard = DataForPlayCard {
  gameStarted_2 :: UnoGameEvent
  , playCard_2 :: UnoGameCommand
  , playCardAtWrongTurn :: UnoGameCommand
  , expectedCardPlayed_2 :: Either UnoGameError [UnoGameEvent]
  , expectedCardPlayedAtWrongTurn :: Either UnoGameError [UnoGameEvent]
  --, penaltyGaveToPlayer :: UnoGameEvent
}

initDataForPlayCard :: IO DataForPlayCard
initDataForPlayCard = do
  pUid <- fmap (\uid -> ProcessUid uid) SafeUUID.nextRandom
  aUid <- fmap (\uid -> AggregateUid uid) SafeUUID.nextRandom
  playerUid_1 <- fmap (\uid -> PlayerUid uid) SafeUUID.nextRandom
  playerUid_2 <- fmap (\uid -> PlayerUid uid) SafeUUID.nextRandom
  return DataForPlayCard {
      gameStarted_2 =
        let player1 = player playerUid_1 "toto" 0
            player2 = player playerUid_2 "titi" 1
        in buildGameStarted pUid aUid [player1, player2]
      , playCard_2 = PlayCard pUid aUid playerUid_2 card
      , playCardAtWrongTurn = PlayCard pUid aUid playerUid_1 card
      , expectedCardPlayed_2 = (Right [CardPlayed pUid aUid playerUid_2 card]) :: Either UnoGameError [UnoGameEvent]
      , expectedCardPlayedAtWrongTurn = (Right [CardPlayedAtWrongTurn pUid aUid playerUid_1 card]) :: Either UnoGameError [UnoGameEvent]
    }


buildGameStarted pUid aUid players = GameStarted pUid aUid players card cardPackage
buildCardPlayed pUid aUid playerUid = CardPlayed pUid aUid playerUid card

uniqPlayer playerUid = Player playerUid (PlayerName "toto") (PlayerPosition 0)
player uid name position = Player uid (PlayerName name) (PlayerPosition position)


cardPackage :: NonEmpty Card
cardPackage = (card) :| []

card = NumericCard Zero Red
otherCard = NumericCard One Blue
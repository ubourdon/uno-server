{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE DuplicateRecordFields #-}

module Domain.UnoGame.UnoGameTestData (InitialDataForTest(..), DataForPlayCard(..), initDataForPlayCard, initTestData) where

import Domain.UnoGame.Commands.UnoGameCommands (UnoGameCommand(..))
import Domain.UnoGame.Events.UnoGameEvents (UnoGameEvent(..))
import Domain.UnoGame.Models.Player (Player(..), PlayerUid(..), PlayerName(..), PlayerPosition(..))
import Domain.UnoGame.Events.UnoGameErrors (UnoGameError(GameIsAlreadyStarted))
import qualified Data.UUID.V4 as SafeUUID (nextRandom)
import Domain.UnoGame.Models.Cards (Card(NumericCard), Color(Red, Blue, Green), ValueCard(Zero, One, Two))
import Data.List.NonEmpty (NonEmpty((:|)))
import Domain.Common.EventMetadata (ProcessUid(..), AggregateUid(..))
import qualified Data.List.NonEmpty as NEL
import Utils.Thrush ((|>))

-- TODO il faut plutot le faire pour chaque données, plutot que de toutes les regrouper (car plusieurs cas de test)
-- TODO ex: _StartGame <- startGame
data InitialDataForTest = InitData {
  startGame :: UnoGameCommand
  , playCard :: UnoGameCommand
  , gameStarted :: UnoGameEvent
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
    startGame = StartGame pUid aUid (NEL.fromList [uniqPlayer playerUid]) twoCardPackage
    , playCard = PlayCard pUid aUid playerUid zeroRedCard
    , gameStarted = buildGameStarted pUid aUid (NEL.fromList [uniqPlayer playerUid]) twoCardPackage
    , expectedGameStarted = (Right [buildGameStarted pUid aUid (NEL.fromList [uniqPlayer playerUid]) twoCardPackage]) :: Either UnoGameError [UnoGameEvent]
    , expectedCardPlayed = (Right [buildCardPlayed pUid aUid playerUid]) :: Either UnoGameError [UnoGameEvent]
    , gameIsAlreadyStarted = (Left GameIsAlreadyStarted) :: Either UnoGameError [UnoGameEvent]
  }

data DataForPlayCard = DataForPlayCard {
  gameStarted :: UnoGameEvent
  , playCard :: UnoGameCommand
  , playCardAtWrongTurn :: UnoGameCommand
  , expectedCardPlayed :: Either UnoGameError [UnoGameEvent]
  , expectedCardPlayedAtWrongTurn :: UnoGameEvent
  , expectedPenaltyGaveToPlayer :: UnoGameEvent
} deriving (Show)

initDataForPlayCard :: IO DataForPlayCard
initDataForPlayCard = do
  pUid <- fmap (\uid -> ProcessUid uid) SafeUUID.nextRandom
  aUid <- fmap (\uid -> AggregateUid uid) SafeUUID.nextRandom
  playerUid_1 <- fmap (\uid -> PlayerUid uid) SafeUUID.nextRandom
  playerUid_2 <- fmap (\uid -> PlayerUid uid) SafeUUID.nextRandom
  return DataForPlayCard {
      gameStarted =
        let player1 = player playerUid_1 "toto" 0
            player2 = player playerUid_2 "titi" 1
        in buildGameStarted pUid aUid (player1 :| [player2]) _3cardsPackage
      , playCard = PlayCard pUid aUid playerUid_2 zeroRedCard
      , playCardAtWrongTurn = PlayCard pUid aUid playerUid_1 zeroRedCard
      , expectedCardPlayed = (Right [CardPlayed pUid aUid playerUid_2 zeroRedCard]) :: Either UnoGameError [UnoGameEvent]
      , expectedCardPlayedAtWrongTurn = CardPlayedAtWrongTurn pUid aUid playerUid_1 zeroRedCard
      , expectedPenaltyGaveToPlayer = PenaltyGaveToPlayer pUid aUid playerUid_1 (NEL.fromList [oneBlueCard, oneRedCard])
    }


buildGameStarted pUid aUid players cardPackage = GameStarted pUid aUid players (NEL.head cardPackage) cardPackage
buildCardPlayed pUid aUid playerUid = CardPlayed pUid aUid playerUid zeroRedCard

uniqPlayer playerUid = Player playerUid (PlayerName "toto") (PlayerPosition 0)
player uid name position = Player uid (PlayerName name) (PlayerPosition position)

twoCardPackage = NEL.fromList [zeroRedCard, oneBlueCard]
_3cardsPackage = NEL.fromList [zeroRedCard, oneBlueCard, oneRedCard]

zeroRedCard = NumericCard Zero Red
oneBlueCard = NumericCard One Blue
oneRedCard = NumericCard One Red
twoGreenCard = NumericCard Two Green

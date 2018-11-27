{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE ScopedTypeVariables #-}


module Domain.UnoGame.UnoGameSpec where

import Test.Hspec
import Utils.EventsourcingTestFramework (_Given, _When, _Then)
import Domain.UnoGame.UnoGameTestData
import Domain.UnoGame.Events.UnoGameEvents
import Domain.UnoGame.Events.UnoGameErrors
import Utils.Thrush ((|>))


spec :: Spec
spec = do
  {-
    TODO préparer le jeu (donne un identifiant de jeu pour l'insscription) |> _________________________________________
    TODO inscription des joueurs                                           |> _________________________________________
    TODO chaque joueur annonce qu'il est prêt                              |> Peut etre fait par un service/domain tier ?
    TODO Démarrage du jeu (quand au moins 2 joueurs sont inscrit)
      C'est le joueur qui a préparé le jeu qui peut le démarrer
  -}

  describe "Should start the game" $ do
    it "Given EmptyState, When try to StartGame, Then should GameStarted" $ do
      testData <- initTestData
      let _StartGame = startGame (testData :: InitialDataForTest)
      let _ExpectedGameStarted = expectedGameStarted (testData :: InitialDataForTest)

      _Given [] |> _When _StartGame |> _Then _ExpectedGameStarted

    it "Given an already started game, When try to StartGame, Then should return GameIsAlreadyStarted error" $ do
      testData :: InitialDataForTest <- initTestData
      let _GameStarted = gameStarted (testData :: InitialDataForTest)
      let _StartGame = startGame (testData :: InitialDataForTest)
      let _GameIsAlreadyStarted = gameIsAlreadyStarted (testData :: InitialDataForTest)

      _Given [_GameStarted] |> _When _StartGame |> _Then _GameIsAlreadyStarted

    it "Given an EmptyState, When try to StartGame, But Player have wrong order, Then should return CantStartGameBecauseOfWrongOrderPlayer" $ do
      pendingWith "not implemented" -- TODO check position are coherent (start at 0, follow each one, no gap)

    it "Given an EmptyState, When try to StartGame, But have not enouhg player (< 2), Then should return CantStartGameBecauseOfNotEnoughPlayer" $ do
      pendingWith "not implemented" -- TODO NonEmpty Player instead of [Player] -- même 2 minimum


  describe "Should play a card" $ do
    it "Given a started game, When try to play a card, Then should CardPlayed" $ do
      testData <- initDataForPlayCard
      let _GameStarted = gameStarted (testData :: DataForPlayCard)
      let _PlayCard = playCard (testData :: DataForPlayCard)
      let _ExpectedCardPlayed = expectedCardPlayed (testData :: DataForPlayCard)

      _Given [_GameStarted] |> _When _PlayCard |> _Then _ExpectedCardPlayed


  describe "Should play a card at wrong turn" $ do
    it (unlines [
      "Given a started game, ",
      "When try to play a card,",
      "But player play at wrong turn,",
      "Then should return [CardPlayedAtWrongTurn, PenaltyGaveToPlayer]"]) $ do
      testData <- initDataForPlayCard
      let _GameStarted = gameStarted (testData :: DataForPlayCard)
      let _PlayCard = playCardAtWrongTurn (testData :: DataForPlayCard)
      let _CardPlayedAtWrongTurn = expectedCardPlayedAtWrongTurn (testData :: DataForPlayCard)
      let _PenaltyGaveToPlayer = expectedPenaltyGaveToPlayer (testData :: DataForPlayCard)

      _Given [_GameStarted] |>
        _When _PlayCard |>
        _Then (Right [_CardPlayedAtWrongTurn, _PenaltyGaveToPlayer] :: Either UnoGameError [UnoGameEvent])


  describe "Should play a wrong card" $ do
    it "Given a started game, When try to play a card, But a wrong card is played, Then should return [WrongCardPlayed, PenaltyGaveToPlayer]" $ do
      pendingWith "not implemented"


  describe "Should change direction" $ do
    it "kick back" $ do
      pending


  describe "should change color" $ do
    it "change color" $ do
      pending

    it "+4" $ do
      pending


  describe "should gave penalty" $ do
    it "+2" $ do
      pending

  describe "should pass his turn" $ do
    it "stop card" $ do
      pending

  describe "Player in Commadn should exist" $ do
    context "property test, all the command with PlayerUid will be tested" $ do
      it "player referenced in command should exist in Game" $ do
        pendingWith "not implemented"

  -- TODO le joueur pioche

  -- TODO si ce n'est pas la bonne carte                      => Event
  -- TODO la carte n'est pas disponible dans le jeu du joueur => ERROR

  -- TODO gérer le cas ou de deck est vide => remettre les cartes jouées (mélangées)
  
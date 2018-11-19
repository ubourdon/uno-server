module Domain.UnoGame.UnoGameSpec where 

import Test.Hspec
import Domain.UnoGame.Commands.UnoGameCommands (UnoGameCommand(..))
import Domain.UnoGame.Events.UnoGameEvents (UnoGameEvent(..), Player(..))
import Domain.UnoGame.Models.UnoGameApply  (apply)
import Domain.UnoGame.Models.UnoGameDecide (decide)
import Domain.UnoGame.Models.UnoGameState (State(..))
import Domain.UnoGame.Events.UnoGameErrors (UnoGameError(..))
import Data.Either
import Utils.Thrush ((|>))

spec :: Spec
spec = do
  describe "Should start the game" $ do
    it "Given EmptyState, When try to StartGame, Then should GameStarted" $ do
      _Given [] |> _When _StartGame |> _Then [_GameStarted]

    it "Given an already  started game, When try to StartGame, Then should return []" $ do
      _Given [_GameStarted] |> _When _StartGame |> _ThenError GameIsAlreadyStarted

_GameStarted = GameStarted [Player 0]
_StartGame = StartGame [Player 0]


newtype ExpectedEvents = ExpEvt [UnoGameEvent]
newtype InitialState = IState [UnoGameEvent]
type ExpectedError = UnoGameError


_Given :: [UnoGameEvent] -> InitialState
_Given = IState

_When :: UnoGameCommand -> InitialState -> (UnoGameCommand, InitialState)
_When x y = (x,y)

_Then :: [UnoGameEvent] -> (UnoGameCommand, InitialState) -> IO ()
_Then expecteds (cmd, istate) = performPredicate cmd istate (ExpEvt expecteds)

_ThenError :: ExpectedError -> (UnoGameCommand, InitialState) -> IO ()
_ThenError expectedError (cmd, istate) = performPredicateError cmd istate expectedError

performPredicate :: UnoGameCommand -> InitialState -> ExpectedEvents -> IO ()
performPredicate cmd (IState evts) (ExpEvt expEvts) = (decide (actualState EmptyState evts) cmd) `shouldBe` Right expEvts

performPredicateError :: UnoGameCommand -> InitialState -> ExpectedError -> IO ()
performPredicateError cmd (IState evts) expError = (decide (actualState EmptyState evts) cmd) `shouldBe` Left expError

actualState :: State -> [UnoGameEvent] -> State
actualState state evts = foldl (\currentState event -> apply currentState event ) state evts
{-
  package utils

  import domain.DomainEventTypeclass
  import domain.service.ApplyTo
  import org.scalatest.{Matchers, FunSuite}

  trait EventsourcingTestingFramework[Event, Command, State, Error] extends FunSuite with Matchers with ApplyTo {
  	def Given(events: List[Event]): List[Event] = events
  	def Given(events: Event*): List[Event] = events.toList

  	def When(command: Command)(events: List[Event]) = (command, events)

  	def Then(expected: List[Event])(when: (Command, List[Event]))(implicit de: DomainEventTypeclass.Aux[Event, State, Command, Error]) =
  		specify(when._1,when._2, expected)

  	def Then(expected: Event*)(when: (Command, List[Event]))(implicit de: DomainEventTypeclass.Aux[Event, State, Command, Error]) =
  		specify(when._1,when._2, expected.toList)

  	def Then(expected: Error)(when: (Command, List[Event]))(implicit de: DomainEventTypeclass.Aux[Event, State, Command, Error]) =
  		specifyError(when._1,when._2, expected)

  	private def specify(command: Command, givenEvents: List[Event], expectedEvents: List[Event])
  	                   (implicit de: DomainEventTypeclass.Aux[Event, State, Command, Error]) = {
  		de.decide(applyTo(de.emptyState)(givenEvents), command).fold(
  			error   => fail(s"expected Event but have error: ${error.toString}"),
  			events  => events shouldBe expectedEvents
  		)
  	}

  	private def specifyError(command: Command, givenEvents: List[Event], expectedError: Error)
  	                        (implicit de: DomainEventTypeclass.Aux[Event, State, Command, Error]) = {
  		de.decide(applyTo(de.emptyState)(givenEvents), command).fold(
  			error   => error shouldBe expectedError,
  			events  => fail(s"should be an error, not Events : $events")
  		)
  	}
  }
-}
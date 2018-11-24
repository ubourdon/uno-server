{-# LANGUAGE AllowAmbiguousTypes #-}

module Utils.EventsourcingTestFramework (_Given, _When, _Then) where

import Domain.UnoGame.DomainEventModule (DomainEventApply(apply), DomainEventDecide(decide), DomainEventEmptyState(_EmptyState))
import Test.Hspec

newtype ExpectedEvents evt = ExpEvt [evt]
newtype InitialState evt = IState [evt]

_Given :: [event] -> InitialState event
_Given = IState

_When :: command -> InitialState event -> (command, InitialState event)
_When cmd istate = (cmd, istate)

_Then :: (DomainEventDecide state command error event, DomainEventApply event state, DomainEventEmptyState state, Show event, Show error, Eq event, Eq error) =>
          Either error [event] -> (command, InitialState event) -> IO ()
_Then expected (cmd, istate) = performPredicate _EmptyState cmd istate expResult
  where expResult = (fmap (\evt -> ExpEvt evt) expected)

performPredicate :: (DomainEventDecide state command error event, DomainEventApply event state, Show event, Show error, Eq event, Eq error) =>
                     state -> command -> InitialState event -> Either error (ExpectedEvents event) -> IO ()
performPredicate emptystate cmd (IState evts) expResult = assertEqual result expResult
  where result = decide currentState cmd
        currentState = calculCurrentState emptystate evts

assertEqual :: (Show event, Show error, Eq event, Eq error) => Either error [event] -> Either error (ExpectedEvents event) -> IO ()
assertEqual (Right result) (Right (ExpEvt expected)) = result `shouldBe` expected
assertEqual (Left err) (Left expected) = err `shouldBe` expected
assertEqual _ _ = True `shouldBe` False

calculCurrentState :: (DomainEventApply event state) => state -> [event] -> state
calculCurrentState istate evts = foldl (\currentState event -> apply currentState event ) istate evts
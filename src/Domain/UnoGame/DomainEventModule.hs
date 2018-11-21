{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE FunctionalDependencies #-}

module Domain.UnoGame.DomainEventModule (DomainEventApply(..), DomainEventDecide(..), DomainEventEmptyState(..)) where

import Domain.UnoGame.Events.UnoGameEvents (UnoGameEvent)
import Domain.UnoGame.Commands.UnoGameCommands (UnoGameCommand)
import Domain.UnoGame.Models.UnoGameState (State(..))
import Domain.UnoGame.Events.UnoGameErrors (UnoGameError)
import qualified Domain.UnoGame.Models.UnoGameApply as UnoGameApply (apply)
import qualified Domain.UnoGame.Models.UnoGameDecide as UnoGameDecide (decide)

--https://stackoverflow.com/questions/20040224/functional-dependencies-in-haskell
class DomainEventApply event state | event -> state where
  apply :: state -> event -> state

instance DomainEventApply UnoGameEvent State where
  apply = UnoGameApply.apply


class DomainEventDecide state command error event where
  decide :: state -> command -> Either error [event]

instance DomainEventDecide State UnoGameCommand UnoGameError UnoGameEvent where
  decide = UnoGameDecide.decide


class DomainEventEmptyState state where
  _EmptyState :: state

instance DomainEventEmptyState State where
  _EmptyState = EmptyState
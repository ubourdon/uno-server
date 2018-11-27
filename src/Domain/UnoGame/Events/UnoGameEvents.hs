module Domain.UnoGame.Events.UnoGameEvents (UnoGameEvent(..), FirstCardFromDeck, CardsPackage) where

import Domain.Common.EventMetadata (ProcessUid, AggregateUid)
import Domain.UnoGame.Models.Player (Player, PlayerUid)
import Domain.UnoGame.Models.Cards (Card)
import Data.List.NonEmpty

data UnoGameEvent = 
  GameStarted ProcessUid AggregateUid (NonEmpty Player) FirstCardFromDeck CardsPackage |

  CardPlayed ProcessUid AggregateUid PlayerUid Card |
  CardPlayedAtWrongTurn ProcessUid AggregateUid PlayerUid Card |

  PenaltyGaveToPlayer ProcessUid AggregateUid PlayerUid (NonEmpty Card)

  deriving (Eq, Show)

-- All the Cards available at the start of the game
type CardsPackage = NonEmpty Card

-- first card display from the deck to start tha game
type FirstCardFromDeck = Card

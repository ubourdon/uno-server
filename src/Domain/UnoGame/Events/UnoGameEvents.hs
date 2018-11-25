module Domain.UnoGame.Events.UnoGameEvents (UnoGameEvent(..), FirstCardFromDeck) where

import Domain.Common.EventMetadata (ProcessUid, AggregateUid)
import Domain.UnoGame.Models.Player (Player, PlayerUid)
import Domain.UnoGame.Models.Cards (Card, CardsPackage)

data UnoGameEvent = 
  GameStarted ProcessUid AggregateUid [Player] FirstCardFromDeck CardsPackage |

  CardPlayed ProcessUid AggregateUid PlayerUid Card |
  CardPlayedAtWrongTurn ProcessUid AggregateUid PlayerUid Card

  deriving (Eq, Show)

-- first card display from the deck to start tha game
type FirstCardFromDeck = Card
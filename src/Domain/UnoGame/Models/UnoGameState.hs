module Domain.UnoGame.Models.UnoGameState (State (..), StateUid(..), LastPlayer(..)) where

import Domain.UnoGame.Models.Player (Player, PlayerUid)
import Data.List.NonEmpty
import Domain.UnoGame.Models.Cards (Card)

import Data.UUID

data State = EmptyState | State StateUid (NonEmpty Player) LastPlayer CardsDeck deriving (Show, Eq)

-- Cards available in the Deck at a T time
-- NonEmpty car quand le deck est "vide" on peut le re-remplir avec les cartes jouées
type CardsDeck = NonEmpty Card

newtype StateUid = StateUid UUID deriving (Show, Eq)

newtype LastPlayer = LastPlayer Player deriving (Show, Eq)

--type LastCardPlayed = Card
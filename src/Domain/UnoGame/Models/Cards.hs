module Domain.UnoGame.Models.Cards (CardsDeck, CardsPackage, Card(..), ValueCard(..), Color(..)) where

import Data.List.NonEmpty

-- All the Cards available at the start of the game
type CardsPackage = NonEmpty Card
-- Cards available in the Deck at a T time
type CardsDeck = [Card]

data Card =
  NumericCard ValueCard Color |
  StopCard Color |
  KickBack Color |
  PlusTwo Color |
  PlusFour |
  ChangeColor deriving (Eq, Show)

-- Eq means can be compared with (==) & (/=]
-- Ord means can be compared with <, <=, ...
-- Enum means can be converted to and from Int with fromEnum & toEnum
-- Show means can be converted to String
data ValueCard = Zero | One | Two | Three | Four | Five | Six | Seven | Height | Nine deriving (Eq, Ord, Enum, Show)

data Color = Red | Green | Blue | Yellow deriving (Eq, Show)
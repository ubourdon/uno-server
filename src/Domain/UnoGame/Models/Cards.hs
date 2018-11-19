module Domain.UnoGame.Models.Cards where

import Data.Ord

data Cards =
  NumericCard ValueCard Color |
  StopCard Color |
  Stepback Color |
  PlusTwo Color |
  PlusFour |
  ChangeColor

-- Eq means can be compared with (==) & (/=]
-- Ord means can be compared with <, <=, ...
-- Enum means can be converted to and from Int with fromEnum & toEnum
-- Show means can be converted to String
data ValueCard = Zero | One | Two | Three | Four | Five | Six | Seven | Height | Nine deriving (Eq, Ord, Enum, Show)

data Color = Red | Green | Blue | Yellow deriving (Eq, Show)
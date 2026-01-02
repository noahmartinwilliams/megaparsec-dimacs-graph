module Text.Megaparsec.DIMACS.Coords.Types where

import Data.Void
import Text.Megaparsec

type CoParser = Parsec Void String

data CoCoord = CoCoord Int Int Int deriving(Show, Eq)

data CoFile = CoFile Int [CoCoord] deriving(Show, Eq)

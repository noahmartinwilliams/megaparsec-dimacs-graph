module Text.Megaparsec.DIMACS.Graph.Types(GrParser, ArcLine(..), ProbLine(..)) where

import Control.Monad
import Data.Void
import Text.Megaparsec
import Text.Megaparsec.Char
import Text.Megaparsec.Char.Lexer

type GrParser = Parsec Void String

data ArcLine = ArcLine Int Int Int deriving(Show, Eq, Ord)

data ProbLine = ProbLine Int Int deriving(Show, Eq)

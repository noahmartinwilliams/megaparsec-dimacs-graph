module Text.Megaparsec.DIMACS.Graph.Types(GrParser, ArcLine(..), ProbLine(..), GrFile(..)) where

import Data.Void
import Text.Megaparsec

type GrParser = Parsec Void String

data ArcLine = ArcLine Int Int Int deriving(Show, Eq, Ord)

data ProbLine = ProbLine Int Int deriving(Show, Eq)

data GrFile = GrFile ProbLine [ArcLine] deriving(Show, Eq)

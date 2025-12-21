module Text.Megaparsec.DIMACS.Graph(module Text.Megaparsec.DIMACS.Graph.Types, grProbLine) where

import Control.Monad
import Text.Megaparsec
import Text.Megaparsec.Char
import Text.Megaparsec.Char.Lexer
import Text.Megaparsec.DIMACS.Graph.Types

grInt :: GrParser Int
grInt = do
    int <- some digitChar
    return (read int :: Int)

grProbLine :: GrParser ProbLine
grProbLine = do
    void $ single 'p'
    void $ hspace1
    void $ string "sp"
    void $ hspace1
    nodes <- grInt
    void $ hspace1
    arcs <- grInt
    void $ hspace
    void $ newline
    return (ProbLine nodes arcs)

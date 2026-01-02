module Text.Megaparsec.DIMACS.Coords where

import Control.Monad
import Data.Maybe
import GHC.Conc
import Text.Megaparsec
import Text.Megaparsec.Char
import Text.Megaparsec.DIMACS.Coords.Types

coInt :: CoParser Int
coInt = do
    neg <- optional (single '-')
    d <- some digitChar
    if isNothing neg
    then
        return (read d :: Int)
    else
        return (- (read d :: Int))

coComment :: CoParser ()
coComment = do
    void $ single 'c'
    void $ many (anySingleBut '\n')
    void $ (try (newline >> return ()) <|> try eof)

coProbLine :: CoParser Int
coProbLine = do
    void $ many coComment
    void $ hspace
    void $ single 'p'
    void $ hspace1
    void $ string "aux"
    void $ hspace1
    void $ string "sp"
    void $ hspace1
    void $ string "co"
    void $ hspace1
    int <- coInt
    void $ hspace
    void $ newline
    return (int)

coCoordLine :: CoParser (Int, Int, Int)
coCoordLine = do
    void $ many coComment
    void $ single 'v'
    void $ hspace1
    id <- coInt
    void $ hspace1
    xcoord <- coInt
    void $ hspace1
    ycoord <- coInt
    void $ (try (newline >> return ()) <|> try eof)
    return (id, xcoord, ycoord)

coFile :: CoParser CoFile
coFile = do
    pl <- coProbLine
    cls <- some coCoordLine 
    void $ many coComment
    let cls' = Prelude.map (\(a, b, c) -> CoCoord a b c) cls 
    return (CoFile pl cls')

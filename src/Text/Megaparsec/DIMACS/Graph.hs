module Text.Megaparsec.DIMACS.Graph(module Text.Megaparsec.DIMACS.Graph.Types, grFile, grSanityCheck) where

import Control.Monad
import Data.List(sort)
import Data.List.Unique
import GHC.Conc
import Text.Megaparsec
import Text.Megaparsec.Char
import Text.Megaparsec.DIMACS.Graph.Types

grInt :: GrParser Int
grInt = do
    int <- some digitChar
    let readInt = read int :: Int
    return (par readInt readInt)

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

grArcLine :: GrParser ArcLine 
grArcLine = do
    void $ many grComment
    void $ single 'a'
    void $ hspace1
    src <- grInt
    void $ hspace1
    dest <- grInt
    void $ hspace1
    weight <- grInt 
    void $ hspace
    void $ (try (newline >> return ()) <|> try eof)
    return (ArcLine src dest weight)

grComment :: GrParser ()
grComment = do
    void $ single 'c'
    void $ many (anySingleBut '\n')
    void $ (try (newline >> return ()) <|> try eof)

grFile :: GrParser GrFile
grFile = do
    void $ many grComment
    prob <- grProbLine
    arcs <- some grArcLine
    void $ many grComment
    return (GrFile prob arcs)

numNodes :: [ArcLine] -> Int
numNodes list = do
    let nodes = Prelude.foldr (++) [] (Prelude.map (\(ArcLine a b _) -> [a, b]) list)
        sorted = sort nodes
        uniqed = uniq sorted
    (Prelude.length uniqed) 

grSanityCheck :: MonadFail m => GrFile -> m ()
grSanityCheck (GrFile (ProbLine _ numArcs) arcs) | (numArcs /= (Prelude.length arcs)) = fail ("Incorrect number of arcs. Expected " ++ (show numArcs) ++ ". Got " ++ (show (Prelude.length arcs)) ++ ".")
grSanityCheck (GrFile (ProbLine nodes _) arcs) | (numNodes arcs) /= nodes = fail ("Incorrect number of nodes. Expected " ++ (show nodes) ++ " Got " ++ (show (numNodes arcs)) ++ ".")
grSanityCheck _ = return ()

-- @+leo-ver=4-thin
-- @+node:gcross.20090223094425.4:@thin NeighborCountStatistics.hs
-- @@language haskell

module Main where

import Data.Array.IArray (elems)
import Data.Graph
import Data.Generics (gmapQ)
import Data.Generics.Text (gshow)
import Data.List
import Text.Printf
import System

import CodeQuest.CodeScan.CodeGraph

intToFloat :: Int -> Float
intToFloat = toEnum

processGraph :: String -> IO ()
processGraph graph_name = do
    putStr graph_name
    putStr " -> "
    let graph = fromGraph6 graph_name
        degrees = zipWith (+) ((elems.indegree) graph) ((elems.outdegree) graph)
        normalizer = (intToFloat.length.vertices) graph
        mean = ((/normalizer).intToFloat.sum) degrees
    putStrLn $ printf "%i %i %.1f %.1f" (minimum degrees) (maximum degrees) mean

main = do
    args <- getArgs
    input <- getContents
    let graphs = if length args > 0
                    then args
                    else lines input
    mapM_ processGraph graphs
-- @-node:gcross.20090223094425.4:@thin NeighborCountStatistics.hs
-- @-leo

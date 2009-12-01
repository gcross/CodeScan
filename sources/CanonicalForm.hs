-- @+leo-ver=4-thin
-- @+node:gcross.20090224112926.3:@thin CanonicalForm.hs
-- @@language haskell

module Main where

import Data.Graph
import Data.Graph.Automorphism
import Data.Graph.Construction
import System

import CodeQuest.CodeScan.CodeGraph

processGraph :: String -> IO ()
processGraph graph_name = do
    (putStrLn.toGraph6.canonicGraph.fromGraph6) graph_name

main = do
    args <- getArgs
    input <- getContents
    let graphs = if length args > 0
                    then args
                    else lines input
    mapM_ processGraph graphs
-- @-node:gcross.20090224112926.3:@thin CanonicalForm.hs
-- @-leo

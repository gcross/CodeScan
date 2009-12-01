-- @+leo-ver=4-thin
-- @+node:gcross.20081228030258.31:@thin CountCombinations.hs
-- @@language haskell

module Main where

import Data.Array.IArray (elems)
import Data.List
import System

import CodeQuest.CodeScan.CodeGraph

processGraph :: String -> IO ()
processGraph graph = do
    putStr graph
    putStr " -> "
    putStrLn $ (show . combinationCount . fromGraph6) graph

main = do
    args <- getArgs
    input <- getContents
    let graphs = if length args > 0
                    then args
                    else lines input
    mapM_ processGraph graphs
-- @-node:gcross.20081228030258.31:@thin CountCombinations.hs
-- @-leo

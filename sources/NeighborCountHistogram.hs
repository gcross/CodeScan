-- @+leo-ver=4-thin
-- @+node:gcross.20090223094425.3:@thin NeighborCountHistogram.hs
-- @@language haskell

{-# LANGUAGE ScopedTypeVariables #-}

module Main where

import Control.Monad
import Data.Function
import Data.Graph
import Data.Array.IO
import Data.Array.IArray
import Data.List
import System
import Text.Printf

import CodeQuest.CodeScan.CodeGraph

processGraph :: String -> IO ()
processGraph graph_name = do
    let graph = fromGraph6 graph_name
        indegrees = indegree graph
        outdegrees = outdegree graph
        degree_of vertex = indegrees!vertex + outdegrees!vertex
        number_of_vertices = (length.vertices)graph
    histogram :: IOUArray Int Int <- newArray (2,number_of_vertices-1) 0
    forM_ [0..number_of_vertices-1] $ \vertex -> do
        let degree = degree_of vertex
        old_count <- readArray histogram degree
        writeArray histogram degree (old_count+1)
    let bins = [2..number_of_vertices-1]
    bin_values <- getElems histogram
    putStrLn $ graph_name ++ " -> "
    putStrLn $ "\t" ++ " " ++ (intercalate " | " (map (printf "%2i") bins))
    putStrLn $ "\t" ++ " " ++ (((take $ (length bins) * 2 + ((length bins)-1) * 3).repeat) '-')
    putStrLn $ "\t" ++ " " ++ (intercalate " | " (map (printf "%2i") bin_values))

main = do
    args <- getArgs
    input <- getContents
    let graphs = if length args > 0
                    then args
                    else lines input
    mapM_ processGraph graphs
-- @-node:gcross.20090223094425.3:@thin NeighborCountHistogram.hs
-- @-leo

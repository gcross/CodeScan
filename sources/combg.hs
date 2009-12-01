-- @+leo-ver=4-thin
-- @+node:gcross.20081230235952.3:@thin combg.hs
-- @@language haskell

module Main where

import CodeQuest.CodeScan.CodeGraph
import Data.Array.IArray (elems)
import Data.List
import System

processGraph test graph = do
    let number_of_combinations = (combinationCount . fromGraph6) graph
    if test number_of_combinations
        then putStrLn graph
        else return ()

main = do
    args <- getArgs
    if length args < 1
        then putStrLn "must specify threshold"
        else return ()
    input <- getContents
    let tester    = if length args == 1 then let max_threshold = (read . head) args
                                             in (\n -> n <= max_threshold)
                                        else let [min_threshold,max_threshold] = map read (take 2 args)
                                             in (\n -> (n > min_threshold) && (n <= max_threshold))
    mapM_ (processGraph tester) (lines input)
-- @-node:gcross.20081230235952.3:@thin combg.hs
-- @-leo

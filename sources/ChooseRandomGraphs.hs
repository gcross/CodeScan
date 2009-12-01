-- @+leo-ver=4-thin
-- @+node:gcross.20090101195553.5:@thin ChooseRandomGraphs.hs
-- @@language Haskell

import qualified Data.ByteString.Char8 as B
import Data.Array.IArray
import System
import System.Random
import Control.Monad
import Data.List (nub,sort)

main = do
    args <- getArgs
    if length args < 1
        then error "must specify number of graphs to select"
        else return ()
    let number_of_outputs = (read . head) args
    input <- B.getContents
    let lines = B.lines input
        number_of_inputs = length lines
    randomized_indices <- forM [1..2*number_of_outputs] $ \_ -> randomRIO (0,number_of_inputs-1)
    let indices_to_select = sort $ ((take number_of_outputs) . nub) randomized_indices
        selectElements _ _ [] = []
        selectElements position list (index:indices) =
            let offset = index-position
                (element_of_interest:remainders) = drop offset list
            in (element_of_interest:selectElements (index+1) remainders indices)  
    mapM_ B.putStrLn $ selectElements 0 lines indices_to_select
-- @-node:gcross.20090101195553.5:@thin ChooseRandomGraphs.hs
-- @-leo

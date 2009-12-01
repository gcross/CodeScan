-- @+leo-ver=4-thin
-- @+node:gcross.20090223094425.2:@thin MakeBunchedCondor.hs
-- @@language haskell

import Control.Monad

import System
import Data.List

import CodeQuest.CodeScan.CodeGraph

bunch :: Integer -> [String] -> [[String]]
bunch _ [] = []
bunch bunch_size graphs =
    let (this_bunch,rest) = computeBunch 0 graphs
    in this_bunch:bunch bunch_size rest
    where
        computeBunch :: Integer -> [String] -> ([String],[String])
        computeBunch _ [] = ([],[])
        computeBunch current_bunch_size graphs =
            if current_bunch_size > bunch_size
                then ([],graphs)
                else
                    let (next_graph:remaining_graphs) = graphs
                        number_of_combinations = (combinationCount.fromGraph6) next_graph
                        new_bunch_size = current_bunch_size + number_of_combinations
                        (rest_bunch,rest_graphs) = computeBunch new_bunch_size remaining_graphs
                    in ((next_graph:rest_bunch),rest_graphs)

main = do
    args <- getArgs
    let bunch_size =
            if length args < 1
                then error "must specify bunch size"
                else (read.head) args
    putStrLn "Executable = ScanCodes"
    putStrLn "Universe = vanilla"
    putStrLn "Error  = logs/err.$(cluster)"
    putStrLn "Output = logs/out.$(cluster)"
    putStrLn "Log    = logs/log.$(cluster)"
    putStrLn ""
    putStrLn "Environment = \"LD_LIBRARY_PATH=/phys/users/gcross/local/lib\""
    putStrLn ""
    input <- getContents
    forM_ (bunch bunch_size (lines input)) $ \this_bunch -> do
        putStrLn $ "Arguments = " ++ (intercalate " " this_bunch)
        putStrLn "Queue"
        putStrLn ""
-- @-node:gcross.20090223094425.2:@thin MakeBunchedCondor.hs
-- @-leo

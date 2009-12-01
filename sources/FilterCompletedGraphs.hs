-- @+leo-ver=4-thin
-- @+node:gcross.20090101195553.2:@thin FilterCompletedGraphs.hs
-- @@language Haskell

{-# LANGUAGE ScopedTypeVariables #-}

import Database.Enumerator
import Database.PostgreSQL.Enumerator
import Data.COrdering
import Data.Set

import CodeQuest.CodeScan.Database

main = do
    connection <- makeConnection "reader"
    (previously_completed_graphs :: [Graph6]) <- withSession connection $
        catchDB (
            doQuery (sql $ "select graph from graphs_completed") fetch1 []
          ) (reportRethrowMsg "Error fetching completed graphs from the database:\n")
    input <- getContents
    let inquiring_graphs = fromList (lines input)
        completed_graphs = fromList previously_completed_graphs
        filtered_graphs = difference inquiring_graphs completed_graphs
    mapM_ putStrLn $ elems filtered_graphs
-- @-node:gcross.20090101195553.2:@thin FilterCompletedGraphs.hs
-- @-leo

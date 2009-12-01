-- @+leo-ver=4-thin
-- @+node:gcross.20090402153014.6:@thin CanonicalizeGraphs.hs
-- @@language haskell

{-# LANGUAGE ScopedTypeVariables #-}

module Main where

-- @<< Imports >>
-- @+node:gcross.20090402153014.7:<< Imports >>
import Control.Arrow
import Control.Monad
import Control.Monad.Trans
import Data.Function
import Data.Map (Map)
import qualified Data.Map as Map
import Data.Maybe
import Data.Set (Set)
import qualified Data.Set as Set
import Database.Enumerator
import Database.PostgreSQL.Enumerator
import Database.PostgreSQL.PGFunctions
import System.IO
import Text.Printf

import CodeQuest.CodeScan.CodeGraph
import CodeQuest.CodeScan.Database
-- @-node:gcross.20090402153014.7:<< Imports >>
-- @nl

-- @+others
-- @+node:gcross.20090402153014.8:buildEquivalenceClasses
buildEquivalenceClasses :: (Monad m) => String -> IterAct m (Map String (Set String))
buildEquivalenceClasses graph current_classes = result' $ Map.alter addToSet canonical_graph current_classes --'
    where
        canonical_graph = canonicalizeGraph6 graph 

        addToSet :: Maybe (Set String) -> Maybe (Set String)
        addToSet maybe_set =
            case maybe_set of
                Nothing -> Just $ Set.singleton graph
                Just set -> Just $ Set.insert graph set
-- @-node:gcross.20090402153014.8:buildEquivalenceClasses
-- @+node:gcross.20090402153014.9:processClass
processClass :: String -> Set String -> (Maybe String,[String])
processClass canonical_graph graphs = ((\g -> if g == canonical_graph then Nothing else Just g) *** Set.elems) $
    if Set.member canonical_graph graphs
        then (canonical_graph,Set.delete canonical_graph graphs)
        else Set.deleteFindMin graphs
-- @-node:gcross.20090402153014.9:processClass
-- @+node:gcross.20090402153014.10:processClasses
processClasses :: (Map String (Set String)) -> ([String],[String])
processClasses =
    (catMaybes *** concat)
    .
    unzip
    .
    (map (uncurry processClass))
    .
    (Map.assocs)
-- @-node:gcross.20090402153014.10:processClasses
-- @+node:gcross.20090402153014.11:getTODOList
getTODOList table_name = 
    (query
        (sql $ "select distinct graph from " ++ table_name)
        buildEquivalenceClasses Map.empty
        (printf "Error fetching graphs from %s:" table_name)
    ) >>= (return.processClasses)
-- @-node:gcross.20090402153014.11:getTODOList
-- @+node:gcross.20090402153014.15:updateGraphs
updateGraphs table_name to_update =
  catchDB (
    withPreparedStatement
        (prepareCommand
            ("update_"++table_name)
            (sql $ "update " ++ table_name ++ " set graph=? where graph=?")
            [pgTypeOid "", pgTypeOid ""]
        ) $ \prepared_stmt ->
        forM_ to_update $ \old_graph ->
            let new_graph = canonicalizeGraph6 old_graph
            in withBoundStatement prepared_stmt [bindP new_graph, bindP old_graph]
                (flip modify $ printf "Error changing graph %s to %s:")
  ) (reportRethrowMsg "\nError setting up query to update graphs:\n")
-- @-node:gcross.20090402153014.15:updateGraphs
-- @+node:gcross.20090402153014.14:deleteGraphs
deleteGraphs table_name to_delete =
    withPreparedStatement
        (prepareCommand
            ("delete_"++table_name)
            (sql $ "delete from " ++ table_name ++ " where graph=?")
            [pgTypeOid ""]
        ) $ \prepared_stmt ->
        forM_ to_delete $ \graph ->
            withBoundStatement prepared_stmt [bindP graph]
                (flip modify $ printf "Error delete graph %s:" graph)
-- @-node:gcross.20090402153014.14:deleteGraphs
-- @+node:gcross.20090402153014.13:performSimpleTODOList
performSimpleTODOList table_name  = do
    liftIO $ putStrLn $ printf "Canonicalizing table %s:" table_name
    liftIO $ putStrLn "\tFetching..."
    (to_update,to_delete) <- getTODOList table_name
    liftIO $ putStrLn $ printf "\t %i graphs to update, %i graphs to delete" (length to_update) (length to_delete)
    liftIO $ putStrLn "\tUpdating..."
    updateGraphs table_name to_update
    liftIO $ putStrLn "\tDeleting..."
    deleteGraphs table_name to_delete
    liftIO $ putStrLn "\tdone!"
-- @-node:gcross.20090402153014.13:performSimpleTODOList
-- @+node:gcross.20090402153014.16:performCodesTODOList
performCodesTODOList table_name = do
    liftIO $ putStrLn $ printf "Canonicalizing table %s:" table_name
    liftIO $ putStrLn "\tFetching..."
    (to_update,to_delete) <- getTODOList table_name
    liftIO $ putStrLn $ printf "\t %i graphs to update, %i graphs to delete" (length to_update) (length to_delete)
    liftIO $ putStrLn "\tUpdating..."
    mapM_ updateGraph to_update
    liftIO $ putStrLn "\tDeleting..."
    deleteGraphs table_name to_delete
    liftIO $ putStrLn "\tdone!"
  where
    updateGraph graph = do
        code_ids_affected <- query
            (sqlbind (printf "select code_id from %s where graph = ?" table_name) [bindP graph])
            fetch1 []
            (printf "Error fetching code_ids for graph %s:" graph)
        codes_affected <- mapM readCodeFromDatabase_ code_ids_affected
        let updated_codes = map canonicalizeCode codes_affected
        mapM_ replaceCodeInDatabase_ updated_codes
-- @-node:gcross.20090402153014.16:performCodesTODOList
-- @+node:gcross.20090402153014.12:main
main =
  catchDB ( do
    connection <- makeConnection "master"
    withSession connection $ withTransaction ReadUncommitted $ do
        mapM_ performSimpleTODOList ["graphs_completed_EDC","graphs_completed"]
        performCodesTODOList "codes"
  ) (reportRethrowMsg "\nDatabase error:\n")
-- @-node:gcross.20090402153014.12:main
-- @-others
-- @-node:gcross.20090402153014.6:@thin CanonicalizeGraphs.hs
-- @-leo

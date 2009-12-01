-- @+leo-ver=4-thin
-- @+node:gcross.20090310145314.2:@thin ComputeEquivalenceClass.hs
-- @@language Haskell

{-# LANGUAGE TypeSynonymInstances #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Main where

-- @<< Imports >>
-- @+node:gcross.20090310145314.6:<< Imports >>
import Control.Monad
import Control.Monad.Cont
import Control.Monad.Trans
import Database.PostgreSQL.Enumerator
import Data.Array
import Data.Graph
import Data.List
import Data.Maybe
import Data.UUID
import System
import System.Exit
import System.IO
import System.Random
import Text.Printf

import CodeQuest.CodeScan.Code
import CodeQuest.CodeScan.CodeGraph
import CodeQuest.CodeScan.Database
import CodeQuest.CodeScan.Equivalence
-- @-node:gcross.20090310145314.6:<< Imports >>
-- @nl

-- @+others
-- @+node:gcross.20090401195710.4:processCode
processCode code_id done = do
    let done_ = done ()
    putStrLn_ $ take 76 $ repeat '-'
    putStrLn_ $ "\t" ++ code_id
    putStrLn_ $ take 76 $ repeat '-'
    putStr_ $ "Determining whether the class of this code is already known... "
    maybe_class_id :: Maybe String <- lift $ query
        (sqlbind "select class_id from classes where code_id = ?::uuid" [bindP code_id])
        get1 Nothing
        "Error looking for the code_id in the classes table:"
    unless (maybe_class_id == Nothing) $ do
        putStrLn_ $ "yep!"
        putStrLn_ $ printf "\tClass: %s" (fromJust maybe_class_id)
        putStrLn_ $ "Done."
        done_
    putStrLn_ $ "nope!"

    putStrLn_ $ printf "Fetching %s..." code_id
    code <- lift $ readCodeFromDatabase_ code_id
    let system = codeToSystem code

    putStrLn_ $ "Computing canonical system identifier..."
    let ((graph,number),_) = canonicalRepresentation system
    putStrLn_ $ printf "\tGraph: %s" graph
    putStrLn_ $ printf "\tNumber: %i" number
    let markIdentifierAndDeclareVictory = do
            number_of_rows_affected <- lift $ modify
                (cmdbind "update classes set code_id=?::uuid where graph=? and number=?::int" [bindP code_id, bindP graph, bindP number])
                "Error updating the identifier with its corresponding code!"
            unless (number_of_rows_affected == 1) $ do
                putStrLn_ $ printf "The number of rows updated should have been 1, but instead it was %i!" number_of_rows_affected
                exitFailure_
            putStrLn_ $ "Done."
            done_

    putStr_ $ "Searching for this identifier in the class table..."
    found_record :: Maybe (String, Maybe String) <- lift $ query
        (sqlbind "select class_id, code_id from classes where graph=? and number=?::int" [bindP graph, bindP number])
        get2 Nothing
        "Error searching for the indentifier in the database:"
    unless (found_record == Nothing) $ do
        let (class_id,maybe_previous_code_id) = fromJust found_record
        putStrLn_ $ "found!"
        putStrLn_ $ printf "\tClass: %s" class_id
        putStrLn_ $ "Updating class table to cache result..."
        if maybe_previous_code_id == Nothing
            then markIdentifierAndDeclareVictory
            else do
                putStrLn_ $ printf "This identifier is already claimed by code %s!  :-(" (fromJust maybe_previous_code_id)
                exitFailure_        
    putStrLn_ $ "done!"
    putStrLn_ $ "The identifier is not present, so this code belongs to a new class."
    uuid_string <- liftIO $ randomIO >>= (return.toString)
    putStrLn_ $ "We'll go ahead and give this new class the UUID " ++ uuid_string
    putStrLn_ $ "Computing the equivalence class... "
    let equivalences = computeEquivalentIdentifiers system
        number_of_equivalences_found = length equivalences
    putStrLn_ $ printf "%i unique but equivalence systems found in this class!" number_of_equivalences_found

    putStrLn_ $ "Sending these equivalences to the database..."
    let values_as_strings = [printf "($$%s$$,%i,$$%s$$)" name number uuid_string | (name,number) <- equivalences]   
    number_of_rows_affected <- lift $ modify
        (sql $ "insert into classes (graph,number,class_id) values " ++ (intercalate "," values_as_strings))
        "Error sending the equivalences to the database!"
    unless (number_of_rows_affected == number_of_equivalences_found) $ do
        putStrLn_ $ printf "The number of rows updated should have been %s, but instead it was %i!" number_of_equivalences_found number_of_rows_affected
        exitFailure_
    putStrLn_ $ "Mark our identifier as belonging to our code..."
    markIdentifierAndDeclareVictory
    done_
-- @-node:gcross.20090401195710.4:processCode
-- @+node:gcross.20090401195710.5:lifted IO
putStr_ :: String -> ContT () (DBM mark s) ()
putStr_ s = lift $ liftIO $ putStr s >> hFlush stdout

putStrLn_ :: String -> ContT () (DBM mark s) ()
putStrLn_ s = lift $ liftIO $ putStrLn s

exitFailure_ :: ContT () (DBM mark s) ()
exitFailure_ = lift $ liftIO $ exitFailure
-- @-node:gcross.20090401195710.5:lifted IO
-- @+node:gcross.20090310145314.3:main
main :: IO ()
main = do
    args <- getArgs
    input <- getContents
    let code_ids = if length args > 0
                    then args
                    else lines input

    connection <- makeConnection "scanner"

    withSession connection $
        forM_ code_ids $ \code_id -> runContT (callCC $ processCode code_id) return
-- @-node:gcross.20090310145314.3:main
-- @-others
-- @nonl
-- @-node:gcross.20090310145314.2:@thin ComputeEquivalenceClass.hs
-- @-leo

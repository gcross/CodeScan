-- @+leo-ver=4-thin
-- @+node:gcross.20090311120257.2:@thin ComputeIdentifier.hs
-- @@language Haskell

{-# LANGUAGE TypeSynonymInstances #-}

module Main where

-- @<< Imports >>
-- @+node:gcross.20090311120257.3:<< Imports >>
import Control.Monad
import Control.Monad.Cont
import Control.Monad.Trans
import Database.PostgreSQL.Enumerator
import Data.Array
import Data.Graph
import Data.List
import Data.Maybe
import System
import System.Exit
import System.IO
import System.Random
import Text.Printf

import CodeQuest.CodeScan.Code
import CodeQuest.CodeScan.Database
import CodeQuest.CodeScan.Equivalence
-- @-node:gcross.20090311120257.3:<< Imports >>
-- @nl

-- @+others
-- @+node:gcross.20090311120257.5:main
main :: IO ()
main = do
    args <- getArgs
    input <- getContents
    let code_ids = if length args > 0
                    then args
                    else lines input

    withSession (
      connect
        [CAhost "bitslayer.cs.washington.edu"
        ,CAdbname "codequest"
        ,CAuser "codescan"
        ,CApassword "questforthebest"
        ]
      ) $ forM_ code_ids processCode

putStr_ :: String -> (DBM mark s) ()
putStr_ s = liftIO $ putStr s >> hFlush stdout

putStrLn_ :: String -> (DBM mark s) ()
putStrLn_ s = liftIO $ putStrLn s

processCode code_id = do
    putStrLn_ $ printf "Fetching %s..." code_id
    code <- readCodeFromDatabase_ code_id
    let system = codeToSystem code

    putStrLn_ $ "Computing canonical system identifier..."
    let ((graph,number),_) = canonicalRepresentation system
    putStrLn_ $ printf "\tGraph: %s" graph
    putStrLn_ $ printf "\tNumber: %i" number

-- @-node:gcross.20090311120257.5:main
-- @-others
-- @nonl
-- @-node:gcross.20090311120257.2:@thin ComputeIdentifier.hs
-- @-leo

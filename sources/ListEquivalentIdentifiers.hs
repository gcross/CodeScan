-- @+leo-ver=4-thin
-- @+node:gcross.20090304103044.25:@thin ListEquivalentIdentifiers.hs
-- @@language Haskell

{-# LANGUAGE TypeSynonymInstances #-}

module Main where

import System
import System.IO
import Database.PostgreSQL.Enumerator
import Data.Graph
import Data.Graph.Automorphism
import Data.Array
import Text.Printf

import CodeQuest.CodeScan.Code
import CodeQuest.CodeScan.CodeGraph
import CodeQuest.CodeScan.Database
import CodeQuest.CodeScan.Equivalence

-- @+others
-- @+node:gcross.20090304103044.26:main
main :: IO ()
main = do
    args <- getArgs
    if args == [] then error "no code id specified" else return ()
    let code_id = head args
    hPutStrLn stderr $ printf "Fetching %s..." code_id
    connection <- makeConnection "reader"
    (code,_) <- readCodeFromDatabase connection code_id
    hPutStrLn stderr "Computing equivalences..."
    ((mapM_ (\(name,number) -> printf "%s %i\n" name number)).computeEquivalentIdentifiers.codeToSystem) code
-- @-node:gcross.20090304103044.26:main
-- @-others
-- @nonl
-- @-node:gcross.20090304103044.25:@thin ListEquivalentIdentifiers.hs
-- @-leo

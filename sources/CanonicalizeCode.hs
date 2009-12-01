-- @+leo-ver=4-thin
-- @+node:gcross.20090402153014.4:@thin CanonicalizeCode.hs
-- @@language Haskell

{-# LANGUAGE TypeSynonymInstances #-}

module Main where

import System
import Control.Monad.Trans
import Database.PostgreSQL.Enumerator
import Data.Graph
import Data.Array

import CodeQuest.CodeScan.Code
import CodeQuest.CodeScan.CodeGraph
import CodeQuest.CodeScan.Database

-- @+others
-- @+node:gcross.20090402153014.5:main
main :: IO ()
main = do
    args <- getArgs
    if args == [] then error "no code id specified" else return ()
    let code_id = head args
    putStr "Fetching "; putStr code_id; putStrLn "..."
    connection <- makeConnection "reader"
    code <- withSession connection $ readCodeFromDatabase_ code_id
    putCode code
    (putCode.canonicalizeCode) code
-- @-node:gcross.20090402153014.5:main
-- @-others
-- @nonl
-- @-node:gcross.20090402153014.4:@thin CanonicalizeCode.hs
-- @-leo

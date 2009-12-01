-- @+leo-ver=4-thin
-- @+node:gcross.20081222162732.12:@thin ShowCode.hs
-- @@language Haskell

{-# LANGUAGE TypeSynonymInstances #-}

module Main where

import System
import Database.PostgreSQL.Enumerator
import Data.Graph
import Data.Array

import CodeQuest.CodeScan.Code
import CodeQuest.CodeScan.Database

-- @+others
-- @+node:gcross.20081228030258.13:main
main :: IO ()
main = do
    args <- getArgs
    if args == [] then error "no code id specified" else return ()
    let code_id = head args
    putStr "Fetching "; putStr code_id; putStrLn "..."
    connection <- makeConnection "reader"
    code <- withSession connection $ readCodeFromDatabase_ code_id
    putCode code
-- @-node:gcross.20081228030258.13:main
-- @-others
-- @nonl
-- @-node:gcross.20081222162732.12:@thin ShowCode.hs
-- @-leo

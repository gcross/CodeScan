-- @+leo-ver=4-thin
-- @+node:gcross.20090107230224.2:@thin ScanCodes.hs
-- @@language haskell

{-# LANGUAGE ScopedTypeVariables #-}

module Main where

import Control.Monad
import Control.Monad.Error
import Control.Monad.Trans
import Data.ConfigFile
import System
import System.CPUTime
import System.Exit
import Text.Printf

import CodeQuest.CodeScan.Scanner

main = do
    either_connection_information <- runErrorT $ do
        cp <- join $ liftIO $ readfile emptyCP "connection.cfg"
        host :: String <- get cp "data source" "host"
        database :: String <- get cp "data source" "database"
        user :: String <- get cp "scanner" "user"
        password :: String <- get cp "scanner" "password"
        return $ printf "host=%s port=5432 dbname=%s user=%s password=%s" host database user password
    case either_connection_information of
        Left err -> do
            print err
            exitFailure
        Right connection_information -> withConnectionTo connection_information $ \connection -> do
            let scan_graph = scan connection
            args <- getArgs
            input <- getContents
            let graphs = if length args > 0
                            then args
                            else lines input

            mapM_ scan_graph graphs
-- @-node:gcross.20090107230224.2:@thin ScanCodes.hs
-- @-leo

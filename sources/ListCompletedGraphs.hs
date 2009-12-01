-- @+leo-ver=4-thin
-- @+node:gcross.20090224112926.2:@thin ListCompletedGraphs.hs
-- @@language Haskell

{-# LANGUAGE ScopedTypeVariables #-}

import Database.Enumerator
import Database.PostgreSQL.Enumerator
import Data.Char
import System
import System.IO
import System.Console.GetOpt
import Text.Printf

import CodeQuest.CodeScan.Database

data Options = Options
 { optNumberOfVertices :: Maybe Int
 } deriving Show

options :: [OptDescr (Options -> Options)]
options =
 [ Option ['v']     ["vertices"] -- '
     (ReqArg ((\ n opts -> opts { optNumberOfVertices = Just (read n) }))
             "#")
     "number of vertices in the graph"
 ]

defaultOptions    = Options
 { optNumberOfVertices = Nothing
 }

parseOptions :: [String] -> Options
parseOptions args = 
    case getOpt Permute options args of
        (o,_,[]  ) -> foldl (flip id) defaultOptions o
        (_,_,errs) -> error (concat errs ++ usageInfo header options)
    where header = "Usage: ListCompletedGraphs [OPTION...]"

filterVertices :: Options -> (String,String)
filterVertices Options { optNumberOfVertices = Nothing } = ("","")
filterVertices Options { optNumberOfVertices = Just n } =
    let begin_letter = chr ((ord 'A') + n-2) -- '
        end_letter = chr ((ord 'A') + n-2 + 1) -- '
    in ("",printf " and graph > '%c' and graph < '%c'" begin_letter end_letter)

main = do
    args <- getArgs
    let options = parseOptions args
        (joins,filters) = (unzip.(map ($ options))) [filterVertices]
        sql_statement = printf "(select distinct graph from graphs_completed %s where true %s) union (select distinct graph from graphs_completed_EDC %s where true %s);" (concat joins) (concat filters) (concat joins) (concat filters)
    hPutStrLn stderr $ "> " ++ sql_statement
    connection <- makeConnection "reader"
    (graphs :: [Graph6]) <- withSession connection $
        catchDB (
            doQuery (sql $ sql_statement) fetch1 []
          ) (reportRethrowMsg "Error fetching completed graphs from the database:\n")
    mapM_ putStrLn graphs
-- @-node:gcross.20090224112926.2:@thin ListCompletedGraphs.hs
-- @-leo

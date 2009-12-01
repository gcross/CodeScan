-- @+leo-ver=4-thin
-- @+node:gcross.20090310192332.2:@thin ListWinningCodes.hs
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
 , optNumberOfLogicals :: Maybe Int
 } deriving Show

options :: [OptDescr (Options -> Options)]
options =
 [ Option ['v']     ["vertices"] -- '
     (ReqArg ((\ n opts -> opts { optNumberOfVertices = Just (read n) }))
             "#")
     "number of vertices in the graph"
 , Option ['l']     ["logicals"] -- '
     (ReqArg ((\ n opts -> opts { optNumberOfLogicals = Just (read n) }))
             "#")
     "number of logical qubits found"
 ]

defaultOptions    = Options
 { optNumberOfVertices = Nothing
 , optNumberOfLogicals = Nothing
 }

parseOptions :: [String] -> Options
parseOptions args = 
    case getOpt Permute options args of
        (o,_,[]  ) -> foldl (flip id) defaultOptions o
        (_,_,errs) -> error (concat errs ++ usageInfo header options)
    where header = "Usage: ListWinningGraphs [OPTION...]"

filterLogicals :: Options -> (String,String)
filterLogicals Options { optNumberOfLogicals = Nothing } = ("","")
filterLogicals Options { optNumberOfLogicals = Just n } = (" natural join count_of_logicals_in_codes "," and count >= " ++ (show n))

filterVertices :: Options -> (String,String)
filterVertices Options { optNumberOfVertices = Nothing } = ("","")
filterVertices Options { optNumberOfVertices = Just n } =
    let begin_letter = chr ((ord 'A') + n-2) -- '
        end_letter = chr ((ord 'A') + n-2 + 1) -- '
    in ("",printf " and graph > '%c' and graph < '%c'" begin_letter end_letter)

main = do
    args <- getArgs
    let options = parseOptions args
        (joins,filters) = (unzip.(map ($ options))) [filterLogicals,filterVertices]
        sql_statement = printf "select code_id from codes %s where true %s;" (concat joins) (concat filters)
    hPutStrLn stderr $ "> " ++ sql_statement
    connection <- makeConnection "reader"
    (code_ids :: [String]) <- withSession connection $
        catchDB (
            doQuery (sql $ sql_statement) fetch1 []
          ) (reportRethrowMsg "Error fetching codes from the database:\n")
    mapM_ putStrLn code_ids
-- @-node:gcross.20090310192332.2:@thin ListWinningCodes.hs
-- @-leo

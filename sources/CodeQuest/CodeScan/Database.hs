-- @+leo-ver=4-thin
-- @+node:gcross.20090101195553.4:@thin Database.hs
-- @@language Haskell

{-# LANGUAGE ScopedTypeVariables #-}

module CodeQuest.CodeScan.Database where

-- @<< Imports >>
-- @+node:gcross.20090319091700.12:<< Imports >>
import Control.Arrow
import Control.Monad
import Control.Monad.Error
import Control.Monad.Trans
import Control.Exception
import Database.Enumerator
import Database.PostgreSQL.Enumerator
import Database.PostgreSQL.PGFunctions
import Data.Array
import Data.ConfigFile
import Data.Graph
import Data.Graph.Construction
import Data.List (zipWith4)
import Data.Maybe
import Data.Time.Clock
import Data.Typeable
import Data.UUID
import System.Exit
import System.IO
import System.Process
import System.Random
import Text.Printf

import CodeQuest.CodeScan.Code
import CodeQuest.CodeScan.CodeGraph




-- @-node:gcross.20090319091700.12:<< Imports >>
-- @nl

-- @<< Types >>
-- @+node:gcross.20090319091700.13:<< Types >>
type CodeID = String
type QubitID = String
type OperatorID = String
type Graph6 = String
-- @-node:gcross.20090319091700.13:<< Types >>
-- @nl


-- @+others
-- @+node:gcross.20081228030258.2:__ToFloat
intToFloat :: Int -> Float
intToFloat = fromInteger . toInteger


doubleToFloat :: Double -> Float
doubleToFloat = fromRational . toRational
-- @-node:gcross.20081228030258.2:__ToFloat
-- @+node:gcross.20090331194158.2:makeConnection
makeConnection heading = do
    either_conn <- runErrorT $ do
        cp <- join $ liftIO $ readfile emptyCP "connection.cfg"
        host <- get cp "data source" "host"
        database <- get cp "data source" "database"
        user <- get cp heading "user"
        password <- get cp heading "password"
        return $ connect
            [   CAhost host
            ,   CAdbname database
            ,   CAuser user
            ,   CApassword password
            ]
    case either_conn of
        Left err -> do
            print err
            exitFailure
        Right conn -> return conn
-- @-node:gcross.20090331194158.2:makeConnection
-- @+node:gcross.20090319091700.8:enumerators
-- @+node:gcross.20090310145314.5:getX
get1 :: (Monad m) => a -> IterAct m (Maybe a)
get1 x _ = return $ Left $ Just $! x

get2 :: (Monad m) => a -> b -> IterAct m (Maybe (a,b))
get2 x y _ = return $ Left $ Just $! (x,y)
-- @-node:gcross.20090310145314.5:getX
-- @+node:gcross.20090319091700.9:fetchX
fetch1 :: (Monad m) => a -> IterAct m [a]
fetch1 a accum = result' (a:accum) --'

fetch2 :: (Monad m) => a -> b -> IterAct m [(a, b)]
fetch2 a b accum = result' ((a, b):accum) --'

fetch3 :: (Monad m) => a -> b -> c -> IterAct m [(a, b, c)]
fetch3 a b c accum = result' ((a, b, c):accum) --'

fetch4 :: (Monad m) => a -> b -> c -> d -> IterAct m [(a, b, c, d)]
fetch4 a b c d accum = result' ((a, b, c, d):accum) --'
-- @-node:gcross.20090319091700.9:fetchX
-- @-node:gcross.20090319091700.8:enumerators
-- @+node:gcross.20090319091700.10:sql wrappers
-- @+node:gcross.20090311194934.2:query
query stmt accum init message = 
    catchDB (
            doQuery stmt accum init
          ) (reportRethrowMsg $ message ++ "\n")
-- @-node:gcross.20090311194934.2:query
-- @+node:gcross.20090311204330.2:modify
modify stmt message = catchDB (
        execDML stmt
    ) (reportRethrowMsg $ message ++ "\n")
-- @nonl
-- @-node:gcross.20090311204330.2:modify
-- @-node:gcross.20090319091700.10:sql wrappers
-- @+node:gcross.20090319091700.3:connectToDatabase
-- @+at
--  connectToDatabase = connect
--          [CAhost "bitslayer.cs.washington.edu"
--          ,CAdbname "codequest"
--          ,CAuser "codescan"
--          ,CApassword "questforthebest"
--          ]
-- @-at
-- @@c

connectToDatabase = connect
        [CAhost "localhost"
        ,CAdbname "codequest"
        ,CAuser "codescan"
        ,CApassword "questforthebest"
        ]
-- @-node:gcross.20090319091700.3:connectToDatabase
-- @+node:gcross.20090319091700.11:code reading
-- @+node:gcross.20081225173032.7:fetchEdges
fetchEdges :: (Monad m) => Int -> Int -> IterAct m ([(Int,Int)],[[(Int,Pauli)]])
fetchEdges label1 label2 (((vertex1,vertex2):edges),lst) =
    let pauli1 = pauliFromLabel label1
        pauli2 = pauliFromLabel label2
    in result' (edges,([(vertex1,pauli1),(vertex2,pauli2)]:lst)) --'
-- @-node:gcross.20081225173032.7:fetchEdges
-- @+node:gcross.20081225173032.8:fetchOperators
fetchOperators :: (Monad m) => String -> Int -> Int -> IterAct m (String,[[(Int,Pauli)]])
fetchOperators id vertex label (last_id,lst) =
    let pauli = pauliFromLabel label
        item = (vertex,pauli)
    in result' (id,if not (id == last_id) --'
                     then [item]:lst
                     else let (op:ops) = lst
                          in (item:op):ops
                 )
-- @-node:gcross.20081225173032.8:fetchOperators
-- @+node:gcross.20081225173032.9:listsToOperators
listsToOperators :: Int -> [(Int,Pauli)] -> QuantumOperator
listsToOperators number_of_bits lst = (listArray (0,number_of_bits-1) (repeat I)) // lst
-- @-node:gcross.20081225173032.9:listsToOperators
-- @+node:gcross.20081225173032.16:queryOperators
queryOperators number_of_bits cmd =  catchDB ( do
    (_,lists) <- doQuery (sql cmd) fetchOperators ("",[])
    return $ map (listsToOperators number_of_bits) lists
   )  (reportRethrowMsg "Error fetching operators from database:\n")
-- @nonl
-- @-node:gcross.20081225173032.16:queryOperators
-- @+node:gcross.20081225173032.10:readCodeFromDatabase
readCodeFromDatabase conn code_id = withContinuedSession conn $ readCodeFromDatabase_ code_id

readCodeFromDatabase_ code_id = catchDB ( do
    let code_id_condition = " where code_id='" ++ code_id ++ "'"
    -- @    << Read in the graph and timestamp. >>
    -- @+node:gcross.20081225173032.11:<< Read in the graph and timestamp. >>
    maybe_code_info :: Maybe (String,UTCTime) <- query
        (sql $ "select graph, timestamp from codes" ++ code_id_condition)
        get2 Nothing
        "Error fetching code from database:"
    when (maybe_code_info == Nothing) $ error $ "code " ++ code_id ++ " not found in codes table"
    let (graph_name,timestamp) = fromJust maybe_code_info
        graph = fromGraph6 graph_name
        number_of_vertices = length $ vertices graph
    -- @-node:gcross.20081225173032.11:<< Read in the graph and timestamp. >>
    -- @nl
    -- @    << Read in the hamiltonian. >>
    -- @+node:gcross.20081225173032.12:<< Read in the hamiltonian. >>
    let edges = graph6Edges graph
    assert (not (edges == [])) $ return ()
    (_,hamiltonian_lists) <- query
        (sql $ "select label1, label2 from hamiltonians" ++ code_id_condition ++ " order by edge")
        fetchEdges (edges,[])
        "Error fetching hamiltonian from database:"
    let hamiltonian = map (listsToOperators number_of_vertices) hamiltonian_lists
        interactions = [(a,b,c,d) | [(a,b),(c,d)] <- hamiltonian_lists]
    -- @-node:gcross.20081225173032.12:<< Read in the hamiltonian. >>
    -- @nl
    -- @    << Read in the stabilizers. >>
    -- @+node:gcross.20081225173032.13:<< Read in the stabilizers. >>
    stabilizers <- catchDB ( queryOperators number_of_vertices $
        "select operator_id, vertex, label \
        \from stabilizers natural join operators"
            ++ code_id_condition ++ " order by operator_id"
      ) (reportRethrowMsg "Error fetching stabilizers from database:\n")
    -- @-node:gcross.20081225173032.13:<< Read in the stabilizers. >>
    -- @nl
    -- @    << Read in the gauge qubits. >>
    -- @+node:gcross.20081225173032.14:<< Read in the gauge qubits. >>
    let fetchOperators typ pauli = let name = (show pauli) in catchDB ( queryOperators number_of_vertices $
           (printf "select qubit_id, vertex, label \
            \from %s_qubits natural join qubits \
                           \join operators on operators.operator_id = qubits.%s_operator_id" typ name)
            ++ code_id_condition ++ (printf " order by %s_qubits.qubit_id" typ)
         ) (reportRethrowMsg $ printf "Error fetching %s qubit %s operators from database:\n" typ name)

    let fetchGaugeOperators = fetchOperators "gauge"

    gauge_X_operators <- fetchGaugeOperators X
    gauge_Y_operators <- fetchGaugeOperators Y
    gauge_Z_operators <- fetchGaugeOperators Z

    let gauge_qubits = zipWith3 Qubit gauge_X_operators gauge_Y_operators gauge_Z_operators
    -- @-node:gcross.20081225173032.14:<< Read in the gauge qubits. >>
    -- @nl
    -- @    << Read in the logical qubits. >>
    -- @+node:gcross.20081225173032.15:<< Read in the logical qubits. >>
    let fetchLogicalOperators = fetchOperators "logical"

    logical_X_operators <- fetchLogicalOperators X
    logical_Y_operators <- fetchLogicalOperators Y
    logical_Z_operators <- fetchLogicalOperators Z

    logical_error_operators <- catchDB ( queryOperators number_of_vertices $
        "select logical_qubits.qubit_id, vertex, label \
        \from logical_qubits inner join operators on logical_qubits.mwe_operator_id = operators.operator_id"
            ++ code_id_condition ++ " order by logical_qubits.qubit_id"
      ) (reportRethrowMsg "Error fetching the logical error operators from database:\n")
    let logical_qubits = zipWith4 (\qX -> \qY -> \qZ -> \qE -> (Qubit qX qY qZ,qE)) logical_X_operators logical_Y_operators logical_Z_operators logical_error_operators
    -- @-node:gcross.20081225173032.15:<< Read in the logical qubits. >>
    -- @nl
    return $  Code
                {   codeId = code_id
                ,   codeGraph = graph
                ,   codeGraphName = graph_name
                ,   codeInteractions = interactions
                ,   codeHamiltonian = hamiltonian
                ,   codeStabilizers = stabilizers
                ,   codeGaugeQubits = gauge_qubits
                ,   codeLogicalQubits = logical_qubits
                ,   codePhysicalQubitCount = number_of_vertices
                ,   codeTimestamp = timestamp
               }
  ) (reportRethrowMsg "Error while fetching the code:\n")
-- @-node:gcross.20081225173032.10:readCodeFromDatabase
-- @-node:gcross.20090319091700.11:code reading
-- @+node:gcross.20090319091700.16:code writing
-- @+node:gcross.20090402074358.5:makeUUID
makeUUID :: (forall mark. DBM mark Database.PostgreSQL.Enumerator.Session String)
makeUUID = liftIO $ (randomIO :: IO UUID) >>= (return.toString)
-- @-node:gcross.20090402074358.5:makeUUID
-- @+node:gcross.20090402074358.2:writeOperator
writeOperator :: QuantumOperator -> (forall mark. DBM mark Database.PostgreSQL.Enumerator.Session String)
writeOperator operator = do
        operator_id :: String <- makeUUID
        withPreparedStatement (prepareCommand "insert operator" (sql "insert into operators (operator_id,vertex,label) values (?::uuid,?,?)") [pgTypeOid "", pgTypeOid (0::Int), pgTypeOid (0::Int)]) $ \prepared_stmt ->
            forM_ (filter (\assoc -> (snd assoc) /= I) $ assocs operator) $ \(vertex,label) -> withBoundStatement prepared_stmt [bindP operator_id, bindP vertex, (bindP.fromEnum) label] $ \stmt ->
                modify stmt "Error inserting operator:"
        return operator_id
    where
        types = operator :: QuantumOperator
-- @-node:gcross.20090402074358.2:writeOperator
-- @+node:gcross.20090402074358.4:writeQubit
writeQubit :: Qubit -> (forall mark. DBM mark Database.PostgreSQL.Enumerator.Session String)
writeQubit (Qubit x y z) = do
    x_id <- writeOperator x
    y_id <- writeOperator y
    z_id <- writeOperator z

    qubit_id <- makeUUID

    modify (cmdbind "insert into qubits (qubit_id,X_operator_id,Y_operator_id,Z_operator_id) values (?::uuid,?::uuid,?::uuid,?::uuid)" [bindP qubit_id, bindP x_id, bindP y_id, bindP z_id])
           "Error writing qubit information into qubits table:"

    return qubit_id
-- @-node:gcross.20090402074358.4:writeQubit
-- @+node:gcross.20090319091700.18:writeCodeToDatabase
writeCodeToDatabase_ :: Code -> (forall mark. DBM mark Database.PostgreSQL.Enumerator.Session ())
writeCodeToDatabase_ code = do
    let code_id = codeId code
    -- @    << Write out the codes table record. >>
    -- @+node:gcross.20090319091700.19:<< Write out the codes table record. >>
    modify (cmdbind "insert into codes (code_id,graph,timestamp) values (?::uuid,?,?)" [bindP code_id, (bindP.codeGraphName) code, (bindP.codeTimestamp) code]) "Error writing code information into codes table:"
    -- @-node:gcross.20090319091700.19:<< Write out the codes table record. >>
    -- @nl
    -- @    << Write out the hamiltonian. >>
    -- @+node:gcross.20090331120204.2:<< Write out the hamiltonian. >>
    let interactions = zip [(0::Int)..] [(fromEnum p1,fromEnum p2) | (_,p1,_,p2) <- codeInteractions code]

    withPreparedStatement (prepareCommand "insert interactions" (sql "insert into hamiltonians (code_id,edge,label1,label2) values (?::uuid,?,?,?)") [pgTypeOid "", pgTypeOid (0::Int), pgTypeOid (0::Int), pgTypeOid (0::Int)]) $ \prepared_stmt ->
        forM_ interactions $ \(edge,(label1,label2)) -> withBoundStatement prepared_stmt [bindP code_id, bindP edge, bindP label1, bindP label2] $ \stmt ->
            modify stmt "Error inserting hamiltonian:"

    -- @-node:gcross.20090331120204.2:<< Write out the hamiltonian. >>
    -- @nl
    -- @    << Write out the stabilizers. >>
    -- @+node:gcross.20090402074358.3:<< Write out the stabilizers. >>
    stabilizer_ids <- mapM writeOperator (codeStabilizers code)

    withPreparedStatement (prepareCommand "insert stabilizer" (sql "insert into stabilizers (code_id,operator_id) values (?::uuid,?::uuid)") [pgTypeOid "", pgTypeOid ""]) $ \prepared_stmt ->
        forM_ stabilizer_ids $ \operator_id -> withBoundStatement prepared_stmt [bindP code_id, bindP operator_id] $ \stmt ->
            modify stmt "Error inserting stabilizer:"
    -- @-node:gcross.20090402074358.3:<< Write out the stabilizers. >>
    -- @nl
    -- @    << Write out the gauge qubits. >>
    -- @+node:gcross.20090402074358.6:<< Write out the gauge qubits. >>
    gauge_ids <- mapM writeQubit (codeGaugeQubits code)

    withPreparedStatement (prepareCommand "insert gauge qubit" (sql "insert into gauge_qubits (code_id,qubit_id) values (?::uuid,?::uuid)") [pgTypeOid "", pgTypeOid ""]) $ \prepared_stmt ->
        forM_ gauge_ids $ \qubit_id -> withBoundStatement prepared_stmt [bindP code_id, bindP qubit_id] $ \stmt ->
            modify stmt "Error inserting gauge qubit:"
    -- @-node:gcross.20090402074358.6:<< Write out the gauge qubits. >>
    -- @nl
    -- @    << Write out the logical qubits. >>
    -- @+node:gcross.20090402074358.7:<< Write out the logical qubits. >>
    logical_ids <- forM (codeLogicalQubits code) $ runKleisli ((Kleisli writeQubit) *** (Kleisli writeOperator))

    withPreparedStatement (prepareCommand "insert logical qubit" (sql "insert into logical_qubits (code_id,qubit_id,mwe_operator_id) values (?::uuid,?::uuid,?::uuid)") [pgTypeOid "", pgTypeOid "", pgTypeOid ""]) $ \prepared_stmt ->
        forM_ logical_ids $ \(qubit_id,operator_id) -> withBoundStatement prepared_stmt [bindP code_id, bindP qubit_id, bindP operator_id] $ \stmt ->
            modify stmt "Error inserting logical qubit:"
    -- @-node:gcross.20090402074358.7:<< Write out the logical qubits. >>
    -- @nl
    return ()
-- @-node:gcross.20090319091700.18:writeCodeToDatabase
-- @+node:gcross.20090319091700.15:deleteCodeFromDatabase
deleteCodeFromDatabase conn code = withContinuedSession conn $ deleteCodeFromDatabase_ code
deleteCodeFromDatabase_ code = deleteCodeIdFromDatabase_ (codeId code)

deleteCodeIdFromDatabase_ code_id = modify (cmdbind "delete from codes where code_id=?::uuid" [bindP code_id]) (printf "Error while deleting code %s:" code_id)
-- @-node:gcross.20090319091700.15:deleteCodeFromDatabase
-- @+node:gcross.20090319091700.14:replaceCodeInDatabase
replaceCodeInDatabase_ code = do
    deleteCodeFromDatabase_ code
    writeCodeToDatabase_ code
-- @-node:gcross.20090319091700.14:replaceCodeInDatabase
-- @-node:gcross.20090319091700.16:code writing
-- @-others
-- @-node:gcross.20090101195553.4:@thin Database.hs
-- @-leo

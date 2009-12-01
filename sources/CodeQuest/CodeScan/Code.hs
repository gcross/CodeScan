-- @+leo-ver=4-thin
-- @+node:gcross.20081225173032.3:@thin Code.hs
-- @@language Haskell

{-# LANGUAGE DeriveDataTypeable #-}

module CodeQuest.CodeScan.Code where

-- @<< Imports >>
-- @+node:gcross.20090319091700.5:<< Imports >>
import Control.Monad
import Data.Array
import Data.Graph
import Data.List (sortBy)
import Data.Typeable
import Data.Time
-- @-node:gcross.20090319091700.5:<< Imports >>
-- @nl

-- @+others
-- @+node:gcross.20090319091700.4:display routines
showOperator :: QuantumOperator -> String
showOperator operator = concat $ map show $ elems operator

putQubit (n,(Qubit x y z)) = do
    putStrLn $ "----- QUBIT " ++ (show n) ++ " -----"
    putStrLn $ "\t Logical X: " ++ (showOperator x)
    putStrLn $ "\t Logical Y: " ++ (showOperator y)
    putStrLn $ "\t Logical Z: " ++ (showOperator z)

putOperators name operators = do
    putStrLn $ name ++ ": (" ++ (show $ length operators) ++ " total)"
    mapM_ (\op -> putStrLn $ "\t" ++ showOperator op) operators

putQubits name qubits = do
    putStrLn $ name ++ ": (" ++ (show (length qubits)) ++ " total)"
    mapM_ putQubit $ zip [1..length qubits] qubits

putLogicalQubits name qubits = do
    putStrLn $ name ++ ": (" ++ (show (length qubits)) ++ " total)"
    forM_ (zip [1..length qubits] qubits) $ \(num,(qubit,error_operator)) -> do
        putQubit (num,qubit)
        putStrLn $ "\t Error    : " ++ (showOperator error_operator)

putCode code = do
    -- Display the graph in graph6 format
    putStrLn $ "Graph: " ++ (codeGraphName code)
    -- Display the hamiltonian
    putStrLn $ ""
    putOperators "Hamiltonian Terms" (codeHamiltonian code)
    -- Display the stabilizers
    putStrLn $ ""
    putOperators "Stabilizers" (codeStabilizers code)
    -- Display the gauge qubits
    putStrLn $ ""
    putQubits "Gauge Qubits" (codeGaugeQubits code)
    -- Display the logical qubits
    putStrLn $ ""
    putLogicalQubits "Logical Qubits" (codeLogicalQubits code)
-- @-node:gcross.20090319091700.4:display routines
-- @+node:gcross.20090319091700.6:types
data Pauli = I | X | Z | Y deriving (Eq,Enum,Typeable)

instance Show Pauli where
    show pauli = case pauli of I -> "."
                               X -> "X"
                               Y -> "Y"
                               Z -> "Z"
pauliFromLabel :: Int -> Pauli
pauliFromLabel = toEnum

type QuantumOperator = Array Int Pauli

data Qubit = Qubit {qX::QuantumOperator,qY::QuantumOperator,qZ::QuantumOperator} deriving (Eq)

data Code = Code
    {   codeId :: String
    ,   codeGraph :: Graph
    ,   codeGraphName :: String
    ,   codeInteractions :: [(Vertex,Pauli,Vertex,Pauli)]
    ,   codeHamiltonian :: [QuantumOperator]
    ,   codeStabilizers :: [QuantumOperator]
    ,   codeGaugeQubits :: [Qubit]
    ,   codeLogicalQubits :: [(Qubit,QuantumOperator)]
    ,   codePhysicalQubitCount :: Int
    ,   codeTimestamp :: UTCTime
 } deriving (Eq,Typeable)

-- @-node:gcross.20090319091700.6:types
-- @-others
-- @-node:gcross.20081225173032.3:@thin Code.hs
-- @-leo

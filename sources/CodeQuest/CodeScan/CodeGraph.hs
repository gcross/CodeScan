-- @+leo-ver=4-thin
-- @+node:gcross.20081221143112.2:@thin CodeGraph.hs
-- @@language haskell

module CodeQuest.CodeScan.CodeGraph where

import Control.Arrow
import Control.Exception

import Data.Array
import Data.Graph
import Data.Graph.Automorphism
import Data.Graph.Construction
import Data.Bits
import Data.List
import Data.Char

import Debug.Trace

import CodeQuest.CodeScan.Code
import qualified CodeQuest.CodeScan.MyNauty as MyNauty
import CodeQuest.CodeScan.Permutations

-- @+others
-- @+node:gcross.20090402153014.2:permuteOperator/Qubit
permuteOperator :: Permutation -> QuantumOperator -> QuantumOperator
permuteOperator permutation operator = array (bounds operator) [(permutation!i,e) | (i,e) <- assocs operator]

permuteQubit :: Permutation -> Qubit -> Qubit
permuteQubit permutation (Qubit x y z) =
    let p = permuteOperator permutation
    in (Qubit (p x) (p y) (p z))
-- @-node:gcross.20090402153014.2:permuteOperator/Qubit
-- @+node:gcross.20090402153014.3:canonicalizeCode
canonicalizeCode :: Code -> Code
canonicalizeCode code =
    if canonical_graph == graph then code else
        code
            {   codeGraph = canonical_graph
            ,   codeGraphName = toGraph6 canonical_graph
            ,   codeInteractions = (
                    (sortBy compareInteractions)
                    .
                    (map (fixVertexOrdering.permuteInteractionVertices))
                    .
                    codeInteractions
                   ) code
            ,   codeHamiltonian = map pO $ codeHamiltonian code
            ,   codeStabilizers = map pO $ codeStabilizers code
            ,   codeGaugeQubits = map pQ $ codeGaugeQubits code
            ,   codeLogicalQubits = map (pQ *** pO) $ codeLogicalQubits code
            }
    where
        graph = codeGraph code       
        (canonical_permutation, canonical_graph) = MyNauty.canonicGraph graph       
        pO = permuteOperator canonical_permutation
        pQ = permuteQubit canonical_permutation

        permuteInteractionVertices :: (Vertex,Pauli,Vertex,Pauli) -> (Vertex,Pauli,Vertex,Pauli)
        permuteInteractionVertices (v1,p1,v2,p2) = (canonical_permutation!v1,p1,canonical_permutation!v2,p2)

        fixVertexOrdering :: (Vertex,Pauli,Vertex,Pauli) -> (Vertex,Pauli,Vertex,Pauli)
        fixVertexOrdering (v1,p1,v2,p2) = if (v1>v2) then (v2,p2,v1,p1) else (v1,p1,v2,p2)

        compareInteractions :: (Vertex,Pauli,Vertex,Pauli) -> (Vertex,Pauli,Vertex,Pauli) -> Ordering
        compareInteractions (i1v1,_,i1v2,_) (i2v1,_,i2v2,_) =
            let c1 = i1v1 `compare` i2v1
                c2 = i1v2 `compare` i2v2
            in if c2 /= EQ then c2 else c1
-- @-node:gcross.20090402153014.3:canonicalizeCode
-- @+node:gcross.20090319091700.17:graph6Edges
graph6Edges graph =
    let (minB,maxB) = bounds graph
        number_of_vertices = maxB-minB+1
        all_possible_edges = [(row,col) | col <- [1..number_of_vertices-1], row <- [0..col-1]]
    in filter (\(row,col) -> adjacent graph row col) all_possible_edges
-- @-node:gcross.20090319091700.17:graph6Edges
-- @+node:gcross.20090309112253.5:swap
swap :: (a,b) -> (b,a)
swap = (uncurry.flip.curry)  id
-- @-node:gcross.20090309112253.5:swap
-- @+node:gcross.20081221143112.3:fromGraph6
fromGraph6 :: [Char] -> Graph
fromGraph6 [] = error "graph6 string must have at least two characters"
fromGraph6 [x] = error "graph6 string must have at least two characters"
fromGraph6 (graph_class:adjacency_chars) =
    let number_of_vertices = (ord graph_class)-(ord 'A')+2 --'

        number_of_edges = number_of_vertices*(number_of_vertices-1) `div` 2;
        number_of_chars = number_of_edges `div` 6 + if (number_of_edges `mod` 6>0) then 1 else 0
        extract_bits :: Int -> Int -> [Char] -> [Bool]
        extract_bits mask byte remaining_chars =
                    if mask == 0
                        then case remaining_chars of
                                next_char:more_chars ->
                                    extract_bits 32 ((ord next_char)-63) more_chars
                                _ -> []
                         else ((byte .&. mask) > 0) :
                                    (extract_bits (shift mask (-1)) byte remaining_chars)
        adjacency_bits = extract_bits 0 0 adjacency_chars
        all_possible_edges = [(row,col) | col <- [1..number_of_vertices-1], row <- [0..col-1]]
        edges = [edge | (flag,edge) <- zip adjacency_bits all_possible_edges, flag]
    in assert ((length adjacency_chars) == number_of_chars) $
            fmap sort $ buildG (0,number_of_vertices-1) (edges ++ map swap edges)
-- @-node:gcross.20081221143112.3:fromGraph6
-- @+node:gcross.20081230235952.2:toGraph6
toGraph6 :: Graph -> [Char]
toGraph6 graph =
    let number_of_vertices = (length . vertices) graph
        all_possible_edges = [(row,col) | col <- [1..number_of_vertices-1], row <- [0..col-1]]
        adj = adjacent graph
        adjacency_bits = [a `adj` b | (a,b) <- all_possible_edges]
        stringFrom :: [Bool] -> [Char]
        stringFrom bits
                   | bits == [] = []
                   | length bits < 6 = stringFrom $ bits ++ (replicate (6 - (length bits)) False)
                   | otherwise = let (word,rest) = splitAt 6 bits
                                     number = foldr (.|.) 0 [if bit then n else 0 | (n,bit) <- zip [32,16,8,4,2,1] word]
                                     char = chr (number+63)
                                 in char:stringFrom rest
        prefix = chr ((ord 'A') + number_of_vertices-2) --'
    in prefix:stringFrom adjacency_bits
-- @-node:gcross.20081230235952.2:toGraph6
-- @+node:gcross.20090314200623.2:canonicalizeGraph6
canonicalizeGraph6 = toGraph6.canonicGraph.fromGraph6
-- @nonl
-- @-node:gcross.20090314200623.2:canonicalizeGraph6
-- @+node:gcross.20081228030258.26:permutationsOfGraphNamed
permutationsOfGraphNamed :: String -> [Array Int Int]
permutationsOfGraphNamed = 
    forestToPermutationList . computeForestFromGenerators . autGenerators . undirG . fromGraph6
-- @-node:gcross.20081228030258.26:permutationsOfGraphNamed
-- @+node:gcross.20081221143112.7:permutationsInCOfGraphNamed
permutationsInCOfGraphNamed :: String -> String
permutationsInCOfGraphNamed graphname = 
    let permutations = permutationsOfGraphNamed graphname
        permutationList [] = "}\n"
        permutationList (p:rest) = (show p) ++ case rest of
                                                [] -> "},\n"
                                                _ -> "," ++ permutationList rest
    in "\t{\n" ++ (concat [("\t\t{"++(permutationList $ elems permutation)) | permutation <- permutations] ) ++ "\t}\n"
-- @-node:gcross.20081221143112.7:permutationsInCOfGraphNamed
-- @+node:gcross.20081225173032.4:adjacent
adjacent :: Graph -> Int -> Int -> Bool
adjacent graph v1 v2 = (elem v1 $ graph ! v2) || (elem v2 $ graph ! v1)
-- @-node:gcross.20081225173032.4:adjacent
-- @+node:gcross.20081228030258.32:combinationCount
combinationCount :: Graph -> Integer
combinationCount graph = 
    let (0,nm1) = bounds graph
        vertex_combinations = map computeVertexCombinations ((elems.indegree) graph)
        computeVertexCombinations :: Int -> Integer
        computeVertexCombinations 1 = 1
        computeVertexCombinations n = (3^(n-1)-1) `div` 2
    in foldr1 (*) vertex_combinations
-- @-node:gcross.20081228030258.32:combinationCount
-- @-others
-- @-node:gcross.20081221143112.2:@thin CodeGraph.hs
-- @-leo

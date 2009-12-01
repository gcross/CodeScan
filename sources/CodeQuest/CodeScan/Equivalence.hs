-- @+leo-ver=4-thin
-- @+node:gcross.20090224112926.5:@thin Equivalence.hs
-- @@language haskell

module CodeQuest.CodeScan.Equivalence where

-- @<< Imports >>
-- @+node:gcross.20090224112926.6:<< Imports >>
import CodeQuest.CodeScan.Code
import CodeQuest.CodeScan.CodeGraph
import CodeQuest.CodeScan.MyNauty
import CodeQuest.CodeScan.Permutations hiding (Permutation)

import Control.Exception

import Data.Array
import Data.Array.IArray (IArray)
import qualified Data.Array.IArray as IArray
import Data.Map (Map)
import qualified Data.Map as Map
import Data.Function
import Data.Graph
import Data.Graph.Permutation
import qualified Data.Graph.Automorphism
import Data.List
import Data.Maybe

import Debug.Trace
import Text.Printf
-- @-node:gcross.20090224112926.6:<< Imports >>
-- @nl

-- @<< Types >>
-- @+node:gcross.20090224112926.18:<< Types >>
type Interactions = Table [Pauli]
data System = System Graph Interactions deriving (Eq,Show)
type SystemIdentifier = (String,Integer)
type SystemQueue = Map SystemIdentifier System

-- @-node:gcross.20090224112926.18:<< Types >>
-- @nl

-- @+others
-- @+node:gcross.20090309112253.6:invertArray
invertArray arr =
    let bnds = IArray.bounds arr
    in IArray.array bnds $ ((map swap).IArray.assocs) arr
-- @-node:gcross.20090309112253.6:invertArray
-- @+node:gcross.20090304103044.24:emptySystem
emptySystem :: Int -> System
emptySystem n = System (listArray (0,n-1) (repeat [])) (listArray (0,n-1) (repeat []))
-- @-node:gcross.20090304103044.24:emptySystem
-- @+node:gcross.20090224112926.7:computeVertexNumber
computeVertexNumber :: [Pauli] -> Integer
computeVertexNumber (X:Z:rest) = paulisToSequenceNumber rest
computeVertexNumber (X:X:rest) = 3^(length rest) + computeVertexNumber (X:rest)
computeVertexNumber _ = error "vertex must be in canonical form -- i.e., first pauli must be X, and after the first string of X's there must follow a Z"
-- @-node:gcross.20090224112926.7:computeVertexNumber
-- @+node:gcross.20090224112926.20:computeVertexCombinationCount
computeVertexCombinationCount :: [Pauli] -> Integer
computeVertexCombinationCount = (\x -> (3^(x-1)-1)`div`2) . length
-- @-node:gcross.20090224112926.20:computeVertexCombinationCount
-- @+node:gcross.20090224112926.17:computeSystemNumber
computeSystemNumber :: System -> Integer
computeSystemNumber (System _ interactions)  = 
    let interactions_as_list = elems interactions
        vertex_bases = ((scanl (*) 1).(map computeVertexCombinationCount)) interactions_as_list
    in (sum.(zipWith (*) vertex_bases).(map computeVertexNumber)) interactions_as_list
-- @-node:gcross.20090224112926.17:computeSystemNumber
-- @+node:gcross.20090224112926.25:computeSystemGraphName
computeSystemGraphName :: System -> String
computeSystemGraphName (System graph _) =
    assert ((toGraph6.undirG) graph == toGraph6 graph) $
    assert ((snd.canonicGraph) graph == graph) $
    assert ((toGraph6.snd.canonicGraph.undirG) graph == toGraph6 graph) $
    toGraph6 graph
-- @-node:gcross.20090224112926.25:computeSystemGraphName
-- @+node:gcross.20090304103044.19:computeTransformedSystems
computeTransformedSystems :: System -> SystemQueue
computeTransformedSystems = (Map.fromList).(map canonicalRepresentation).computeGraphTransformations
-- @-node:gcross.20090304103044.19:computeTransformedSystems
-- @+node:gcross.20090304103044.21:computeEquivalentIdentifiers
computeEquivalentIdentifiers :: System -> [SystemIdentifier]
computeEquivalentIdentifiers = (processQueue Map.empty).(uncurry Map.singleton).canonicalRepresentation
    where
        processQueue :: (Map SystemIdentifier ()) -> SystemQueue -> [SystemIdentifier]
        processQueue processed_systems queued_systems =
            let popped :: Maybe ((SystemIdentifier,System),SystemQueue)
                popped = Map.minViewWithKey queued_systems
            in case popped of
                Nothing -> Map.keys processed_systems
                Just ((popped_system_identifier,popped_system),remaining_queued_systems) ->
                    let next_queued_systems = (
                                (remaining_queued_systems `Map.union`)
                                .
                                (`Map.difference` processed_systems)
                                .
                                computeTransformedSystems
                            ) popped_system
                        next_processed_systems = Map.insert popped_system_identifier () processed_systems
                    in -- debugging code starts here
                    let (name,_) = popped_system_identifier
                        System graph _ = popped_system
                    in assert (name == (toGraph6.snd.canonicGraph) graph) $
                       assert (name == (toGraph6.snd.canonicGraph.fromGraph6) name) $
                        processQueue next_processed_systems next_queued_systems
-- @-node:gcross.20090304103044.21:computeEquivalentIdentifiers
-- @+node:gcross.20090304103044.20:canonicalizeSystem
canonicalizeSystem :: System -> System
canonicalizeSystem system =
    let (System graph _) = system
        (automorphism_generators,(canonicalize_graph_permutation,canonical_graph)) = (automorphisms.undirG) graph
        canonical_graph_name = toGraph6 canonical_graph
    in assert (Data.Graph.Automorphism.canonicGraph graph == canonical_graph) $
       assert (undirG graph == graph) $
       assert (undirG canonical_graph == canonical_graph) $
       assert (applyPerm canonicalize_graph_permutation graph == canonical_graph) $
       (
        (\system -> let (System graph _) = system in assert (graph == canonical_graph) $ system)
        .
        snd
        .
        (minimumBy (compare `on` fst))
        .
        (\systems -> assert ((all (\(_,System graph _) -> graph == canonical_graph)) systems) $ systems)
        .
        (map (
            (\permuted_system -> (computeSystemNumber permuted_system,permuted_system))
            .
            (\(System graph interactions) -> assert (undirG graph == graph) $ (System graph interactions))
            .
            (permuteSystem system)
        ))
        .
        (map (\permutation -> let (System graph _ ) = permuteSystem system permutation in
                        assert (undirG graph == graph) $
                        assert (graph == canonical_graph) $
                        permutation))
        .
        (map (multiplyPermutationByPermutation canonicalize_graph_permutation))
        .
        forestToPermutationList
        .
        computeForestFromGenerators
        .
        (\permutations -> assert ((length.nub.(map (computeSystemGraphName.(permuteSystem system).(multiplyPermutationByPermutation canonicalize_graph_permutation)))) permutations == 1) $ permutations)
    ) automorphism_generators

-- @-node:gcross.20090304103044.20:canonicalizeSystem
-- @+node:gcross.20090304103044.22:canonicalRepresentation
canonicalRepresentation :: System -> (SystemIdentifier,System)
canonicalRepresentation old_system =
    let system = canonicalizeSystem old_system
        number = computeSystemNumber system
        name = computeSystemGraphName system
    in
    let (System graph _) = system
        canonical_graph = (snd.canonicGraph) graph
        canonical_name = toGraph6 canonical_graph
    in assert (name == canonical_name) $
       assert (graph == canonical_graph) $
       assert ((undirG.fromGraph6.toGraph6) graph == graph) $
        ((name,number),system)
-- @-node:gcross.20090304103044.22:canonicalRepresentation
-- @+node:gcross.20090304103044.23:codeToSystem
codeToSystem :: Code -> System
codeToSystem code =
    let (min,max) = (bounds.codeGraph) code
        n = max-min+1
    in ((foldl uncurriedAddEdge (emptySystem n)).codeInteractions) code
    where
        uncurriedAddEdge :: System -> (Vertex,Pauli,Vertex,Pauli) -> System
        uncurriedAddEdge system (v1,p1,v2,p2) = addEdge system v1 p1 v2 p2
-- @-node:gcross.20090304103044.23:codeToSystem
-- @+node:gcross.20090224112926.14:paulisToSequenceNumber
paulisToSequenceNumber :: [Pauli] -> Integer
paulisToSequenceNumber = sum . (zipWith (*) (iterate (*3) 1)) . (map (toInteger.(\x -> x-1).fromEnum))
-- @-node:gcross.20090224112926.14:paulisToSequenceNumber
-- @+node:gcross.20090224112926.23:permuteSystem
permuteSystem :: System -> Permutation -> System
permuteSystem (System graph interactions) permutation =
    let permuted_graph = applyPerm permutation graph
        permutation_bounds = bounds permutation
        inverse_permutation = array permutation_bounds [(permutation!x,x) | x <- range permutation_bounds]
        convertPaulisAt permuted_graph_index =
            let permuted_vertex = permuted_graph ! permuted_graph_index
                original_graph_index = inverse_permutation ! permuted_graph_index
                original_vertex = graph ! original_graph_index
                original_paulis = interactions ! original_graph_index
                permuted_paulis = assert (length original_paulis == length original_vertex) $
                    map ((original_paulis !!).fromJust.((flip elemIndex) original_vertex).(inverse_permutation !)) permuted_vertex
                first_pauli:second_pauli:_ = nub permuted_paulis
                third_pauli = (head.(delete first_pauli).(delete second_pauli)) [X,Y,Z]
                pauli_map = array (1,3) [(fromEnum first_pauli,X),(fromEnum second_pauli,Z),(fromEnum third_pauli,Y)]
            in map ((pauli_map!).fromEnum) permuted_paulis
        permuted_interactions = array permutation_bounds [(i,convertPaulisAt i) | i <- range permutation_bounds]
    in System permuted_graph permuted_interactions
-- @-node:gcross.20090224112926.23:permuteSystem
-- @+node:gcross.20090226131439.6:deleteEdge
-- note that resulting system is NOT in canonical form!
deleteEdge :: System -> Vertex -> Vertex -> Maybe System
deleteEdge (System graph interactions) v1 v2 =
    let (new_v1_adjacents,new_v1_paulis) = processVertex v1 v2
        (new_v2_adjacents,new_v2_paulis) = processVertex v2 v1
        new_graph = graph // [(v1,new_v1_adjacents),(v2,new_v2_adjacents)]
        new_interactions = interactions // [(v1,new_v1_paulis),(v2,new_v2_paulis)]
    in if (length new_v1_adjacents < 2) || (length new_v2_adjacents < 2)
        then Nothing
        else Just $ System new_graph new_interactions
    where
        deleteElemIndex :: [a] -> Int -> [a]
        deleteElemIndex list index = (take index list) ++ (drop (index+1) list)

        processVertex target other =
            let target_adjacents = (graph!target)
                target_paulis = (interactions!target)
                index_of_other = fromJust $ elemIndex other target_adjacents
                new_target_adjacents = target_adjacents `deleteElemIndex` index_of_other
                new_target_paulis = target_paulis `deleteElemIndex` index_of_other
            in (new_target_adjacents,new_target_paulis)
-- @-node:gcross.20090226131439.6:deleteEdge
-- @+node:gcross.20090304103044.9:getEdge
getEdge :: System -> Vertex -> Vertex -> Maybe (Pauli,Pauli)
getEdge (System graph interactions) v1 v2 =
    do -- Maybe monad
        pauli_1 <- getPauli v2 v1
        pauli_2 <- getPauli v1 v2
        return (pauli_1,pauli_2)
    where
        getPauli v1 v2 =
            let v2_adjacents = graph!v2
                v2_paulis = interactions!v2
            in do -- Maybe monad
                index <- elemIndex v1 v2_adjacents
                return (v2_paulis!!index)
-- @-node:gcross.20090304103044.9:getEdge
-- @+node:gcross.20090227105300.2:addEdge
-- note that resulting system is NOT in canonical form!
addEdge :: System -> Vertex -> Pauli -> Vertex -> Pauli -> System
addEdge (System graph interactions) v1 p1 v2 p2 =
    let (new_v1_adjacents,new_v1_paulis) = processVertex v1 p1 v2
        (new_v2_adjacents,new_v2_paulis) = processVertex v2 p2 v1
        new_graph = graph // [(v1,new_v1_adjacents),(v2,new_v2_adjacents)]
        new_interactions = interactions // [(v1,new_v1_paulis),(v2,new_v2_paulis)]
    in  assert (not (v1 == v2)) $
        assert (undirG graph == graph) $
        assert (undirG new_graph == new_graph) $ 
        System new_graph new_interactions
    where
        insertElementAt :: [a] -> a -> Int -> [a]
        insertElementAt list element index =  (take index list) ++ (element:(drop index list))

        processVertex target target_pauli other =
            let target_adjacents = (graph!target)
                target_paulis = (interactions!target)
            in case findIndex (> other) target_adjacents of
                    Just index -> (
                        insertElementAt target_adjacents other index,
                        insertElementAt target_paulis target_pauli index
                        )
                    Nothing -> (
                        target_adjacents ++ [other],
                        target_paulis ++ [target_pauli]
                        )
-- @-node:gcross.20090227105300.2:addEdge
-- @+node:gcross.20090304103044.2:walkPairs
walkPairs [] = []
walkPairs (_:[]) = []
walkPairs (head:tail) = [(head,elem) | elem <- tail] ++ walkPairs tail
-- @-node:gcross.20090304103044.2:walkPairs
-- @+node:gcross.20090304103044.5:computeVertexTransformations
computeVertexTransformations :: System -> Vertex -> [System]
computeVertexTransformations system vertex = (concat.(map processPauli)) [X,Y,Z]
    where
        (System graph interactions) = system
        adjacents = graph!vertex
        paulis = interactions!vertex
        processPauli = concat.(map (uncurry processEdge)).walkPairs.(map (adjacents!!)).((flip elemIndices) paulis)
        processEdge i j =
            let Just (i_pauli,_) = getEdge system i vertex
                Just (j_pauli,_) = getEdge system j vertex
                system_with_edge_added = addEdge system i i_pauli j j_pauli
            in case getEdge system i j of
                Just (i_pauli_,j_pauli_) ->
                    if (i_pauli/=i_pauli_) || (j_pauli/=j_pauli_) || (vertex > i) || (vertex > j)
                        then []
                        else (catMaybes.(map (uncurry $ deleteEdge system))) [(i,j),(i,vertex),(j,vertex)]
                Nothing ->
                    system_with_edge_added:(catMaybes.(map (deleteEdge system_with_edge_added vertex))) [i,j]
-- @-node:gcross.20090304103044.5:computeVertexTransformations
-- @+node:gcross.20090304103044.6:computeGraphTransformations
computeGraphTransformations :: System -> [System]
computeGraphTransformations system =
    let (System graph _) = system
        (min,max) = bounds graph
    in (concat.(map (computeVertexTransformations system))) [min..max]
-- @-node:gcross.20090304103044.6:computeGraphTransformations
-- @-others

-- @-node:gcross.20090224112926.5:@thin Equivalence.hs
-- @-leo

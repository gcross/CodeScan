-- @+leo-ver=4-thin
-- @+node:gcross.20090224112926.9:@thin EquivalenceTests.hs
-- @@language haskell

module Main where

-- @<< Imports >>
-- @+node:gcross.20090224112926.10:<< Imports >>
import Test.HUnit
import Test.Framework
import Test.Framework.Providers.HUnit

import CodeQuest.CodeScan.Code
import CodeQuest.CodeScan.CodeGraph
import CodeQuest.CodeScan.Equivalence

import Control.Monad
import Data.Array
import Data.Graph.Construction
import Data.Maybe
import Text.Printf
-- @-node:gcross.20090224112926.10:<< Imports >>
-- @nl

-- @+others
-- @+node:gcross.20090224112926.12:Tests
-- @+others
-- @+node:gcross.20090224112926.13:maketest_paulisToSequenceNumber
maketest_paulisToSequenceNumber (input_paulis,correct_output_number) =
    let output_number = paulisToSequenceNumber input_paulis
    in assertEqual ("correct answer for " ++ show input_paulis ++ "?") correct_output_number output_number

paulisToSequenceNumber_test_data =
    [   ([],0)
    ,   ([X],0)
    ,   ([Z],1)
    ,   ([Y],2)
    ,   ([X,X],0)
    ,   ([Z,X],1)
    ,   ([Y,X],2)
    ,   ([X,Z],3)
    ,   ([Z,Z],4)
    ,   ([Y,Z],5)
    ,   ([X,Y],6)
    ,   ([Z,Y],7)
    ,   ([Y,Y],8)
    ]

test_paulisToSequenceNumber = mapM_ maketest_paulisToSequenceNumber paulisToSequenceNumber_test_data
-- @-node:gcross.20090224112926.13:maketest_paulisToSequenceNumber
-- @+node:gcross.20090224112926.11:maketest_computeVertexNumber
maketest_computeVertexNumber (input_paulis,correct_output_number) =
    let output_number = computeVertexNumber input_paulis
    in assertEqual ("correct answer for " ++ show input_paulis ++ "?") correct_output_number output_number

computeVertexNumber_test_data =
    [   ([X,Z],0)
    ,   ([X,Z,X],0)
    ,   ([X,Z,Z],1)
    ,   ([X,Z,Y],2)
    ,   ([X,X,Z],3)
    ,   ([X,Z,X,X],0)
    ,   ([X,Z,Z,X],1)
    ,   ([X,Z,Y,X],2)
    ,   ([X,Z,X,Z],3)
    ,   ([X,Z,Z,Z],4)
    ,   ([X,Z,Y,Z],5)
    ,   ([X,Z,X,Y],6)
    ,   ([X,Z,Z,Y],7)
    ,   ([X,Z,Y,Y],8)
    ,   ([X,X,Z,X],9)
    ,   ([X,X,Z,Z],10)
    ,   ([X,X,Z,Y],11)
    ,   ([X,X,X,Z],12)
    ]

test_computeVertexNumber = mapM_ maketest_computeVertexNumber computeVertexNumber_test_data
-- @-node:gcross.20090224112926.11:maketest_computeVertexNumber
-- @+node:gcross.20090224112926.21:maketest_computeVertexCombinationCount
maketest_computeVertexCombinationCount (input_paulis,correct_output_number) =
    let output_number = computeVertexCombinationCount input_paulis
    in assertEqual ("correct answer for " ++ show input_paulis ++ "?") correct_output_number output_number

computeVertexCombinationCount_test_data =
    [   ([X,Z],1)
    ,   ([X,Z,X],4)
    ,   ([X,Z,Z],4)
    ,   ([X,Z,X,X],13)
    ,   ([X,Z,Y,Z],13)
    ,   ([X,X,X,Z,Z],40)
    ,   ([X,X,X,Z,Z,Y],121)
    ]

test_computeVertexCombinationCount = mapM_ maketest_computeVertexCombinationCount computeVertexCombinationCount_test_data
-- @-node:gcross.20090224112926.21:maketest_computeVertexCombinationCount
-- @+node:gcross.20090224112926.19:maketest_computeSystemNumber
maketest_computeSystemNumber (input_paulis,correct_output_number) =
    let output_number = computeSystemNumber $ System undefined $ (listArray (0,length input_paulis-1)) input_paulis
    in assertEqual ("correct answer for " ++ show input_paulis ++ "?") correct_output_number output_number

computeSystemNumber_test_data =
    [   ([[X,Z],[X,Z]],0)
    ,   ([[X,Z,X],[X,Z,X]],0)
    ,   ([[X,Z,Z],[X,Z,X]],1)
    ,   ([[X,Z,Y],[X,Z,X]],2)
    ,   ([[X,X,Z],[X,Z,X]],3)
    ,   ([[X,Z,X],[X,Z,Z]],4)
    ,   ([[X,Z,Z],[X,Z,Z]],5)
    ,   ([[X,Z,Y],[X,Z,Z]],6)
    ,   ([[X,X,Z],[X,Z,Z]],7)
    ,   ([[X,Z,X],[X,Z,Y]],8)
    ,   ([[X,Z,Z],[X,Z,Y]],9)
    ,   ([[X,Z,Y],[X,Z,Y]],10)
    ,   ([[X,Z,Y],[X,X,Z]],14)
    ,   ([[X,X,Z],[X,X,Z]],15)
    ,   ([[X,Z],[X,X,Z]],3)
    ,   ([[X,Z],[X,X,X,Z]],12)
    ,   ([[X,X,X,Z],[X,Z,Z,Z]],12+13*4)
    ,   ([[X,Z],[X,X,Z],[X,X,X,Z]],0+3+12*4)
    ]

test_computeSystemNumber = mapM_ maketest_computeSystemNumber computeSystemNumber_test_data
-- @-node:gcross.20090224112926.19:maketest_computeSystemNumber
-- @+node:gcross.20090224112926.26:permuteSystem
-- @+node:gcross.20090224112926.27:square
maketest_permuteSystem_square permutation_as_list correct_graph_as_list =
    let system = System (listArray (0,3) [[1,3],[0,2],[1,3],[0,2]]) (listArray (0,3) [[X,Z],[X,Z],[X,Z],[X,Z]])
        permutation = listArray (0,3) permutation_as_list
        correct_graph = listArray (0,3) correct_graph_as_list
        correct_interactions = listArray (0,3) [[X,Z],[X,Z],[X,Z],[X,Z]]
        System permuted_graph permuted_interactions = permuteSystem system permutation
    in do
        assertEqual "is the permuted graph correct?" correct_graph permuted_graph
        assertEqual "are the permuted interactions correct?" correct_interactions permuted_interactions

test_permuteSystem_square_01 = maketest_permuteSystem_square [1,0,2,3] [[1,2],[0,3],[0,3],[1,2]]
test_permuteSystem_square_13 = maketest_permuteSystem_square [0,3,2,1] [[1,3],[0,2],[1,3],[0,2]]
test_permuteSystem_square_1230 = maketest_permuteSystem_square [1,2,3,0] [[1,3],[0,2],[1,3],[0,2]]
-- @-node:gcross.20090224112926.27:square
-- @+node:gcross.20090224112926.30:square with diagonal
maketest_permuteSystem_squareWithDiagonal permutation_as_list correct_graph_as_list correct_interactions_as_list =
    let system = System (listArray (0,3) [[1,2,3],[0,2],[0,1,3],[0,2]]) (listArray (0,3) [[X,Z,Y],[X,Z],[X,X,Z],[X,Z]])
        permutation = listArray (0,3) permutation_as_list
        correct_graph = listArray (0,3) correct_graph_as_list
        correct_interactions = listArray (0,3) correct_interactions_as_list
        System permuted_graph permuted_interactions = permuteSystem system permutation
    in do
        assertEqual "is the permuted graph correct?" correct_graph permuted_graph
        assertEqual "are the permuted interactions correct?" correct_interactions permuted_interactions

test_permuteSystem_squareWithDiagonal_01 = maketest_permuteSystem_squareWithDiagonal
                                                [1,0,2,3]
                                                [[1,2],[0,2,3],[0,1,3],[1,2]]
                                                [[X,Z],[X,Z,Y],[X,X,Z],[X,Z]]

test_permuteSystem_squareWithDiagonal_03_12 = maketest_permuteSystem_squareWithDiagonal
                                                [3,2,1,0]
                                                [[1,3],[0,2,3],[1,3],[0,1,2]]
                                                [[X,Z],[X,Z,Z],[X,Z],[X,Z,Y]]
-- @-node:gcross.20090224112926.30:square with diagonal
-- @+node:gcross.20090224112926.31:square with both diagonals
maketest_permuteSystem_squareWithBothDiagonals permutation_as_list correct_interactions_as_list =
    let system = System (listArray (0,3) [[1,2,3],[0,2,3],[0,1,3],[0,1,2]]) (listArray (0,3) [[X,Z,Y],[X,X,Z],[X,Z,Z],[X,Z,X]])
        permutation = listArray (0,3) permutation_as_list
        correct_graph = listArray (0,3) [[1,2,3],[0,2,3],[0,1,3],[0,1,2]]
        correct_interactions = listArray (0,3) correct_interactions_as_list
        System permuted_graph permuted_interactions = permuteSystem system permutation
    in do
        assertEqual "is the permuted graph correct?" correct_graph permuted_graph
        assertEqual "are the permuted interactions correct?" correct_interactions permuted_interactions

test_permuteSystem_squareWithBothDiagonals_01 = maketest_permuteSystem_squareWithBothDiagonals
                                                [1,0,2,3]
                                                [[X,X,Z],[X,Z,Y],[X,Z,X],[X,Z,Z]]

test_permuteSystem_squareWithBothDiagonals_23 = maketest_permuteSystem_squareWithBothDiagonals
                                                [0,1,3,2]
                                                [[X,Z,Y],[X,Z,X],[X,Z,X],[X,Z,Z]]

test_permuteSystem_squareWithBothDiagonals_03 = maketest_permuteSystem_squareWithBothDiagonals
                                                [3,1,2,0]
                                                [[X,Z,Z],[X,Z,Z],[X,X,Z],[X,Z,Y]]
-- @-node:gcross.20090224112926.31:square with both diagonals
-- @+node:gcross.20090226131439.3:house
maketest_permuteSystem_house permutation_as_list correct_graph_as_list correct_interactions_as_list =
    let system = System 
                    (listArray (0,4) [[1,3,4],[0,2,3,4],[1,3],[0,1,2,4],[0,1,3]])
                    (listArray (0,4) [[X,X,Z],[X,Z,Y,Z],[X,Z],[X,Z,Y,Y],[X,Z,Y]])
        permutation = listArray (0,4) permutation_as_list
        correct_graph = listArray (0,4) correct_graph_as_list
        correct_interactions = listArray (0,4) correct_interactions_as_list
        System permuted_graph permuted_interactions = permuteSystem system permutation
    in do
        assertEqual "is the permuted graph correct?" correct_graph permuted_graph
        assertEqual "are the permuted interactions correct?" correct_interactions permuted_interactions

test_permuteSystem_house_03 = maketest_permuteSystem_house
                                                [3,1,2,0,4]
                                                [[1,2,3,4],[0,2,3,4],[0,1],[0,1,4],[0,1,3]]
                                                [[X,Z,Y,Z],[X,Z,Y,Z],[X,Z],[X,X,Z],[X,Z,Y]]

test_permuteSystem_house_14 = maketest_permuteSystem_house
                                                [0,4,2,3,1]
                                                [[1,3,4],[0,3,4],[3,4],[0,1,2,4],[0,1,2,3]]
                                                [[X,Z,Z],[X,Z,Y],[X,Z],[X,Z,Z,Y],[X,Z,Z,Y]]
-- @-node:gcross.20090226131439.3:house
-- @-node:gcross.20090224112926.26:permuteSystem
-- @+node:gcross.20090226131439.7:deleteEdge
-- @+node:gcross.20090226131439.8:square with diagonal
maketest_deleteEdge_squareWithDiagonal vertex1 vertex2 correct_graph_as_list correct_interactions_as_list =
    let system = System (listArray (0,3) [[1,2,3],[0,2],[0,1,3],[0,2]]) (listArray (0,3) [[X,Z,Y],[X,Z],[X,X,Z],[X,Z]])
        correct_graph = listArray (0,3) correct_graph_as_list
        correct_interactions = listArray (0,3) correct_interactions_as_list
        maybe_output_system = deleteEdge system vertex1 vertex2
    in do
        assertBool "was a new system returned?" (isJust maybe_output_system)
        let Just (System permuted_graph permuted_interactions) = maybe_output_system
        assertEqual "is the new graph correct?" correct_graph permuted_graph
        assertEqual "are the new interactions correct?" correct_interactions permuted_interactions

test_deleteEdge_squareWithDiagonal_02 = maketest_deleteEdge_squareWithDiagonal
                                                0 2
                                                [[1,3],[0,2],[1,3],[0,2]]
                                                [[X,Y],[X,Z],[X,Z],[X,Z]]

makenulltest_deleteEdge_squareWithDiagonal vertex1 vertex2  =
    let input_system = System (listArray (0,3) [[1,2,3],[0,2],[0,1,3],[0,2]]) (listArray (0,3) [[X,Z,Y],[X,Z],[X,X,Z],[X,Z]])
        maybe_output_system = deleteEdge input_system vertex1 vertex2
    in do
        assertBool "was no new system returned?" ((not.isJust) maybe_output_system)

test_deleteEdge_squareWithDiagonal_01 = makenulltest_deleteEdge_squareWithDiagonal 0 1

test_deleteEdge_squareWithDiagonal_12 = makenulltest_deleteEdge_squareWithDiagonal 1 2
-- @-node:gcross.20090226131439.8:square with diagonal
-- @-node:gcross.20090226131439.7:deleteEdge
-- @+node:gcross.20090227105300.3:addEdge
-- @+node:gcross.20090227105300.4:square
maketest_addEdge_square vertex1 pauli1 vertex2 pauli2 correct_graph_as_list correct_interactions_as_list =
    let system = System (listArray (0,3) [[1,3],[0,2],[1,3],[0,2]]) (listArray (0,3) [[X,Z],[X,Z],[X,Z],[X,Z]])
        correct_graph = listArray (0,3) correct_graph_as_list
        correct_interactions = listArray (0,3) correct_interactions_as_list
        System permuted_graph permuted_interactions = addEdge system vertex1 pauli1 vertex2 pauli2
    in do
        assertEqual "is the new graph correct?" correct_graph permuted_graph
        assertEqual "are the new interactions correct?" correct_interactions permuted_interactions

test_addEdge_square_02 = maketest_addEdge_square
                                                0 Z 2 X
                                                [[1,2,3],[0,2],[0,1,3],[0,2]]
                                                [[X,Z,Z],[X,Z],[X,X,Z],[X,Z]]


test_addEdge_square_13 = maketest_addEdge_square
                                                1 Y 3 Y
                                                [[1,3],[0,2,3],[1,3],[0,1,2]]
                                                [[X,Z],[X,Z,Y],[X,Z],[X,Y,Z]]
-- @-node:gcross.20090227105300.4:square
-- @-node:gcross.20090227105300.3:addEdge
-- @+node:gcross.20090304103044.3:walkPairs
-- @+node:gcross.20090304103044.4:test_walkPairs
test_walkPairs = forM_ walkPairs_test_data $ \(input,correct_output) ->
    let output = walkPairs input
    in assertEqual ("testing input " ++ show input) correct_output output

walkPairs_test_data =
    [   ([],[])
    ,   ([0],[])
    ,   ([0..1],[(0,1)])
    ,   ([0..2],[(0,1),(0,2),(1,2)])
    ,   ([0..3],[(0,1),(0,2),(0,3),(1,2),(1,3),(2,3)])
    ,   ([0..4],[(0,1),(0,2),(0,3),(0,4),(1,2),(1,3),(1,4),(2,3),(2,4),(3,4)])
    ]
-- @-node:gcross.20090304103044.4:test_walkPairs
-- @-node:gcross.20090304103044.3:walkPairs
-- @+node:gcross.20090304103044.7:computeVertexTransformations
-- @+node:gcross.20090304103044.8:empty
test_computeVertexTransformations_empty =
    let system3 = System ((undirG.fromGraph6) "Bw") ((listArray (0,2).(take 3).repeat) [X,X])
        system4 = System ((undirG.fromGraph6) "C]") ((listArray (0,3).(take 4).repeat) [X,Z]) 
    in do
        forM_ [0..2] $ \vertex -> let output = computeVertexTransformations system3 vertex
            in assertEqual (printf "compute transformations for vertex %i for 3-vertex graph" vertex) [] output
        forM_ [0..3] $ \vertex -> let output = computeVertexTransformations system4 vertex
            in assertEqual (printf "compute transformations for vertex %i for 4-vertex graph" vertex) [] output
-- @-node:gcross.20090304103044.8:empty
-- @+node:gcross.20090304103044.10:square
maketest_computeVertexTransformations_square vertex correct_output =
    let system = System (listArray (0,3) [[1,3],[0,2],[1,3],[0,2]]) (listArray (0,3) [[X,Z],[X,X],[X,Z],[Y,Y]])
        output = computeVertexTransformations system vertex
    in do
        assertEqual "is the output correct?" correct_output output

test_computeVertexTransformations_square_0 = maketest_computeVertexTransformations_square 0 []
test_computeVertexTransformations_square_1 = maketest_computeVertexTransformations_square 1
                                                    [   System
                                                            (listArray (0,3) [[1,2,3],[0,2],[0,1,3],[0,2]])
                                                            (listArray (0,3) [[X,X,Z],[X,X],[X,X,Z],[Y,Y]])
                                                    ]
test_computeVertexTransformations_square_2 = maketest_computeVertexTransformations_square 2 []
test_computeVertexTransformations_square_3 = maketest_computeVertexTransformations_square 3
                                                    [   System
                                                            (listArray (0,3) [[1,2,3],[0,2],[0,1,3],[0,2]])
                                                            (listArray (0,3) [[X,Z,Z],[X,X],[Z,X,Z],[Y,Y]])
                                                    ]
-- @-node:gcross.20090304103044.10:square
-- @+node:gcross.20090304103044.11:square with diagonal
maketest_computeVertexTransformations_squareWithDiagonal vertex correct_output =
    let system = System
            (listArray (0,3) [[1,2,3],[0,2],[0,1,3],[0,2]])
            (listArray (0,3) [[Y,Y,Y],[X,X],[Y,Z,Y],[Z,Z]])
        output = computeVertexTransformations system vertex
    in do
        assertEqual "is the output correct?" correct_output output

test_computeVertexTransformations_squareWithDiagonal_0 = maketest_computeVertexTransformations_squareWithDiagonal 0
    [ -- First, consider product of edges 1,2; this results in no systems
      -- Next, consider product of edges 1,3; this results in
        System
            (listArray (0,3) [[1,2,3],[0,2,3],[0,1,3],[0,1,2]])
            (listArray (0,3) [[Y,Y,Y],[X,X,X],[Y,Z,Y],[Z,Z,Z]])
    ,   System
            (listArray (0,3) [[2,3],[2,3],[0,1,3],[0,1,2]])
            (listArray (0,3) [[Y,Y],[X,X],[Y,Z,Y],[Z,Z,Z]])
    ,   System
            (listArray (0,3) [[1,2],[0,2,3],[0,1,3],[1,2]])
            (listArray (0,3) [[Y,Y],[X,X,X],[Y,Z,Y],[Z,Z]])
      -- Finally, consider products of edges 2,3;  this results in
    ,   System
            (listArray (0,3) [[1,3],[0,2],[1,3],[0,2]])
            (listArray (0,3) [[Y,Y],[X,X],[Z,Y],[Z,Z]])
    ]

test_computeVertexTransformations_squareWithDiagonal_1 = maketest_computeVertexTransformations_squareWithDiagonal 1 []
test_computeVertexTransformations_squareWithDiagonal_2 = maketest_computeVertexTransformations_squareWithDiagonal 2 []
test_computeVertexTransformations_squareWithDiagonal_3 = maketest_computeVertexTransformations_squareWithDiagonal 3 []
-- @nonl
-- @-node:gcross.20090304103044.11:square with diagonal
-- @+node:gcross.20090304103044.16:complex5 #1
maketest_computeVertexTransformations_complex5_1 vertex correct_output =
    let system = System
            (listArray (0,4) [[1,2,3,4],[0,2],[0,1,3],[0,2,4],[0,3]])
            (listArray (0,4) [[Y,X,X,Y],[Z,Y],[Y,Y,Y],[Z,Z,Z],[Z,Z]])
        output = computeVertexTransformations system vertex
    in do
        assertEqual "is the output correct?" correct_output output

test_computeVertexTransformations_complex5_1_0 = maketest_computeVertexTransformations_complex5_1 0
    [ -- First, consider product of edges 2,3
        System
            (listArray (0,4) [[1,2,3,4],[0,2],[0,1],[0,4],[0,3]])
            (listArray (0,4) [[Y,X,X,Y],[Z,Y],[Y,Y],[Z,Z],[Z,Z]])
    ,   System
            (listArray (0,4) [[1,3,4],[0,2],[1,3],[0,2,4],[0,3]])
            (listArray (0,4) [[Y,X,Y],[Z,Y],[Y,Y],[Z,Z,Z],[Z,Z]])
    ,   System
            (listArray (0,4) [[1,2,4],[0,2],[0,1,3],[2,4],[0,3]])
            (listArray (0,4) [[Y,X,Y],[Z,Y],[Y,Y,Y],[Z,Z],[Z,Z]])
      -- Finally, consider product of edges 1,4
    ,   System
            (listArray (0,4) [[1,2,3,4],[0,2,4],[0,1,3],[0,2,4],[0,1,3]])
            (listArray (0,4) [[Y,X,X,Y],[Z,Y,Z],[Y,Y,Y],[Z,Z,Z],[Z,Z,Z]])
    ,   System
            (listArray (0,4) [[2,3,4],[2,4],[0,1,3],[0,2,4],[0,1,3]])
            (listArray (0,4) [[X,X,Y],[Y,Z],[Y,Y,Y],[Z,Z,Z],[Z,Z,Z]])
    ,   System
            (listArray (0,4) [[1,2,3],[0,2,4],[0,1,3],[0,2,4],[1,3]])
            (listArray (0,4) [[Y,X,X],[Z,Y,Z],[Y,Y,Y],[Z,Z,Z],[Z,Z]])
    ]

test_computeVertexTransformations_complex5_1_1 = maketest_computeVertexTransformations_complex5_1 1 []

test_computeVertexTransformations_complex5_1_2 = maketest_computeVertexTransformations_complex5_1 2
    [ -- First, consider product of edges 0,1;  this results in no solutions
      -- Next, consider product of edges 0,3;  this also results in no solutions (since triangle is already done in vertex 0)
      -- Finally, consider product of edges 1,3
        System
            (listArray (0,4) [[1,2,3,4],[0,2,3],[0,1,3],[0,1,2,4],[0,3]])
            (listArray (0,4) [[Y,X,X,Y],[Z,Y,Y],[Y,Y,Y],[Z,Z,Z,Z],[Z,Z]])
    ,   System
            (listArray (0,4) [[1,2,3,4],[0,3],[0,3],[0,1,2,4],[0,3]])
            (listArray (0,4) [[Y,X,X,Y],[Z,Y],[Y,Y],[Z,Z,Z,Z],[Z,Z]])
    ,   System
            (listArray (0,4) [[1,2,3,4],[0,2,3],[0,1],[0,1,4],[0,3]])
            (listArray (0,4) [[Y,X,X,Y],[Z,Y,Y],[Y,Y],[Z,Z,Z],[Z,Z]])
    ]

test_computeVertexTransformations_complex5_1_3 = maketest_computeVertexTransformations_complex5_1 3
    [ -- First, consider product of edges 0,2;  this results in no solutions (since triangle is already done in vertex 0)
      -- Next, consider product of 0,4;  this results in no solutions
      -- Finally, consider product of 2,4
        System
            (listArray (0,4) [[1,2,3,4],[0,2],[0,1,3,4],[0,2,4],[0,2,3]])
            (listArray (0,4) [[Y,X,X,Y],[Z,Y],[Y,Y,Y,Y],[Z,Z,Z],[Z,Z,Z]])
    ,   System
            (listArray (0,4) [[1,2,3,4],[0,2],[0,1,4],[0,4],[0,2,3]])
            (listArray (0,4) [[Y,X,X,Y],[Z,Y],[Y,Y,Y],[Z,Z],[Z,Z,Z]])
    ,   System
            (listArray (0,4) [[1,2,3,4],[0,2],[0,1,3,4],[0,2],[0,2]])
            (listArray (0,4) [[Y,X,X,Y],[Z,Y],[Y,Y,Y,Y],[Z,Z],[Z,Z]])
    ]


test_computeVertexTransformations_complex5_1_4 = maketest_computeVertexTransformations_complex5_1 4 []
-- @-node:gcross.20090304103044.16:complex5 #1
-- @+node:gcross.20090304103044.18:complex5 #2
maketest_computeVertexTransformations_complex5_2 vertex correct_output =
    let system = System
            (listArray (0,4) [[1,2,3,4],[0,2],[0,1,3],[0,2,4],[0,3]])
            (listArray (0,4) [[Y,Y,Y,Y],[Z,Z],[Y,X,Z],[X,X,X],[X,Z]])
        output = computeVertexTransformations system vertex
    in do
        assertEqual "is the output correct?" correct_output output

test_computeVertexTransformations_complex5_2_0 = maketest_computeVertexTransformations_complex5_2 0
    [ -- First consider 1,2; no solutions due to edge conflict
      -- Next consider 1,3
        System
            (listArray (0,4) [[1,2,3,4],[0,2,3],[0,1,3],[0,1,2,4],[0,3]])
            (listArray (0,4) [[Y,Y,Y,Y],[Z,Z,Z],[Y,X,Z],[X,X,X,X],[X,Z]])
    ,   System
            (listArray (0,4) [[2,3,4],[2,3],[0,1,3],[0,1,2,4],[0,3]])
            (listArray (0,4) [[Y,Y,Y],[Z,Z],[Y,X,Z],[X,X,X,X],[X,Z]])
    ,   System
            (listArray (0,4) [[1,2,4],[0,2,3],[0,1,3],[1,2,4],[0,3]])
            (listArray (0,4) [[Y,Y,Y],[Z,Z,Z],[Y,X,Z],[X,X,X],[X,Z]])
      -- Next, consider 1,4
    ,   System
            (listArray (0,4) [[1,2,3,4],[0,2,4],[0,1,3],[0,2,4],[0,1,3]])
            (listArray (0,4) [[Y,Y,Y,Y],[Z,Z,Z],[Y,X,Z],[X,X,X],[X,X,Z]])
    ,   System
            (listArray (0,4) [[2,3,4],[2,4],[0,1,3],[0,2,4],[0,1,3]])
            (listArray (0,4) [[Y,Y,Y],[Z,Z],[Y,X,Z],[X,X,X],[X,X,Z]])
    ,   System
            (listArray (0,4) [[1,2,3],[0,2,4],[0,1,3],[0,2,4],[1,3]])
            (listArray (0,4) [[Y,Y,Y],[Z,Z,Z],[Y,X,Z],[X,X,X],[X,Z]])
      -- Next, consider 2,3;  no solutions due to edge conflict
      -- Next, consider 2,4
    ,   System
            (listArray (0,4) [[1,2,3,4],[0,2],[0,1,3,4],[0,2,4],[0,2,3]])
            (listArray (0,4) [[Y,Y,Y,Y],[Z,Z],[Y,X,Z,Y],[X,X,X],[X,X,Z]])
    ,   System
            (listArray (0,4) [[1,3,4],[0,2],[1,3,4],[0,2,4],[0,2,3]])
            (listArray (0,4) [[Y,Y,Y],[Z,Z],[X,Z,Y],[X,X,X],[X,X,Z]])
    ,   System
            (listArray (0,4) [[1,2,3],[0,2],[0,1,3,4],[0,2,4],[2,3]])
            (listArray (0,4) [[Y,Y,Y],[Z,Z],[Y,X,Z,Y],[X,X,X],[X,Z]])
      -- Finally, consider 3,4;  no solutions due to edge conflict
    ]

test_computeVertexTransformations_complex5_2_1 = maketest_computeVertexTransformations_complex5_2 1 []

test_computeVertexTransformations_complex5_2_2 = maketest_computeVertexTransformations_complex5_2 2 []

test_computeVertexTransformations_complex5_2_3 = maketest_computeVertexTransformations_complex5_2 3
    [ -- Only case with solutions is 2,4
        System
            (listArray (0,4) [[1,2,3,4],[0,2],[0,1,3,4],[0,2,4],[0,2,3]])
            (listArray (0,4) [[Y,Y,Y,Y],[Z,Z],[Y,X,Z,Z],[X,X,X],[X,Z,Z]])
    ,   System
            (listArray (0,4) [[1,2,3,4],[0,2],[0,1,4],[0,4],[0,2,3]])
            (listArray (0,4) [[Y,Y,Y,Y],[Z,Z],[Y,X,Z],[X,X],[X,Z,Z]])
    ,   System
            (listArray (0,4) [[1,2,3,4],[0,2],[0,1,3,4],[0,2],[0,2]])
            (listArray (0,4) [[Y,Y,Y,Y],[Z,Z],[Y,X,Z,Z],[X,X],[X,Z]])
    ]

test_computeVertexTransformations_complex5_2_4 = maketest_computeVertexTransformations_complex5_2 4 []

-- @-node:gcross.20090304103044.18:complex5 #2
-- @-node:gcross.20090304103044.7:computeVertexTransformations
-- @+node:gcross.20090304103044.12:computeGraphTransformations
-- @+node:gcross.20090304103044.13:empty
test_computeGraphTransformations_empty =
    let system3 = System ((undirG.fromGraph6) "Bw") ((listArray (0,2).(take 3).repeat) [X,X])
        system4 = System ((undirG.fromGraph6) "C]") ((listArray (0,3).(take 4).repeat) [X,Z]) 
    in do
        assertEqual "compute transformations for 3-vertex graph" [] (computeGraphTransformations system3)
        assertEqual "compute transformations for 4-vertex graph" [] (computeGraphTransformations system4)
-- @-node:gcross.20090304103044.13:empty
-- @+node:gcross.20090304103044.14:square
test_computeGraphTransformations_square =
    let system = System (listArray (0,3) [[1,3],[0,2],[1,3],[0,2]]) (listArray (0,3) [[X,Z],[X,X],[X,Z],[Y,Y]])
        output = computeGraphTransformations system
        correct_output = 
                [   System
                        (listArray (0,3) [[1,2,3],[0,2],[0,1,3],[0,2]])
                        (listArray (0,3) [[X,X,Z],[X,X],[X,X,Z],[Y,Y]])
                ,   System
                        (listArray (0,3) [[1,2,3],[0,2],[0,1,3],[0,2]])
                        (listArray (0,3) [[X,Z,Z],[X,X],[Z,X,Z],[Y,Y]])
                ]
    in do
        assertEqual "is the output correct?" correct_output output
-- @-node:gcross.20090304103044.14:square
-- @+node:gcross.20090304103044.15:square with diagonal
test_computeGraphTransformations_squareWithDiagonal =
    let system = System
            (listArray (0,3) [[1,2,3],[0,2],[0,1,3],[0,2]])
            (listArray (0,3) [[Y,Y,Y],[X,X],[Y,Z,Y],[Z,Z]])
        output = computeGraphTransformations system
        correct_output = 
            [ -- First, consider product of edges 1,2; this results in no systems
              -- Next, consider product of edges 1,3; this results in
                System
                    (listArray (0,3) [[1,2,3],[0,2,3],[0,1,3],[0,1,2]])
                    (listArray (0,3) [[Y,Y,Y],[X,X,X],[Y,Z,Y],[Z,Z,Z]])
            ,   System
                    (listArray (0,3) [[2,3],[2,3],[0,1,3],[0,1,2]])
                    (listArray (0,3) [[Y,Y],[X,X],[Y,Z,Y],[Z,Z,Z]])
            ,   System
                    (listArray (0,3) [[1,2],[0,2,3],[0,1,3],[1,2]])
                    (listArray (0,3) [[Y,Y],[X,X,X],[Y,Z,Y],[Z,Z]])
              -- Finally, consider products of edges 2,3;  this results in
            ,   System
                    (listArray (0,3) [[1,3],[0,2],[1,3],[0,2]])
                    (listArray (0,3) [[Y,Y],[X,X],[Z,Y],[Z,Z]])
            ]
    in do
        assertEqual "is the output correct?" correct_output output
-- @-node:gcross.20090304103044.15:square with diagonal
-- @+node:gcross.20090304103044.17:complex5 #1
test_computeGraphTransformations_complex5_1 =
    let system = System
            (listArray (0,4) [[1,2,3,4],[0,2],[0,1,3],[0,2,4],[0,3]])
            (listArray (0,4) [[Y,X,X,Y],[Z,Y],[Y,Y,Y],[Z,Z,Z],[Z,Z]])
        output = computeGraphTransformations system
        correct_output =
            [   System
                    (listArray (0,4) [[1,2,3,4],[0,2],[0,1],[0,4],[0,3]])
                    (listArray (0,4) [[Y,X,X,Y],[Z,Y],[Y,Y],[Z,Z],[Z,Z]])
            ,   System
                    (listArray (0,4) [[1,3,4],[0,2],[1,3],[0,2,4],[0,3]])
                    (listArray (0,4) [[Y,X,Y],[Z,Y],[Y,Y],[Z,Z,Z],[Z,Z]])
            ,   System
                    (listArray (0,4) [[1,2,4],[0,2],[0,1,3],[2,4],[0,3]])
                    (listArray (0,4) [[Y,X,Y],[Z,Y],[Y,Y,Y],[Z,Z],[Z,Z]])
            ,   System
                    (listArray (0,4) [[1,2,3,4],[0,2,4],[0,1,3],[0,2,4],[0,1,3]])
                    (listArray (0,4) [[Y,X,X,Y],[Z,Y,Z],[Y,Y,Y],[Z,Z,Z],[Z,Z,Z]])
            ,   System
                    (listArray (0,4) [[2,3,4],[2,4],[0,1,3],[0,2,4],[0,1,3]])
                    (listArray (0,4) [[X,X,Y],[Y,Z],[Y,Y,Y],[Z,Z,Z],[Z,Z,Z]])
            ,   System
                    (listArray (0,4) [[1,2,3],[0,2,4],[0,1,3],[0,2,4],[1,3]])
                    (listArray (0,4) [[Y,X,X],[Z,Y,Z],[Y,Y,Y],[Z,Z,Z],[Z,Z]])
            ,   System
                    (listArray (0,4) [[1,2,3,4],[0,2,3],[0,1,3],[0,1,2,4],[0,3]])
                    (listArray (0,4) [[Y,X,X,Y],[Z,Y,Y],[Y,Y,Y],[Z,Z,Z,Z],[Z,Z]])
            ,   System
                    (listArray (0,4) [[1,2,3,4],[0,3],[0,3],[0,1,2,4],[0,3]])
                    (listArray (0,4) [[Y,X,X,Y],[Z,Y],[Y,Y],[Z,Z,Z,Z],[Z,Z]])
            ,   System
                    (listArray (0,4) [[1,2,3,4],[0,2,3],[0,1],[0,1,4],[0,3]])
                    (listArray (0,4) [[Y,X,X,Y],[Z,Y,Y],[Y,Y],[Z,Z,Z],[Z,Z]])
            ,   System
                    (listArray (0,4) [[1,2,3,4],[0,2],[0,1,3,4],[0,2,4],[0,2,3]])
                    (listArray (0,4) [[Y,X,X,Y],[Z,Y],[Y,Y,Y,Y],[Z,Z,Z],[Z,Z,Z]])
            ,   System
                    (listArray (0,4) [[1,2,3,4],[0,2],[0,1,4],[0,4],[0,2,3]])
                    (listArray (0,4) [[Y,X,X,Y],[Z,Y],[Y,Y,Y],[Z,Z],[Z,Z,Z]])
            ,   System
                    (listArray (0,4) [[1,2,3,4],[0,2],[0,1,3,4],[0,2],[0,2]])
                    (listArray (0,4) [[Y,X,X,Y],[Z,Y],[Y,Y,Y,Y],[Z,Z],[Z,Z]])
            ]
    in do
        assertEqual "is the output correct?" correct_output output
-- @-node:gcross.20090304103044.17:complex5 #1
-- @-node:gcross.20090304103044.12:computeGraphTransformations
-- @+node:gcross.20090309112253.2:canonicalizeSystem
-- @+node:gcross.20090309112253.3:square with diagonal
test_canonicalizeSystem_squareWithDiagonal =
    let system = System
            (listArray (0,3) [[1,2,3],[0,2],[0,1,3],[0,2]])
            (listArray (0,3) [[X,Z,X],[X,Z],[X,Z,X],[X,Z]])
        output = canonicalizeSystem system
        correct_output = System
            (listArray (0,3) [[2,3],[2,3],[0,1,3],[0,1,2]])
            (listArray (0,3) [[X,Z],[X,Z],[X,Z,X],[X,X,Z]])
    in do
        assertEqual "is the output correct?" correct_output output
-- @-node:gcross.20090309112253.3:square with diagonal
-- @-node:gcross.20090309112253.2:canonicalizeSystem
-- @-others
-- @nonl
-- @-node:gcross.20090224112926.12:Tests
-- @+node:gcross.20090224112926.29:Test list
tests = [   testCase "paulisToSequenceNumber" test_paulisToSequenceNumber
        ,   testCase "computeVertexNumber" test_computeVertexNumber
        ,   testCase "computeVertexCombinationCount" test_computeVertexCombinationCount
        ,   testCase "computeSystemNumber" test_computeSystemNumber
        ,   testGroup "permuteSystem"
                [   testGroup "square"
                        [   testCase "0 <-> 1" test_permuteSystem_square_01
                        ,   testCase "1 <-> 3" test_permuteSystem_square_13
                        ,   testCase "0 -> 1 -> 2 -> 3 -> 0" test_permuteSystem_square_1230
                        ]
                ,   testGroup "square with diagonal"
                        [   testCase "0 <-> 1" test_permuteSystem_squareWithDiagonal_01
                        ,   testCase "0 <-> 3, 1 <-> 2" test_permuteSystem_squareWithDiagonal_03_12
                        ]
                ,   testGroup "square with both diagonals"
                        [   testCase "0 <-> 1" test_permuteSystem_squareWithBothDiagonals_01
                        ,   testCase "0 <-> 3" test_permuteSystem_squareWithBothDiagonals_03
                        ,   testCase "2 <-> 3" test_permuteSystem_squareWithBothDiagonals_23
                        ]
                ,   testGroup "house"
                        [   testCase "0 <-> 3" test_permuteSystem_house_03
                        ,   testCase "1 <-> 4" test_permuteSystem_house_14
                        ]
                ]
        ,   testGroup "deleteEdge"
                [   testGroup "square with diagonal"
                        [   testCase "0 -- 1" test_deleteEdge_squareWithDiagonal_01
                        ,   testCase "0 -- 2" test_deleteEdge_squareWithDiagonal_02
                        ,   testCase "1 -- 2" test_deleteEdge_squareWithDiagonal_12
                        ]
                ]
        ,   testGroup "addEdge"
                [   testGroup "square"
                        [   testCase "0 -- 2" test_addEdge_square_02
                        ,   testCase "1 -- 3" test_addEdge_square_13
                        ]
                ]
        ,   testCase "walkPairs" test_walkPairs
        ,   testGroup "computeVertexTransformations"
                [   testCase "empty" test_computeVertexTransformations_empty
                ,   testGroup "square"
                    [   testCase "vertex 0" test_computeVertexTransformations_square_0
                    ,   testCase "vertex 1" test_computeVertexTransformations_square_1
                    ,   testCase "vertex 2" test_computeVertexTransformations_square_2
                    ,   testCase "vertex 3" test_computeVertexTransformations_square_3
                    ]
                ,   testGroup "square with diagonal"
                    [   testCase "vertex 0" test_computeVertexTransformations_squareWithDiagonal_0
                    ,   testCase "vertex 1" test_computeVertexTransformations_squareWithDiagonal_1
                    ,   testCase "vertex 2" test_computeVertexTransformations_squareWithDiagonal_2
                    ,   testCase "vertex 3" test_computeVertexTransformations_squareWithDiagonal_3
                    ]
                ,   testGroup "complex 5-vertex, #1"
                    [   testCase "vertex 0" test_computeVertexTransformations_complex5_1_0
                    ,   testCase "vertex 1" test_computeVertexTransformations_complex5_1_1
                    ,   testCase "vertex 2" test_computeVertexTransformations_complex5_1_2
                    ,   testCase "vertex 3" test_computeVertexTransformations_complex5_1_3
                    ,   testCase "vertex 4" test_computeVertexTransformations_complex5_1_4
                    ]
                ,   testGroup "complex 5-vertex, #2"
                    [   testCase "vertex 0" test_computeVertexTransformations_complex5_2_0
                    ,   testCase "vertex 1" test_computeVertexTransformations_complex5_2_1
                    ,   testCase "vertex 2" test_computeVertexTransformations_complex5_2_2
                    ,   testCase "vertex 3" test_computeVertexTransformations_complex5_2_3
                    ,   testCase "vertex 4" test_computeVertexTransformations_complex5_2_4
                    ]
                ]
        ,   testGroup "computeGrephTransformations"
                [   testCase "empty" test_computeGraphTransformations_empty
                ,   testCase "square" test_computeGraphTransformations_square
                ,   testCase "square with diagonal" test_computeGraphTransformations_squareWithDiagonal
                ,   testCase "complex 5-vertex, #1" test_computeGraphTransformations_complex5_1
                ]
        ,   testGroup "canonicalizeSystem"
                [   testCase "square with diagonal" test_computeGraphTransformations_squareWithDiagonal
                ]
    ]
-- @-node:gcross.20090224112926.29:Test list
-- @-others

main = defaultMain tests

-- @-node:gcross.20090224112926.9:@thin EquivalenceTests.hs
-- @-leo

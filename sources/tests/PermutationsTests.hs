-- @+leo-ver=4-thin
-- @+node:gcross.20081211214849.2:@thin PermutationsTests.hs
-- @@language haskell

{-# LANGUAGE TypeSynonymInstances #-}

module Main where

import Test.HUnit
import Test.Framework
import Test.Framework.Providers.HUnit
import Test.Framework.Providers.QuickCheck
import Test.QuickCheck

import CodeQuest.CodeScan.Permutations

import Data.Tree
import Data.List
import Data.Array.Unboxed
import Control.Monad
import Debug.Trace

-- @<< Types >>
-- @+node:gcross.20081212145859.15:<< Types >>
data WrappedDensePermutationForest = WrappedDensePermutationForest PermutationForest
        deriving (Eq,Show)

data WrappedSparsePermutationForest = WrappedSparsePermutationForest PermutationForest
        deriving (Eq,Show)
-- @-node:gcross.20081212145859.15:<< Types >>
-- @nl

-- @+others
-- @+node:gcross.20081212145859.5:Tests
-- @+others
-- @+node:gcross.20081212145859.4:Generators
-- @+node:gcross.20081212145859.13:(utilities)
popIndex :: Int -> [a] -> (a,[a])
popIndex index list =
    let (prefix, suffix) = splitAt index list
    in case suffix of
        [] -> error $ "pop out of bounds: " ++ show index
        head:tail -> (head,prefix++tail)

genShuffle :: [Int] -> Gen [Int]
genShuffle [] = return []
genShuffle list = do index <- choose (0,length list-1)
                     let (element,remainder) = popIndex index list
                     shuffled_remainder <- genShuffle remainder
                     return (element:shuffled_remainder)
-- @-node:gcross.20081212145859.13:(utilities)
-- @+node:gcross.20081212145859.11:Permutation
instance Arbitrary Permutation where
    arbitrary = sized $ \n -> if n <= 1 then return $ identityPermutation 1 else
                                do list <- genShuffle [0..n-1]
                                   size <- choose (0,n-1)
                                   let cycle = take size list
                                   return $ cycleToPermutation n cycle
    coarbitrary = fail "coarbitrary not implemented"
-- @-node:gcross.20081212145859.11:Permutation
-- @+node:gcross.20081212145859.12:PermutationForest
genForestWithDepth :: Int -> Gen PermutationForest
genForestWithDepth 0 = return ([])
genForestWithDepth depth = sized $ \n -> do list <- genShuffle [0..n-1]
                                            count <- choose(0,n-1)
                                            let values = sort $ take count list
                                            forests <- forM values (\value -> genForestWithDepth(n-1))
                                            return $ zipWith (\value -> \forest -> Node {
                                                                           rootLabel=value,
                                                                           subForest=forest
                                                                            }) values forests
genDenseForest :: Gen PermutationForest
genDenseForest = sized $ \n -> do forest <- genForestWithDepth n
                                  return forest


genSparseForest = sized $ \n -> do permutations <- forM [1..2*n] $ (\_ -> resize n (arbitrary::Gen Permutation))
                                   return $ mergeForests $ map permutationToForest permutations

instance Arbitrary WrappedDensePermutationForest where
    arbitrary = do forest <- genDenseForest
                   return (WrappedDensePermutationForest forest)
    coarbitrary = fail "coarbitrary not implemented"

instance Arbitrary WrappedSparsePermutationForest where
    arbitrary = do forest <- genSparseForest
                   return (WrappedSparsePermutationForest forest)
    coarbitrary = fail "coarbitrary not implemented"
-- @-node:gcross.20081212145859.12:PermutationForest
-- @-node:gcross.20081212145859.4:Generators
-- @+node:gcross.20081217190737.2:Permutations
-- @+node:gcross.20081211214849.3:multiplyPermutationByPermutation
-- @+node:gcross.20081212145859.7:specific instances
test_multiplyPermutationByPermutation = forM_ testCases (\(a,b,c) -> assertEqual ((show a) ++ " * " ++ (show b)) (multiplyPermutationByPermutation a b) c)
    where lTP = listToPermutation . (map ((+) (-1)))
          testCases = map (\(a,b,c) -> (lTP a, lTP b, lTP c)) [
             ([1,2,3],[1,2,3],[1,2,3]),
             ([5,4,1,2,3],[2,4,3,5,1],[4,2,1,3,5]),
             ([2,3,1],[1,3,2],[2,1,3]),
             ([2,1,3,5,4,6],[6,1,2,4,3,5],[6,2,1,5,3,4])
            ]
-- @-node:gcross.20081212145859.7:specific instances
-- @+node:gcross.20081212145859.8:random instances
prop_multiplyPermutationByPermutation_correct permutation1 permutation2 =
    let size = permutationLength permutation1
        computedResult = multiplyPermutationByPermutation permutation1 permutation2
    in all (\index -> (computedResult!index) == (permutation1!(permutation2!index)) ) [0..size-1]
    where types = (permutation1::Permutation,permutation2::Permutation)
-- @-node:gcross.20081212145859.8:random instances
-- @-node:gcross.20081211214849.3:multiplyPermutationByPermutation
-- @+node:gcross.20081212145859.3:forestToPermutationList
test_forestToPermutationList = forM_ testCases (\(forest,list) -> assertEqual (drawForest $ showForest forest) (forestToPermutationList forest) list)
    where testCases = [
             ([],[]),
             ([Node{rootLabel=1,subForest=[ Node{rootLabel=2,subForest=[]},
                                            Node{rootLabel=3,subForest=[]}
                                           ]},
               Node{rootLabel=4,subForest=[ Node{rootLabel=1,subForest=[]}
                                           ]}
               ],map listToPermutation [[1,2],[1,3],[4,1]])
            ]
-- @-node:gcross.20081212145859.3:forestToPermutationList
-- @+node:gcross.20081212145859.6:addPermutationToForest
-- @+node:gcross.20081212145859.10:forest contains permutation
prop_addPermutationToForest_forest_contains_permutation (WrappedDensePermutationForest forest) permutation =
    let new_forest = addPermutationToForest forest permutation
    in permutationContainedIn new_forest permutation
-- @-node:gcross.20081212145859.10:forest contains permutation
-- @+node:gcross.20081212145859.16:forest contains old elements
prop_addPermutationToForest_forest_contains_old_elements (WrappedSparsePermutationForest forest) permutation =
    let new_forest = addPermutationToForest forest permutation
    in all (permutationContainedIn new_forest) (forestToPermutationList forest)
-- @-node:gcross.20081212145859.16:forest contains old elements
-- @+node:gcross.20081212145859.17:forest not overcomplete
prop_addPermutationToForest_forest_not_overcomplete (WrappedSparsePermutationForest forest) permutation =
    let new_forest = addPermutationToForest forest permutation
    in all (\p -> permutationContainedIn forest p || permutation == p) (forestToPermutationList new_forest)
-- @-node:gcross.20081212145859.17:forest not overcomplete
-- @-node:gcross.20081212145859.6:addPermutationToForest
-- @+node:gcross.20081215152235.3:computeForestFromGenerators
-- @+node:gcross.20081215152235.4:same result as naive algorithm
genPermutations :: Gen [Permutation]
genPermutations = sized $ \n -> do i <- choose(0,min n 3)
                                   mapM (\_ -> arbitrary) [0..i] 

prop_computeForestFromGenerators_matches_naive = 
    forAll genPermutations $ \permutations -> ( (permutationLength $ head permutations) < 8 ==>
        let forest1 = computeForestFromGenerators permutations
            forest2 = computeForestFromGeneratorsUsingDimino permutations
        in forest1 == forest2)

-- @-node:gcross.20081215152235.4:same result as naive algorithm
-- @+node:gcross.20081215152235.7:random products contained in result
genPermutationsAndMultiplications :: Gen ([Permutation],Permutation)
genPermutationsAndMultiplications = sized $ \n -> do i <- choose(0,min n 3)
                                                     permutations <- mapM (\_ -> arbitrary) [0..i]
                                                     j <- choose(0,n)
                                                     indices <- mapM (\_ -> choose (0,i)) [0..j]
                                                     let permutation = foldr1 multiplyPermutationByPermutation $ map ((!!) permutations) indices
                                                     return (permutations,permutation)

prop_computeForestFromGenerators_contains_random_generator_products = 
    forAll genPermutationsAndMultiplications $ \(permutations,permutation) -> ( (permutationLength $ head permutations) < 8 ==>
        let forest = computeForestFromGenerators permutations
        in permutationContainedIn forest permutation)
-- @-node:gcross.20081215152235.7:random products contained in result
-- @+node:gcross.20081214194817.2:gets correct number of permutations
test_computeForestFromGenerators_has_correct_size =
    forM_ testCases (\(graph_size,correct_size,generator_cycles) -> 
        let generators = map (cyclesToPermutation graph_size) generator_cycles
            forest = computeForestFromGenerators generators
            forest_size = numberOfPermutationsInForest forest
        in assertEqual (show graph_size ++ ": " ++ show generator_cycles) correct_size forest_size)
    where
        testCases = [(8,16,[
                            [[2,3]],
                            [[0,1]],
                            [[0,2],[1,3]],
                            [[4,7],[5,6]]
                        ]),
                     (8,96,[
                            [[6,7]],
                            [[4,5]],
                            [[2,3]],
                            [[2,4],[3,5]],
                            [[0,1]],
                            [[0,2],[1,3]]
                        ]),
                     (6,2,[[[1,2],[3,4]]]),
                     (6,4,[
                            [[0,2]],
                            [[1,3],[4,5]]
                          ]),
                     (9,12,[
                            [[7,8]],
                            [[2,3]],
                            [[0,2]]
                           ])
                    ]
-- @-node:gcross.20081214194817.2:gets correct number of permutations
-- @-node:gcross.20081215152235.3:computeForestFromGenerators
-- @-node:gcross.20081217190737.2:Permutations
-- @-others

tests = [
            testCase "forestToPermutationList" test_forestToPermutationList,
            testGroup "multiplyPermutationByPermutation" [
                    testCase "specific instances" test_multiplyPermutationByPermutation,
                    testProperty "random instances" prop_multiplyPermutationByPermutation_correct
                ],
            testGroup "addPermutationToForest" [
                    testProperty "forest contains permutation" prop_addPermutationToForest_forest_contains_permutation,
                    testProperty "forest contains old elements" prop_addPermutationToForest_forest_contains_old_elements,
                    testProperty "forest is not overcomplete" prop_addPermutationToForest_forest_not_overcomplete
                ],
            testGroup "computeForestFromGenerators" [
                    testProperty "result matches naive algorithm" prop_computeForestFromGenerators_matches_naive,
                    testProperty "contains random generator products" prop_computeForestFromGenerators_contains_random_generator_products,
                    testCase "gets correct number of permutations" test_computeForestFromGenerators_has_correct_size
                ]
    ]
-- @-node:gcross.20081212145859.5:Tests
-- @-others

main = defaultMain tests
-- @-node:gcross.20081211214849.2:@thin PermutationsTests.hs
-- @-leo

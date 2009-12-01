-- @+leo-ver=4-thin
-- @+node:gcross.20081221143112.4:@thin CodeGraphTests.hs
-- @@language Haskell

module Main where

import Test.HUnit
import Test.Framework
import Test.Framework.Providers.HUnit

import Control.Monad

import Data.Graph
import Data.Array
import Data.List

import Debug.Trace

import CodeQuest.CodeScan.CodeGraph

-- @+others
-- @+node:gcross.20081221143112.5:Tests
-- @+others
-- @+node:gcross.20081221143112.6:test_fromGraph6
test_fromGraph6 (name,correct_graph) =
    let graph = fromGraph6 name
        nvertices = length correct_graph
    in do assertEqual ("correct number of vertices for '" ++ name ++ "'")
                      ((sort [0..nvertices-1]))
                      (sort $ vertices graph)
          forM_ [0..nvertices-1] (\index ->
                      assertEqual ("checking vertices adjacent to vertex " ++ (show index))(sort $ correct_graph !! index) (sort $ graph ! index))
-- @-node:gcross.20081221143112.6:test_fromGraph6
-- @-others

graphs = [
    ("E?~o",[[4..5],[4..5],[4..5],[4..5],[0..3],[0..3]]),
    ("F|A}_",[[1,2,3,5,6],[0,2,6],[0,1,3,5],[0,2,5,6],[5],[0,2,3,4],[0,1,3]]),
    ("GP|~C[",[[2,4,6,7],[4..6],[0,3,4,5,6],[2,4,5],[0,1,2,3,5,7],[1,2,3,4,7],[0,1,2,7],[0,4,5,6]])
  ]

tests = [testCase name (test_fromGraph6 (name,vertices)) | (name,vertices) <- graphs]
-- @-node:gcross.20081221143112.5:Tests
-- @-others

main = defaultMain tests
-- @-node:gcross.20081221143112.4:@thin CodeGraphTests.hs
-- @-leo

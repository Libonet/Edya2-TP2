module ArrTests where

import Test.HUnit
import Seq
import Arr        (Arr)
import ArrSeq


s0, s1, s2, s3, s4 :: Arr Int
s0 = fromList []
s1 = fromList [4]
s2 = fromList [5,1]
s3 = fromList [6,3,4]
s4 = fromList [6,3,4,7]

testLengthEmptySeq :: Test
testLengthEmptySeq = 
  TestCase $ assertEqual "Error on empty sequence length"
                         0 (lengthS s0)

testLengthNonEmptySeq :: Test
testLengthNonEmptySeq = 
  TestCase $ assertEqual "Error on non-empty sequence length"
                         2 (lengthS s2)

testMapEmptySeq :: Test
testMapEmptySeq = 
  TestCase $ assertEqual "Error on empty sequence map"
                         s0 (mapS (+1) s0)

testMapNonEmptySeq :: Test
testMapNonEmptySeq = 
  TestCase $ assertEqual "Error on non-empty sequence map"
                         (fromList [7,4,5]) (mapS (+1) s3)

testFilterEmptySeq :: Test
testFilterEmptySeq = 
  TestCase $ assertEqual "Error on empty sequence filter"
                         s0 (filterS (>3) s0)

testFilterNonEmptySeq :: Test
testFilterNonEmptySeq = 
  TestCase $ assertEqual "Error on non-empty sequence filter"
                         (fromList [6,4]) (filterS (>3) s3)


testReduceSumSeq0 :: Test
testReduceSumSeq0 = 
  TestCase $ assertEqual "Error reducing empty sequence"
                         0 (reduceS (+) 0 s0)

testReduceSumSeq3 :: Test
testReduceSumSeq3 = 
  TestCase $ assertEqual "Error reducing sequence of length 3"
                         13 (reduceS (+) 0 s3)

testScanSumSeq0 :: Test
testScanSumSeq0 = 
  TestCase $ assertEqual "Error on empty sequence scan"
                         (emptyS, 0) (scanS (+) 0 s0)

testScanSumSeq3 :: Test
testScanSumSeq3 = 
  TestCase $ assertEqual "Error on scan for sequence of length 3"
                         (fromList[0,6,9], 13) (scanS (+) 0 s3)


testScanSumSeq4 :: Test
testScanSumSeq4 = 
  TestCase $ assertEqual "Error on scan for sequence of length 4"
                         (fromList[0,6,9,13], 20) (scanS (+) 0 s4)

testsArray = 
  [
    testMapEmptySeq,
    testMapNonEmptySeq,
    testFilterEmptySeq,
    testFilterNonEmptySeq,
    testLengthEmptySeq,
    testLengthNonEmptySeq,
    testReduceSumSeq0,
    testReduceSumSeq3,
    testScanSumSeq0,
    testScanSumSeq3,
    testScanSumSeq4
  ]


main :: IO Counts
main = runTestTT $ TestList testsArray

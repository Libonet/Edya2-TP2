module ListTests where

import Test.HUnit
import Seq
import ListSeq

s0, s1, s2, s3, s4 :: [Int]
s0 = fromList []
s1 = fromList [4]
s2 = fromList [5,1]
s3 = fromList [6,3,4]
s4 = fromList [6,3,4,1]

testTabulate5 :: Test
testTabulate5 =
  TestCase $ assertEqual "Error on tabulate of size 5"
                        [0..4] (tabulateS id 5)

testTabulate5por2 :: Test
testTabulate5por2 =
  TestCase $ assertEqual "Error on tabulate of size 5 (function: 2*)"
                        [0,2..8] (tabulateS (2*) 5)

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

testScanSumSeq4 :: Test
testScanSumSeq4 =
  TestCase $ assertEqual "Error on scan for sequence of length 4"
                         (fromList [0,6,9,13], 14) (scanS (+) 0 s4)

testScanSumSeq3 :: Test
testScanSumSeq3 =
  TestCase $ assertEqual "Error on scan for sequence of length 3"
                         (fromList [0,6,9], 13) (scanS (+) 0 s3)


testsLists =
  [
    testTabulate5,
    testTabulate5por2,
    testMapEmptySeq,
    testMapNonEmptySeq,
    testLengthEmptySeq,
    testLengthNonEmptySeq,
    testReduceSumSeq0,
    testReduceSumSeq3,
    testScanSumSeq0,
    testScanSumSeq4,
    testScanSumSeq3
  ]


main :: IO Counts
main = runTestTT $ TestList testsLists

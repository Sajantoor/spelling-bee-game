{-# OPTIONS_GHC -F -pgmF htfpp #-}

module GameEngineTests (
  htf_thisModulesTests
) where

import Test.Framework
import Data.List
import GameEngine

test_toCandidateBasis :: IO ()
test_toCandidateBasis = do
    assertEqual (Just "abcdefg") (toCandidateBasis "gabcdef")
    assertEqual Nothing (toCandidateBasis "abcdefgh")
    assertEqual Nothing (toCandidateBasis "abcdef")

test_extractBases :: IO ()
test_extractBases = do
    assertEqual ["abcdefg"] (extractBases ["abcdefg"])
    assertEqual [] (extractBases ["abcdefg","gabcdef"])
    assertEqual ["abcdefg"] (extractBases ["bcdefg","gabcdef"])

test_basisToPuzzle :: IO ()
test_basisToPuzzle = do
    let (c0,rest0) = basisToPuzzle "abcdefg" 0
    assertEqual ('a',"bcdefg") (c0,sort rest0)
    let (c1,rest1) = basisToPuzzle "abcdefg" 1
    assertEqual ('b',"acdefg") (c1,sort rest1)
    let (c2,rest2) = basisToPuzzle "abcdefg" 2
    assertEqual ('c',"abdefg") (c2,sort rest2)

test_isWordCorrect :: IO ()
test_isWordCorrect = do
  assertEqual True (isWordCorrect ["abc"] ('a',"bcdefg") "abc")
  assertEqual False (isWordCorrect [] ('a',"bcdefg") "abc")
  assertEqual False (isWordCorrect ["bc"] ('a',"bcdefg") "bc")
  assertEqual False (isWordCorrect ["abch"] ('a',"bcdefg") "abch")

test_allAnswers :: IO ()
test_allAnswers = do
    assertEqual ["abc","abcdefg"] (sort (allAnswers ["abc","abcdefg"] ('a',"bcdefg")))
    assertEqual ["abc","abcdefg"] (sort (allAnswers ["abc","abcdefg","zxc"] ('a',"bcdefg")))

test_finalScore :: IO ()
test_finalScore = do
    assertEqual Zero (finalScore ["abc","abcdefg"] ('a',"bcdefg") [])
    assertEqual Good (finalScore ["abc","abcdefg","zxc"] ('a',"bcdefg") ["abc"])
    assertEqual Perfect (finalScore ["abc","abcdefg","zxc"] ('a',"bcdefg") ["abc","abcdefg"])
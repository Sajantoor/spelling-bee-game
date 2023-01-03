{-# LANGUAGE NoImplicitPrelude #-}
{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}

module GameEngine
  ( Score (..),
    toCandidateBasis,
    extractBases,
    basisToPuzzle,
    isWordCorrect,
    allAnswers,
    finalScore,
  )
where

import Helpers
import Prelude hiding (filter, foldl, foldr, init, length, map)

data Score = Zero | Bad | OK | Good | Great | Perfect
  deriving (Eq, Show)

type Dictionary = [String]

type Basis = [Char]

type Puzzle = (Char, [Char])

-- Turns a string into a canidate basis
-- Canadiate basis - list of 7 characters put in order with no duplicates
-- If not exactly 7 distinct characters are used, return Nothing
toCandidateBasis :: String -> Maybe Basis
toCandidateBasis str =
  let filteredString = dedupAndSort str
      lengthOfFilter = length filteredString
   in case lengthOfFilter of
        7 -> Just filteredString
        _ -> Nothing

-- The incoming string list corresponds to a dictionary of all words in the language.
-- In the game, this will (by default) be loaded from words.txt – a list of the 3000
-- most common English words. You should create the list of all bases from this list.
-- Recall, for a list of characters to a basis of a puzzle, it must be a valid basis
-- (in other words have exactly 7 distinct characters in the list, in order), and there
--  must be exactly one word corresponding to the in the dictionary.
extractBases :: [String] -> [String]
extractBases [] = []
extractBases string =
  removeDuplicates (extractBases' string [])
  where
    extractBases' :: [String] -> [Basis] -> [Basis]
    extractBases' [] result = result
    extractBases' (h : t) result =
      let candidateBasis = toCandidateBasis h
       in case candidateBasis of
            Nothing -> extractBases' t result
            Just basis -> extractBases' t (basis : result)
    removeDuplicates :: [String] -> [String]
    removeDuplicates [] = []
    -- removeDuplicates [h] = [h]
    removeDuplicates (h : t) =
      -- check if item exists in the tail otherwise, if so filter otherwise don't filter
      let doesHeadExist = exists (== h) t
       in if doesHeadExist then removeDuplicates (filter (/= h) t) else h : removeDuplicates t

-- basisToPuzzle. Given a basis, and a number from 0 to 6, return a puzzle. A puzzle
-- consists of two parts, a Char, and a list of Chars. The first Char should be the character at the provided
-- index. The list of Chars should be 6 Chars long, and should consist of all Chars in the basis except the one
-- at the provided index.
basisToPuzzle :: Basis -> Int -> Puzzle
basisToPuzzle (h : t) 0 = (h, t)
basisToPuzzle string i =
  let character = findIndex string i
      filteredString = filter (/= character) string
   in (character, filteredString)
  where
    findIndex :: String -> Int -> Char
    findIndex [] _ = error "Index out of bounds"
    findIndex (h : t) i =
      case i of
        0 -> h
        _ -> findIndex t (i - 1)

-- Return true when the following conditions are met, provided string must be in the dictionary
-- The provided string must contain only letters in the puzzle and must contain at least one instance of the designated char that is the fst element of the puzzle
isWordCorrect :: Dictionary -> Puzzle -> String -> Bool
isWordCorrect [] _ _ = False
isWordCorrect _ _ [] = False
isWordCorrect dict puzzle string =
  if (not (exists (== string) dict)) || (not (exists (== fst puzzle) string))
    then False
    else
      let basis = fst puzzle : snd puzzle
       in isWordCorrect' string basis
  where
    isWordCorrect' [] _ = True
    isWordCorrect' _ [] = False
    -- Filter letters fom the basis that are not in the string
    isWordCorrect' string (h : t) =
      let filteredString = filter (/= h) string
       in isWordCorrect' filteredString t

-- Calculates all valid answers to the puzzle
allAnswers :: Dictionary -> Puzzle -> [String]
allAnswers [] _ = []
allAnswers dict puzzle =
  allAnswers' dict puzzle []
  where
    allAnswers' [] _ result = result
    allAnswers' (h : t) puzzle result =
      if isWordCorrect [h] puzzle h
        then allAnswers' t puzzle (h : result)
        else allAnswers' t puzzle result

-- calculates how well the user did
-- if the user guessed 0% < p < 25%
-- of the correct answers, return Bad. If the user guessed 25% ≤ p < 50%, return OK. If the user guessed
-- 50% ≤ p < 75%, return Good. If the user guessed 75% ≤ p < 100%, return Great. If the user guessed all
-- the correct answers, return Perfect.
finalScore :: Dictionary -> Puzzle -> [String] -> Score
finalScore dict puzzle answers =
  let allAns = allAnswers dict puzzle
      score = divide (length answers) (length allAns) * 100
   in -- write this better
      case score of
        x | x == 0 -> Zero
        x | x < 25 -> Bad
        x | x < 50 -> OK
        x | x < 75 -> Good
        x | x < 100 -> Great
        _ -> Perfect
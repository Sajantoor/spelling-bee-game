module Main (main) where
    
import GameEngine
import System.IO
import Data.Char
import System.Random
import Text.Printf
import GameEngine (allAnswers)

-- https://stackoverflow.com/questions/65351055/how-do-i-generate-randomly-pick-an-element-in-a-list-in-haskell
randomElement :: [a] -> IO a
randomElement list = do
  gen <- getStdGen
  let (i, _) = randomR (0, length list - 1) gen
  return $ list !! i

main :: IO ()
main = do
    putStrLn "Input a newline-delimited dictionary file (empty for words.txt)"
    dfileInput <- getLine
    let dfileName = if null dfileInput then "words.txt" else dfileInput
    dfileContents <- readFile dfileName
    let dictionaryWords = lines (map toLower dfileContents)
    let puzzlesBases = extractBases dictionaryWords
    basis <- randomElement puzzlesBases
    elementIndex <- randomElement [0..6]
    let puzzle = basisToPuzzle basis elementIndex
    let (c,s) = puzzle
    putStrLn (printf "The puzzle is _%s_%s\nPlease enter your guesses as a space-separated list" [c] s)
    guessesInput <- getLine
    let guessesWords = words (map toLower guessesInput)
    let userScore = finalScore dictionaryWords puzzle guessesWords
    putStrLn (printf "All of the words were: %s" (show (allAnswers dictionaryWords puzzle)))
    putStrLn (printf "You got a score of: %s" (show userScore))

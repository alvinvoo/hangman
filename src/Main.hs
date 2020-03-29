module Main where

import Puzzle
import Data.Char (toLower)
import Data.Maybe (isJust)
import Control.Monad (forever) -- execute a function over and over again, until program exit or fail
import System.Exit (exitSuccess) -- exitSuccess :: IO a

-- buggy, game exits whenever wrong guessed word hit 7 irregardless
gameOver :: Puzzle -> IO ()
gameOver (Puzzle wordToGuess _ guessedWrong) =
  if (length guessedWrong) >= maxWrongGuess then
    do 
      putStrLn "You lose!"
      putStrLn $ "The word was: " ++ wordToGuess
      exitSuccess
  else return ()
        
gameWin :: Puzzle -> IO ()
gameWin (Puzzle _ filledInSoFar _) =
  if all isJust filledInSoFar then
    do 
      putStrLn "You win!"
      exitSuccess
  else return ()

runGame :: Puzzle -> IO ()
runGame puzzle = forever $ do
  gameOver puzzle
  gameWin puzzle
  putStrLn $ "Current puzzle is: " ++ show puzzle
  putStr "Guess a letter: "
  guess <- getLine
  case guess of
    [c] -> handleGuess puzzle c >>= runGame
    _ -> putStrLn "Your guess must\
                  \ be a single character"

main :: IO ()
main = do
  word <- randomWord'
  let puzzle = freshPuzzle (fmap toLower word)
  runGame puzzle
  


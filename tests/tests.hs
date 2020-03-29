module Main where

import Puzzle
import Test.Hspec

getDiscovered :: Puzzle -> [Maybe Char]
getDiscovered (Puzzle _ discovered _) = discovered

getWrongGuesses :: Puzzle -> [Char]
getWrongGuesses (Puzzle _ _ wrongGuesses) = wrongGuesses

main :: IO ()
main = hspec $ do
  describe "Addition" $ do
    it "1 + 1 is greater than 1" $ do
      (1 + 1) > (1 :: Int) `shouldBe` True

  describe "fillInCharacter function" $ do
    let testPuzzle = freshPuzzle "alvin"
    it "fills in correct guesses as `discovered`" $ do
      getDiscovered (fillInCharacter testPuzzle 'l') `shouldBe`  [Nothing, Just 'l', Nothing, Nothing, Nothing]
    it "does not fill in wrong guesses as `discovered`" $ do
      getDiscovered (fillInCharacter testPuzzle 'x') `shouldBe` [Nothing, Nothing, Nothing, Nothing, Nothing]
    it "fills in wrong guesses as `wrong guesses`" $ do
      getWrongGuesses (fillInCharacter testPuzzle 'x') `shouldBe` ['x']
    it "does not fill in correct guesses as `wrong guesses`" $ do
      getWrongGuesses (fillInCharacter testPuzzle 'n') `shouldBe` []

  describe "handlGuess function" $ do
    let testPuzzle = Puzzle "lollipop" [Just 'l', Nothing, Just 'l', Just 'l', Nothing, Nothing, Nothing, Nothing] ['a','e']
    it "returns same puzzle when character already guessed" $ do
      retPuzzle <- handleGuess testPuzzle 'l'
      retPuzzle `shouldBe` testPuzzle
    it "returns correctly filled puzzle when character is correct" $ do
      retPuzzle <- handleGuess testPuzzle 'o'
      retPuzzle `shouldBe` Puzzle "lollipop" [Just 'l', Just 'o', Just 'l', Just 'l', Nothing, Nothing, Just 'o', Nothing] ['a','e']
    it "returns puzzle with appended wrong guess when character is incorrect" $ do
      retPuzzle <- handleGuess testPuzzle 'x'
      retPuzzle `shouldBe` Puzzle "lollipop" [Just 'l', Nothing, Just 'l', Just 'l', Nothing, Nothing, Nothing, Nothing] ['x','a','e']
module MasterMind.MasterMind where

import Data.List
import Control.Monad
import Lib.Prompt

data Answer a = Answer { guess :: [a], correct :: Int, misplaced :: Int,
                         wrong :: Int } deriving Eq

instance (Show a) => Show (Answer a) where
    show (Answer g c m w) = "Assessment for guess: " ++ show g ++
                        "\nCorrect:   " ++ show c ++
                        "\nMisplaced: " ++ show m ++
                        "\nWrong:     " ++ show w

findCorrect :: (Eq a) => [a] -> [a] -> [a]
findCorrect [] _ = []
findCorrect _ [] = []
findCorrect (p:pattern) (g:guess)
    | p == g = p : findCorrect pattern guess
    |otherwise = findCorrect pattern guess

countMisplaced :: (Eq a) => [a] -> [a] -> Int
countMisplaced [] _ = 0
countMisplaced _ [] = 0
countMisplaced (p:pat) guess
    | p `elem` guess = 1 + countMisplaced pat (delete p guess)
    | otherwise = 0 + countMisplaced pat guess

masterMind :: (Eq a) => [a] -> [a] -> Answer a
masterMind pattern guess =
    let cor = findCorrect pattern guess
        correct = length cor
        misplaced = countMisplaced (pattern \\ cor) (guess \\ cor)
        total = max (length pattern) (length guess) in
        Answer guess correct misplaced (total - correct - misplaced)

playMM :: String -> Int -> IO String
playMM pattern tries = do
    result <- liftM (masterMind pattern) (runPrompt guessPrompt ())
    print result
    if correct result
    then return ("\nCorrect! The pattern is: " ++ show pattern ++ "\nYou won!")
    else if tries > 0
         then playMM pattern (tries - 1)
         else return ("Wrong! The pattern is: " ++ show pattern ++
                      "\nTry harder next time.")
    where
    guessPrompt = Prompt (\i -> "Guess the sequence. " ++ show tries ++
                " attempts remaining: ") id
    correct result = wrong result == 0 && misplaced result == 0

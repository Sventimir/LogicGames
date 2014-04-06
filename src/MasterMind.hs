module MasterMind where

import Data.List

data Answer a = Answer { guess :: [a], correct :: Int, misplaced :: Int,
                         wrong :: Int } deriving Eq

instance (Show a) => Show (Answer a) where
    show (Answer g c m w) = "Assessment for guess: " ++ show g ++
                        "\nCorrect:   " ++ show c ++
                        "\nMisplaced: " ++ show m ++
                        "\nWrong:     " ++ show w

-- TODO: Rewrite enabling tail recursion optimisation.
extractCorrect :: (Eq a) => [a] -> [a] -> ([a], [a], [a])
extractCorrect [] guess = ([], [], guess)
extractCorrect pattern [] = ([], pattern, [])
extractCorrect (p:pat) (g:guess)
    | p == g = (p : newCorrect, newPat, newGuess)
    | otherwise = (newCorrect, p : newPat, g : newGuess)
        where
        (newCorrect, newPat, newGuess) = extractCorrect pat guess

countMisplaced :: (Eq a) => [a] -> [a] -> Int
countMisplaced [] _ = 0
countMisplaced _ [] = 0
countMisplaced (p:pat) guess
    | p `elem` guess = 1 + countMisplaced pat (delete p guess)
    | otherwise = 0 + countMisplaced pat guess

masterMind :: (Eq a) => [a] -> [a] -> Answer a
masterMind pattern guess =
    let (cor, newPattern, newGuess) = extractCorrect guess pattern
        correct = length cor
        misplaced = countMisplaced newPattern newGuess
        total = max (length pattern) (length guess) in
        Answer guess correct misplaced (total - correct - misplaced)


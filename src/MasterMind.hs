module MasterMind where

import Data.List

data Answer = Answer { correct :: Int, misplaced :: Int, wrong :: Int }

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

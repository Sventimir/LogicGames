import System.Random
import Prompt
import MasterMind

passPromptText :: String -> String
passPromptText chrs = "Please enter a sequence of [" ++ chrs ++ 
                "] to be guessed or press [Enter] to generate a random one: "

main :: IO ()
main = let getRandom = take 5 . randomRs ('A', 'E') in do
    pat <- runPrompt (MaskedPrompt passPromptText id) "ABCDE"
    gen <- getStdGen
    pattern <- return (if null pat then getRandom gen else pat)
    playMM pattern 10 >>= putStrLn

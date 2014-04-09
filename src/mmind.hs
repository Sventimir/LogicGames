import System.IO
import System.Random
import Control.Exception
import MasterMind

getPattern :: IO String
getPattern = do
    putStrLn "Please enter a sequence of [ABCDE] to be guessed or press [Enter] to generate a random one: "
    hFlush stdout
    pass <- withEcho False getLine
    putChar '\n'
    return pass

withEcho :: Bool -> IO a -> IO a
withEcho echo action = do
    old <- hGetEcho stdin
    bracket_ (hSetEcho stdin echo) (hSetEcho stdin old) action

main :: IO ()
main = let getRandom = take 5 . randomRs ('A', 'E') in do
    pat <- getPattern
    gen <- getStdGen
    pattern <- return (if null pat then getRandom gen else pat)
    playMM pattern 10 >>= putStrLn

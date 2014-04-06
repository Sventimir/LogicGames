import System.IO
import Control.Exception
import MasterMind

getPattern :: IO String
getPattern = do
    putStrLn "Please enter a sequence of [ABCDE] to be guessed: "
    hFlush stdout
    pass <- withEcho False getLine
    putChar '\n'
    return pass

withEcho :: Bool -> IO a -> IO a
withEcho echo action = do
    old <- hGetEcho stdin
    bracket_ (hSetEcho stdin echo) (hSetEcho stdin old) action

main :: IO ()
main = do
    pattern <- getPattern
    playMM pattern 10 >>= putStrLn

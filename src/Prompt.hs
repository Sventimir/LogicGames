module Prompt where

import System.IO
import System.IO.Error
import Control.Monad
import Control.Exception

data Prompt o i = Prompt (o -> String) (String -> i) |
                  MaskedPrompt (o -> String) (String -> i)

handleEx :: IOError -> IO String
handleEx e
    | isEOFError e = return ""
    | otherwise = return "[ERROR]: Unexpected exception thrown."


runPrompt :: Prompt o i -> o -> IO i
runPrompt (Prompt ofun ifun) o = do
        putStr (ofun o)
        hFlush stdout
        liftM ifun (catch getLine handleEx)
runPrompt (MaskedPrompt ofun ifun) o = do
        putStrLn $ ofun o
        hFlush stdout
        pass <- withEcho False (catch getLine handleEx)
        putChar '\n'
        return $ ifun pass
    where
    withEcho :: Bool -> IO a -> IO a
    withEcho echo action = do
        old <- hGetEcho stdin
        bracket_ (hSetEcho stdin echo) (hSetEcho stdin old) action

examplePrompt = Prompt (const "Enter something: ") id

intPrompt = Prompt (const "Enter a number: ") read :: Prompt () Int

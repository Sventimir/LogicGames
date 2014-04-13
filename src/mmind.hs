import System.Random
import Data.Configurator
import qualified Data.Text as Txt
import Prompt
import MasterMind

passPromptText :: String -> String
passPromptText chrs = "Please enter a sequence of [" ++ chrs ++
                "] to be guessed or press [Enter] to generate a random one: "

randomize :: String -> Int -> IO String
randomize chars len = getStdGen
        >>= return . map (chars !!) . take len . randomRs (0, length chars - 1)

main :: IO ()
main = do
    cfg <- load [Optional "../cfg/mmind.cfg"]
    chars <- lookupDefault "ABCD" cfg (Txt.pack "chars")
    length <- lookupDefault 4 cfg (Txt.pack "length")
    pat <- runPrompt (MaskedPrompt passPromptText id) chars
    pattern <- if null pat then randomize chars length else return pat
    lookupDefault 10 cfg (Txt.pack "tries") >>= playMM pattern >>= putStrLn

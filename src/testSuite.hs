import System.Exit
import Test.HUnit
import MasterMind

testEnv = TestCase $ do
    1 @=? 1

testSuite = TestList [testEnv]

main = do
    counts <- runTestTT $ TestList [testSuite]
    if errors counts > 0
    then exitWith $ ExitFailure 2
    else if failures counts > 0
         then exitWith $ ExitFailure 1
         else exitSuccess

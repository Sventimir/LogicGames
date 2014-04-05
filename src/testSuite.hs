import System.Exit
import Test.HUnit
import MasterMind

testCorrectness = TestLabel "Test function counting correct guesses." $
        TestCase $ do
    ("", "", "") @=? extractCorrect "" ""
    ("", "A", "F") @=? extractCorrect "A" "F"
    ("X", "", "") @=? extractCorrect "X" "X"
    ("B", "ADEF", "CEAD") @=? extractCorrect "ABDEF" "CBEAD"
    ("HWDP", "X", "Y") @=? extractCorrect "XHWDP" "YHWDP"
    ("C", "ABDE", "EDBA") @=? extractCorrect "ABCDE" "EDCBA"

testMisplacement = TestLabel "Test function counting misplaced guesses." $
        TestCase $ do
    0 @=? countMisplaced "" ""
    0 @=? countMisplaced "ABCDE" "XF"
    2 @=? countMisplaced "EDCBA" "ABHJK"
    1 @=? countMisplaced "BFG" "ABC"
    3 @=? countMisplaced "ABXFZ" "XXFAA"

testSuite = TestList [testCorrectness, testMisplacement]

main = do
    counts <- runTestTT $ TestList [testSuite]
    if errors counts > 0
    then exitWith $ ExitFailure 2
    else if failures counts > 0
         then exitWith $ ExitFailure 1
         else exitSuccess

import System.Exit
import Test.HUnit
import MasterMind.MasterMind

testCorrectness = TestLabel "Test function counting correct guesses." $
        TestCase $ do
    "" @=? findCorrect "" ""
    "" @=? findCorrect "A" "F"
    "X" @=? findCorrect "X" "X"
    "B" @=? findCorrect "ABDEF" "CBEAD"
    "HWDP" @=? findCorrect "XHWDP" "YHWDP"
    "C" @=? findCorrect "ABCDE" "EDCBA"
    "" @=? findCorrect (replicate 999999 'A') (replicate 999999 'X')
    replicate 100 'C' @=? findCorrect (concat $ replicate 100 "ABCDE")
                                      (concat $ replicate 100 "EDCBA")

testMisplacement = TestLabel "Test function counting misplaced guesses." $
        TestCase $ do
    0 @=? countMisplaced "" ""
    0 @=? countMisplaced "ABCDE" "XF"
    2 @=? countMisplaced "EDCBA" "ABHJK"
    1 @=? countMisplaced "BFG" "ABC"
    3 @=? countMisplaced "ABXFZ" "XXFAA"

testMasterMind = TestLabel "Test main guess-assessment function." $ TestCase $ do
    Answer ""    0 0 0 @=? masterMind ""    ""
    Answer "A"   1 0 2 @=? masterMind "ABC" "A"
    Answer "CBA" 1 2 0 @=? masterMind "ABC" "CBA"
    Answer "SVN" 0 0 3 @=? masterMind "GIT" "SVN"
    Answer "ZUS" 2 0 1 @=? masterMind "ZU"  "ZUS"
    Answer "FBI" 3 0 0 @=? masterMind "FBI" "FBI"
    Answer "PPS" 2 0 1 @=? masterMind "PSS" "PPS"
    Answer "XX"  0 1 2 @=? masterMind "PSX" "XX"

testSuite = TestList [testCorrectness, testMisplacement, testMasterMind]

main = do
    counts <- runTestTT $ TestList [testSuite]
    if errors counts > 0
    then exitWith $ ExitFailure 2
    else if failures counts > 0
         then exitWith $ ExitFailure 1
         else exitSuccess

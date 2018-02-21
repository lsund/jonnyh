
import Protolude

import Test.HUnit

import Test.Moves


runTests :: IO Counts
runTests = runTestTT tests

main :: IO ()
main = runTests >>= print

module TestBestShuffle exposing (..)

import Expect exposing (Expectation)
import Fuzz exposing (Fuzzer, int, list, string)
import String exposing (toList)
import Test exposing (..)
import BestShuffle exposing (stringDiffScore, differingChars)


-- Task
-- Shuffle the characters of a string in such a way that as many of the character values are in a different position as possible.
-- 
-- Display the result as follows:
-- 
--   original string, shuffled string, (score)
-- 
-- The score gives the number of positions whose character value did not change.
-- 
-- Example
--   tree, eetr, (0)
-- 
-- Test cases
--   abracadabra
--   seesaw
--   elk
--   grrrrrr
--   up
--   a
-- 
-- [Source https://rosettacode.org/wiki/Best_shuffle]


commonYearSuite : Test
commonYearSuite =
    describe "Years are common years if"
        [ test "They are divisible by 100" <|
            \_ ->
                stringDiffScore "tree" "eetr"
                    |> Expect.equal 0
        , test "Returns difference list" <|
            \_ ->
                let 
                    in1 = toList "tree" 
                    in2 = toList "eetr"
                in
                differingChars in1 in2
                    |> Expect.equal [False, False, False, False]
        ]

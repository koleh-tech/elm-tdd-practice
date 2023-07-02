module TestBestShuffle exposing (..)

import Expect exposing (Expectation)
import Fuzz exposing (Fuzzer, int, list, string)
import String exposing (toList)
import Test exposing (..)
import BestShuffle exposing (stringDiffScore)


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


stringDiffScoreSuite : Test
stringDiffScoreSuite =
    describe "stringDiffScore, returns 0 if"
        [ test "if all chars have shifted" <|
            \_ ->
                stringDiffScore "tree" "eetr"
                    |> Expect.equal 0
        , test "returns 1 if one char hasn't shifted" <|
            \_ ->
                stringDiffScore "tree" "eter"
                    |> Expect.equal 0

        ]

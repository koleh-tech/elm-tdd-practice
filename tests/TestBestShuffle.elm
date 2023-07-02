module TestBestShuffle exposing (..)

import Expect exposing (Expectation)
import Fuzz exposing (Fuzzer, int, list, string)
import String exposing (toList)
import Test exposing (..)
import List exposing (map2)
import BestShuffle exposing (stringDiffScore, bestOutOfShuffledStrings)


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
                let
                    inputOriginalStrings = ["tree", "elk", "up"]
                    inputShuffledStrings = ["eetr", "kel", "pu"]
                    expectedPassingAssertions  = List.length(inputOriginalStrings)
                in
                    List.map2 stringDiffScore inputOriginalStrings inputShuffledStrings
                        |> List.filter (\x -> x == 0)
                        |> List.length
                        |> Expect.equal expectedPassingAssertions
        , test "returns 1 if one char hasn't shifted" <|
            \_ ->
                stringDiffScore "tree" "eter"
                    |> Expect.equal 1
        , test "strings with many overlapping chars will produce same result" <|
            \_ ->
                let
                    inputOriginalStrings = ["grrrrrr", "grrrrrr", "grrrrrr"]
                    inputShuffledStrings = ["rgrrrrr", "rrgrrrr", "rrrgrrr"]
                    expectedPassingAssertions  = List.length(inputShuffledStrings)
                in
                    List.map2 stringDiffScore inputOriginalStrings inputShuffledStrings
                        |> List.filter (\x -> x == 5)
                        |> List.length
                        |> Expect.equal expectedPassingAssertions
        ]

maxStringDiffSuite : Test
maxStringDiffSuite =
    describe "Given the original string, and a list of shuffled versions of that string"
        [ test "Returns the most shuffled string" <|
            \_ ->
                bestOutOfShuffledStrings "tree" ["eetr", "eter", "tere"]
                    |> Expect.equal "eetr"
        , test "If several shuffled strings are just as shuffled, picks last one" <|
            \_ ->
                bestOutOfShuffledStrings "grrrrrr" ["rgrrrrr", "rrgrrrr", "rrrgrrr"]
                    |> Expect.equal "rrrgrrr"

        ]


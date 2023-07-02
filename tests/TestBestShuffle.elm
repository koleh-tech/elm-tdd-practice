module TestBestShuffle exposing (..)

import Expect exposing (Expectation)
import Fuzz exposing (Fuzzer, int, list, string)
import String exposing (toList)
import Test exposing (..)
import List exposing (map2)
import List.Extra exposing (permutations)
import BestShuffle exposing (numberOfDifferingCharacters, bestOutOfShuffledStrings, shuffleString, bestShuffle)


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
                    List.map2 numberOfDifferingCharacters inputOriginalStrings inputShuffledStrings
                        |> List.filter (\x -> x == 0)
                        |> List.length
                        |> Expect.equal expectedPassingAssertions
        , test "returns 1 if one char hasn't shifted" <|
            \_ ->
                numberOfDifferingCharacters "tree" "eter"
                    |> Expect.equal 1
        , test "strings with many overlapping chars will produce same result" <|
            \_ ->
                let
                    inputOriginalStrings = ["grrrrrr", "grrrrrr", "grrrrrr"]
                    inputShuffledStrings = ["rgrrrrr", "rrgrrrr", "rrrgrrr"]
                    expectedPassingAssertions  = List.length(inputShuffledStrings)
                in
                    List.map2 numberOfDifferingCharacters inputOriginalStrings inputShuffledStrings
                        |> List.filter (\x -> x == 5)
                        |> List.length
                        |> Expect.equal expectedPassingAssertions
        ]

bestOutOfShuffledStringsSuite : Test
bestOutOfShuffledStringsSuite =
    describe "Given the original string, and a list of shuffled versions of that string"
        [ test "Returns the most shuffled string" <|
            \_ ->
                bestOutOfShuffledStrings "tree" ["eetr", "eter", "tere"]
                    |> Expect.equal "eetr"
        , test "If several shuffled strings are just as shuffled, picks last one" <|
            \_ ->
                bestOutOfShuffledStrings "grrrrrr" ["rgrrrrr", "rrgrrrr", "rrrgrrr"]
                    |> Expect.equal "rrrgrrr"
        , test "If all shuffled strings are actually the same, returns the original string" <|
            \_ ->
                bestOutOfShuffledStrings "a" ["a","a","a"]
                    |> Expect.equal "a"

        ]

shuffleStringSuite : Test
shuffleStringSuite =
    describe "Given the original string, shuffleString will return a list"
        [ test "containing a single element for words with one letter" <|
            \_ ->
                shuffleString "a"
                    |> Expect.equal ["a"]
        , test "including the original string, just once" <|
            \_ ->
                shuffleString "elk"
                    |> List.filter (\x -> x == "elk")
                    |> List.length
                    |> Expect.equal 1
        , test "with length equal to possible permutations of chars in the original string" <|
            \_ ->
                shuffleString "seesaw"
                    |> List.length
                    |> Expect.equal 720
        ]

bestShuffleErrHandlingSuite : Test
bestShuffleErrHandlingSuite =
    describe "bestShuffle will return Nothing if the input"
        [ test "contains spaces" <|
            \_ ->
                bestShuffle "a b"
                    |> Expect.equal Nothing
        , test "is more than 8 chars long" <|
            \_ ->
                bestShuffle "123456789"
                    |> Expect.equal Nothing
        ]

bestShuffleValidSuite : Test
bestShuffleValidSuite =
    describe "given valid input, bestShuffle will:"
        [ test "return the original input if it is only one character long" <|
            \_ ->
                bestShuffle "a"
                    |> Expect.equal(Just "a")
        , test "returns shuffle with the most differing chars" <|
            \_ ->
                bestShuffle "tree"
                    |> Expect.equal(Just "eetr")
        -- , test "takes a while for long words" <|
        --     \_ ->
        --         bestShuffle "12345678"
        --             |> (\x -> x == Just "eetr")
        --             |> Expect.equal True
        ]


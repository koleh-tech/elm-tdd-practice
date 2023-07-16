module TestFizzBuzz exposing (..)

import Expect exposing (Expectation)
import Fuzz exposing (Fuzzer, int, list, string)
import Random
import Test exposing (..)
import FizzBuzz exposing (FizzBuzzModel, initialFizzBuzzModel, updateFizzBuzzModel, renderFizzBuzzSequence, determineFizzBuzz)


-- Write a program that prints the numbers from 1 to 100.
-- But for multiples of three print "Fizz" instead of the number and for the multiples of five print "Buzz".
-- For numbers which are multiples of both three and five print "FizzBuzz".
-- 
-- Sample output:
-- 
-- 1
-- 2
-- Fizz
-- 4
-- Buzz
-- Fizz
-- 7
-- 8
-- Fizz
-- Buzz
-- 11
-- Fizz
-- 13
-- 14
-- FizzBuzz
-- 16
-- 17
-- Fizz
-- 19
-- Buzz
-- ... etc up to 100

     
updateFizzBuzzModelSuite : Test
updateFizzBuzzModelSuite =
    describe "When the user hits the button"
        [ test "The sequence in the FizzBuzzModel is updated to have 100 elements" <|
            \_ ->
                let
                    actual = updateFizzBuzzModel initialFizzBuzzModel
                in
                [List.length initialFizzBuzzModel.sequence, List.length actual.sequence]
                    |> Expect.equal [0, 100]
        ]

     
determineFizzBuzzSuite : Test
determineFizzBuzzSuite =
    describe "determineFizzBuzz will"
        [ test "Return the string version of a number, if it is not divisible by 3 and/or 5" <|
            \_ ->
                let
                    input = [1,
                        2,
                        4,
                        7,
                        8,
                        11,
                        13,
                        14,
                        16,
                        17]
                in
                List.map determineFizzBuzz input
                    |> Expect.equal (List.map String.fromInt input)
        , test "Return 'Fizz' if the number is divisible by 3" <|
            \_ ->
                let
                    input = [3, 6, 9, 12, 18, 21, 24, 27, 33, 36, 39, 42, 48, 51, 54, 57, 63, 66, 69, 72, 78, 81, 84, 87, 93, 96, 99]
                in
                List.map determineFizzBuzz input
                    |> List.filter (\x -> x ==  "Fizz")
                    |> List.length
                    |> Expect.equal (List.length input)
        ]

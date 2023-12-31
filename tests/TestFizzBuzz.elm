module TestFizzBuzz exposing (..)

import Expect exposing (Expectation)
import Fuzz exposing (Fuzzer, int, list, string)
import Random
import Test exposing (..)
import FizzBuzz exposing (FizzBuzzModel, initialFizzBuzzModel, updateFizzBuzzModel, renderFizzBuzzSequence, determineFizzBuzz)


updateFizzBuzzModelSuite : Test
updateFizzBuzzModelSuite =
    describe "When the user hits the button"
        [ test "The sequence in the FizzBuzzModel is updated to have 100 elements" <|
            \_ ->
                let
                    actual =
                        updateFizzBuzzModel initialFizzBuzzModel
                in
                    [ List.length initialFizzBuzzModel.sequence, List.length actual.sequence ]
                        |> Expect.equal [ 0, 100 ]
        , test "Contains 33 'Fizz'" <|
            \_ ->
                (updateFizzBuzzModel initialFizzBuzzModel).sequence
                    |> List.filter (\x -> x == "Fizz")
                    |> List.length
                    |> Expect.equal 27
        , test "Contains 14 'Buzz'" <|
            \_ ->
                (updateFizzBuzzModel initialFizzBuzzModel).sequence
                    |> List.filter (\x -> x == "Buzz")
                    |> List.length
                    |> Expect.equal 14
        , test "Contains 6 'FizzBuzz'" <|
            \_ ->
                (updateFizzBuzzModel initialFizzBuzzModel).sequence
                    |> List.filter (\x -> x == "FizzBuzz")
                    |> List.length
                    |> Expect.equal 6
        ]


regularNumberSuite : Test
regularNumberSuite =
    describe "determineFizzBuzz will return the string version of a number"
        [ test "If it is not divisible by 3 and/or 5" <|
            \_ ->
                let
                    input =
                        [ 1
                        , 2
                        , 4
                        , 7
                        , 8
                        , 11
                        , 13
                        , 14
                        , 16
                        , 17
                        ]
                in
                    List.map determineFizzBuzz input
                        |> Expect.equal (List.map String.fromInt input)
        ]


fizzNumberSuite : Test
fizzNumberSuite =
    describe "determineFizzBuzz will return 'Fizz'"
        [ test "If the number is divisible by 3" <|
            \_ ->
                let
                    input =
                        [ 3, 6, 9, 12, 18, 21, 24, 27, 33, 36, 39, 42, 48, 51, 54, 57, 63, 66, 69, 72, 78, 81, 84, 87, 93, 96, 99 ]
                in
                    List.map determineFizzBuzz input
                        |> Expect.equal (List.map (\x -> "Fizz") input)
        , test "If the number is not divisible by 5 and 3" <|
            \_ ->
                let
                    onlyDivisibleBy3 =
                        [ 3, 6, 9, 12, 18, 21, 24, 27, 33, 36, 39, 42, 48, 51, 54, 57, 63, 66, 69, 72, 78, 81, 84, 87, 93, 96, 99 ]

                    alsoDivisibleBy5 =
                        [ 15, 30, 60, 75, 90 ]

                    input =
                        onlyDivisibleBy3 ++ alsoDivisibleBy5
                in
                    List.map determineFizzBuzz input
                        |> List.filter (\x -> x == "Fizz")
                        |> List.length
                        |> Expect.equal (List.length onlyDivisibleBy3)
        ]


buzzNumberSuite : Test
buzzNumberSuite =
    describe "determineFizzBuzz will return 'Buzz'"
        [ test "If the number is divisible by 5" <|
            \_ ->
                let
                    input =
                        [ 5, 10, 25, 55, 65 ]
                in
                    List.map determineFizzBuzz input
                        |> Expect.equal (List.map (\x -> "Buzz") input)
        , test "If the number is not divisible by 5 and 3" <|
            \_ ->
                let
                    onlyDivisibleBy5 =
                        [ 5, 10, 25, 55, 65 ]

                    alsoDivisibleBy3 =
                        [ 15, 30, 60, 90 ]

                    input =
                        onlyDivisibleBy5 ++ alsoDivisibleBy3
                in
                    List.map determineFizzBuzz input
                        |> List.filter (\x -> x == "Buzz")
                        |> List.length
                        |> Expect.equal (List.length onlyDivisibleBy5)
        ]


fizzBuzzNumberSuite : Test
fizzBuzzNumberSuite =
    describe "determineFizzBuzz will return 'FizzBuzz'"
        [ test "If the number is divisible by 3 and 5" <|
            \_ ->
                let
                    input =
                        [ 15, 30, 45, 60, 75, 90 ]
                in
                    List.map determineFizzBuzz input
                        |> Expect.equal (List.map (\x -> "FizzBuzz") input)
        ]

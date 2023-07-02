module TestLeapYear exposing (..)

import Expect exposing (Expectation)
import Fuzz exposing (Fuzzer, int, list, string)
import Test exposing (..)
import LeapYear exposing (isLeapYear)


-- Write a function that returns true or false depending on whether its input integer is a leap year or not.
--
-- A leap year is defined as one that is divisible by 4,
-- but is not otherwise divisible by 100 unless it is also divisible by 400.
--
-- For example, 2001 is a typical common year and 1996 is a typical leap year, whereas 1900 is an atypical common year and 2000 is an atypical leap year.


commonYearSuite : Test
commonYearSuite =
    describe "Years are common years if"
        [ test "They are not divisible by 4" <|
            \_ ->
                let
                    input =
                        [ 5, 2001, 1900, 1996 ]
                in
                    List.map isLeapYear input
                        |> List.filter (\x -> x == False)
                        |> List.length
                        |> Expect.equal 3
        , test "They are divisible by 100" <|
            \_ ->
                isLeapYear 100
                    |> Expect.equal False
        ]


leapYearSuite : Test
leapYearSuite =
    describe "Years are leap years if"
        [ test "They are divisible by 4" <|
            \_ ->
                isLeapYear 4
                    |> Expect.equal True
        , test "They are divisible by 400" <|
            \_ ->
                let
                    input =
                        [ 400, 2000, 1900, 1996 ]
                in
                    List.map isLeapYear input
                        |> List.filter (\x -> x == True)
                        |> List.length
                        |> Expect.equal 3
        ]

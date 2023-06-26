module Example exposing (..)

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

suite : Test
suite =
    describe "Years are not leap years if"
        [
            test "They are not divisible by 4" <|
            \_ ->
                isLeapYear 5
                |> Expect.equal False
        ]


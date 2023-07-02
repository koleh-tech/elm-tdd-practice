module LeapYear exposing (isLeapYear, yearType)

import String


isLeapYear : Int -> Bool
isLeapYear year =
    (remainderBy 4 year
        == 0
        && remainderBy 100 year
        /= 0
    )
        || remainderBy 400 year
        == 0


convertUserInput : String -> Int
convertUserInput input =
    case String.toInt input of
        Nothing ->
            -1

        Just number ->
            number


yearType : String -> String
yearType year =
    let
        convertedYear =
            convertUserInput year
    in
        if convertedYear >= 0 then
            if isLeapYear convertedYear == True then
                "Leap year"
            else
                "Common year"
        else
            "Not a valid year"

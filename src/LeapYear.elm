module LeapYear exposing (isLeapYear)

isLeapYear : Int -> Bool
isLeapYear year =
    case remainderBy 4 year of
        0 ->
            remainderBy 100 year /= 0
        _ ->
            False

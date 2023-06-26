module LeapYear exposing (isLeapYear)

isLeapYear : Int -> Bool
isLeapYear year =
    case remainderBy 4 year of
        0 ->
            case remainderBy 400 year of
                0 ->
                    True
                _ ->
                    remainderBy 100 year /= 0
        _ ->
            False

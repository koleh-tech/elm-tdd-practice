module LeapYear exposing (isLeapYear)

isLeapYear : Int -> Bool
isLeapYear year =
    remainderBy 4 year == 0 && remainderBy 100 year /= 0

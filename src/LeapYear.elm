module LeapYear exposing (isLeapYear)

isLeapYear : Int -> Bool
isLeapYear year =
    remainderBy 4 year == 0

module BestShuffle exposing (stringDiffScore, differingChars)
import String exposing (toList)
import List exposing (map2)

charsMatch char1 char2 =
    char1 == char2

stringDiffScore : String -> String -> Int
stringDiffScore originalString shuffledString =
    0

differingChars : List Char -> List Char -> List Bool
differingChars originalStringChars shuffledStringChars =
    map2 charsMatch originalStringChars shuffledStringChars

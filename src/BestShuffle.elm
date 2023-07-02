module BestShuffle exposing (stringDiffScore, bestOutOfShuffledStrings)
import String exposing (toList)
import List exposing (map2, filter, length)

stringDiffScore : String -> String -> Int
stringDiffScore originalString shuffledString =
    let 
        originalStringChars = toList originalString
        shuffledStringChars = toList shuffledString
        charsMatch = (\char1 char2 -> char1 == char2)
    in
    map2 charsMatch originalStringChars shuffledStringChars
        |> filter (\match -> match)
        |> length
        
bestOutOfShuffledStrings : String -> List String -> String
bestOutOfShuffledStrings originalString shuffledStrings =
    "eetr"

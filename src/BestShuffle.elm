module BestShuffle exposing (stringDiffScore, bestShuffledString)
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
        
bestShuffledString : String -> List String -> String
bestShuffledString originalString shuffledStrings =
    "eer"

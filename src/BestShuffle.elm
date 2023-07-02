module BestShuffle exposing (stringDiffScore, bestOutOfShuffledStrings)
import String exposing (toList)
import List exposing (map2, filter, length)
import List.Extra exposing (minimumWith)

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
    let
        diffFromOriginalString = stringDiffScore originalString
        bestString = minimumWith (\str1 str2 -> if diffFromOriginalString(str1) > diffFromOriginalString(str2) then GT else LT) shuffledStrings
    in
        case bestString of
            Nothing ->
                originalString
            Just result ->
                result

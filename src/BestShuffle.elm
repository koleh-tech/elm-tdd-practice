module BestShuffle exposing (numberOfDifferingCharacters, bestOutOfShuffledStrings)
import String exposing (toList)
import List exposing (map2, filter, length)
import List.Extra exposing (minimumWith)

numberOfDifferingCharacters : String -> String -> Int
numberOfDifferingCharacters originalString shuffledString =
    let 
        originalStringChars = toList originalString
        shuffledStringChars = toList shuffledString
        charactersMatch = (\char1 char2 -> char1 == char2)
    in
    map2 charactersMatch originalStringChars shuffledStringChars
        |> filter (\match -> match)
        |> length
        
bestOutOfShuffledStrings : String -> List String -> String
bestOutOfShuffledStrings originalString shuffledStrings =
    let
        differingCharactersFromOriginalString = numberOfDifferingCharacters originalString
        compareStrings = (\str1 str2 ->
            if differingCharactersFromOriginalString(str1) > differingCharactersFromOriginalString(str2)
            then GT else LT
            )
    in
        case minimumWith compareStrings shuffledStrings of
            Nothing ->
                originalString
            Just result ->
                result

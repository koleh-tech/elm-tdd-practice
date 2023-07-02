module BestShuffle exposing (numberOfDifferingCharacters, bestOutOfShuffledStrings, shuffleString)
import String
import List exposing (map, map2, filter, length)
import List.Extra exposing (minimumWith)
import Permutations 

numberOfDifferingCharacters : String -> String -> Int
numberOfDifferingCharacters originalString shuffledString =
    let 
        originalStringChars = String.toList originalString
        shuffledStringChars = String.toList shuffledString
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

shuffleString : String -> List String
shuffleString originalString =
    let 
        input = String.toList originalString
    in
    Permutations.ofList input
        |> map (\x -> String.fromList x)

module BestShuffle exposing (numberOfDifferingCharacters, bestOutOfShuffledStrings, shuffleString, bestShuffle)
import String
import List exposing (map, map2, filter, length)
import List.Extra exposing (minimumWith)
import Permutations 

numberOfDifferingCharacters : String -> String -> Int
numberOfDifferingCharacters originalString shuffledString =
    let 
        shuffledStringChars = String.toList shuffledString
    in
    String.toList originalString
        |> map2 (\char1 char2 -> char1 == char2) shuffledStringChars
        |> filter (\match -> match)
        |> length
        
bestOutOfShuffledStrings : String -> List String -> String
bestOutOfShuffledStrings originalString shuffledStrings =
    let
        differenceFromOriginal = numberOfDifferingCharacters originalString
        compareStrings = (\s1 s2 ->
            if differenceFromOriginal(s1) > differenceFromOriginal(s2)
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
    String.toList originalString
        |> Permutations.ofList
        |> map (\x -> String.fromList x)

bestShuffle : String -> Maybe String
bestShuffle input =
    Nothing

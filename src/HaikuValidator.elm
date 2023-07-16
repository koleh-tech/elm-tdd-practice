module HaikuValidator exposing (isValidHaiku, haikuSyllables)

import List
import Char
import List.Extra exposing (groupWhile)
import Array
import String


type alias HaikuReviewModel =
    { haiku : String
    , isValid : Bool
    , syllables : String
    }


isValidHaiku : String -> Bool
isValidHaiku haiku =
    haikuSyllables haiku == [ 5, 7, 5 ]


isSyllable : Char -> Bool
isSyllable toCheck =
    Char.toLower toCheck
        |> (\c -> List.member c [ 'a', 'e', 'i', 'o', 'u', 'y' ])


numberOfSyllablesInList : List Char -> Int
numberOfSyllablesInList word =
    List.map isSyllable word
        |> groupWhile (\a b -> a == True && b == True)
        |> List.filter (\a -> a /= ( False, [] ))
        |> List.length


haikuSyllables : String -> List Int
haikuSyllables haiku =
    String.split "/" haiku
        |> List.map String.toList
        |> List.map numberOfSyllablesInList

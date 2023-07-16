module HaikuValidator exposing (isValidHaiku, haikuSyllables, updateHaikuReviewModel, HaikuReviewModel, initialHaikuReviewModel)

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

initialHaikuReviewModel : HaikuReviewModel
initialHaikuReviewModel =
    { haiku = ""
    , isValid = True
    , syllables = ""
    }

updateHaikuReviewModel : HaikuReviewModel -> String -> HaikuReviewModel
updateHaikuReviewModel model userInput = 
    { model
        | isValid = isValidHaiku userInput
        , syllables = haikuSyllables userInput |> List.map String.fromInt |> List.map (\x -> x ++ ",") |> String.concat |> String.slice 0 -1
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

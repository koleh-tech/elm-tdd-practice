module HaikuValidator exposing (isValidHaiku, haikuSyllables)
import List
import Array
import String


isValidHaiku : String -> Bool
isValidHaiku haiku =
    False

isSyllable : Char -> Bool
isSyllable toCheck =
    List.member toCheck ['a', 'e', 'i', 'o', 'u']

numberOfSyllablesInList : List Char -> Int
numberOfSyllablesInList word =
    List.map isSyllable word
    |> List.filter (\x -> x == True)
    |> List.length

haikuSyllables : String -> List Int
haikuSyllables haiku =
    String.split "/" haiku
    |> List.map String.toList
    |> List.map numberOfSyllablesInList

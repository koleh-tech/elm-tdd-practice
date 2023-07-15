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

haikuSyllables : String -> List Int
haikuSyllables haiku =
    let
        stringsToCheck = String.split "/" haiku
        classifyNoSyllables = (\word -> List.map isSyllable word |> List.filter (\x -> x == True) |> List.length)
    in
    List.map String.toList stringsToCheck
    |> List.map classifyNoSyllables

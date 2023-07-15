module HaikuValidator exposing (isValidHaiku, haikuSyllables)
import List
import List.Extra exposing (groupWhile)
import Array
import String


isValidHaiku : String -> Bool
isValidHaiku haiku =
    False

isSyllable : Char -> Bool
isSyllable toCheck =
    List.member toCheck ['a', 'e', 'i', 'o', 'u', 'y']

numberOfSyllablesInList : List Char -> Int
numberOfSyllablesInList word =
    List.map isSyllable word
    |> groupWhile (\a b -> a == True && b == True)
    |> List.filter (\a -> a /= (False,[]))
    |> List.length

haikuSyllables : String -> List Int
haikuSyllables haiku =
    String.split "/" haiku
    |> List.map String.toList
    |> List.map numberOfSyllablesInList

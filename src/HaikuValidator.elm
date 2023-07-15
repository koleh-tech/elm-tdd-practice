module HaikuValidator exposing (isValidHaiku, haikuSyllables)

import String


isValidHaiku : String -> Bool
isValidHaiku haiku =
    False

haikuSyllables : String -> List Int
haikuSyllables haiku =
    [1,0,1]

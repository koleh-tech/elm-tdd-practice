module TestHaikuValidator exposing (..)

import Expect exposing (Expectation)
import Fuzz exposing (Fuzzer, int, list, string)
import Test exposing (..)
import HaikuValidator exposing (isValidHaiku, haikuSyllables, updateHaikuReviewModel, HaikuReviewModel, initialHaikuReviewModel)


-- Haiku is an ancient form of Japanese poetry. A haiku is a three-line poem with seventeen syllables,
-- where the first line must contain five syllables, the second line must contain seven syllables,
-- and the third line must contain five syllables. The lines do not have to rhyme. Here is an example, where slashes separate the lines:
--
-- Computer programs/The bugs try to eat my code/I must not let them.
--
--
-- You must write a program that will review a haiku and check that each line contains the correct number of syllables.
--
-- Input
--
-- The input contains one or more lines, each of which contains a single haiku. A haiku will contain at least three words,
-- and words will be separated by either a single space or a slash ('/'). Slashes also separate the three lines of a haiku,
-- so each haiku will contain exactly two slashes. (The three lines of the haiku will be contained within one physical line of the file.)
--
-- A haiku will contain only lowercase letters ('a'-'z'), forward slashes ('/'), and spaces,
-- and will be no more than 200 characters long (not counting the end-of-line characters).
--
-- Each haiku is guaranteed to contain three lines, and each line will contain at least one word.
-- Your job is to determine whether each line has the correct number of syllables (5/7/5). For the purposes of this problem,
-- every contiguous sequence of one or more vowels counts as one syllable, where the vowels are a, e,
-- i, o, u, and y. Every word will contain at least one syllable.
--
-- (Note that this method of counting syllables does not always agree with English conventions.
-- In the second example below, your program must consider the word 'code' to have two syllables because the 'o' and the 'e' are not consecutive.
-- However, in English the 'e' is silent and so 'code' actually has only one syllable.)
--
-- Output
--
-- For each haiku, output a comma-separated single line that contains the number of syllables in each haiku,
-- together with the letter Y if it is a haiku, or N if it is not a haiku (see below).
--
--
-- Sample Input
--
-- happy purple frog/eating bugs in the marshes/get indigestion
-- computer programs/the bugs try to eat my code/i will not let them
-- An old silent pond/A frog jumps into the pond—/Splash! Silence again.
--
-- Sample Output
--
-- 5,7,5,Yes
-- 5,8,5,No
--
--
-- [Source: http://uva.onlinejudge.org/]


updateHaikuReviewModelSuite : Test
updateHaikuReviewModelSuite =
    describe "HaikuReviewModels are updated to have new syllables and valid state"
        [ test "New syllables as user types, and invalid state if not correct" <|
            \_ ->
                let
                    userInput =
                        "testing//"
                in
                    updateHaikuReviewModel initialHaikuReviewModel userInput
                        |> Expect.equal { initialHaikuReviewModel | isValid = False, syllables = "2,0,0" }
        , test "Valid state is updated if correct syllables" <|
            \_ ->
                let
                    invalidUserInput =
                        "happy purple frog/eating bugs in the marshes/get"

                    validUserInput =
                        "happy purple frog/eating bugs in the marshes/get indigestion"
                in
                    updateHaikuReviewModel initialHaikuReviewModel invalidUserInput
                        |> (\model ->
                                [ updateHaikuReviewModel initialHaikuReviewModel invalidUserInput
                                , updateHaikuReviewModel model validUserInput
                                ]
                           )
                        |> Expect.equal
                            [ { initialHaikuReviewModel | isValid = False, syllables = "5,7,1" }
                            , { initialHaikuReviewModel | isValid = True, syllables = "5,7,5" }
                            ]
        ]


invalidHaikuSuite : Test
invalidHaikuSuite =
    describe "Poems are not valid Haikus if"
        [ test "They have the incorrect spacing of synonyms" <|
            \_ ->
                let
                    input =
                        [ "computer programs/the bugs try to eat my code/i will not let them"
                        , "./some incorrect number/abc def ghij"
                        , "Forgotten flower/silently wilts -/May you have sweet dreams"
                        ]
                in
                    List.map isValidHaiku input
                        |> List.filter (\x -> x == False)
                        |> List.length
                        |> Expect.equal 3
        , test "They are blank strings" <|
            \_ ->
                isValidHaiku "//"
                    |> Expect.equal False
        ]


validHaikuSuite : Test
validHaikuSuite =
    describe "Poems are valid Haikus if"
        [ test "They have the correct spacing of synonyms" <|
            \_ ->
                let
                    input =
                        [ "happy purple frog/eating bugs in the marshes/get indigestion"
                        ]
                in
                    List.map isValidHaiku input
                        |> List.filter (\x -> x == True)
                        |> List.length
                        |> Expect.equal 1
        ]


countingSyllablesTestSuite : Test
countingSyllablesTestSuite =
    describe "A syllable will counted if"
        [ test "There is a vowel" <|
            \_ ->
                [ haikuSyllables "a/qwtjk/i"
                , haikuSyllables "qwtjk/a/i"
                , haikuSyllables "i/A/qwtjk"
                ]
                    |> Expect.equal
                        [ [ 1, 0, 1 ]
                        , [ 0, 1, 1 ]
                        , [ 1, 1, 0 ]
                        ]
        , test "y is counted as a vowel" <|
            \_ ->
                haikuSyllables "y/a/b"
                    |> Expect.equal [ 1, 1, 0 ]
        , test "There are a contiguous set of vowels" <|
            \_ ->
                [ haikuSyllables "at/coaders/toad"
                , haikuSyllables "ta/rohdy/code"
                , haikuSyllables "computer programs/The bugs try to eat my code/I must not let them"
                ]
                    |> Expect.equal
                        [ [ 1, 2, 1 ]
                        , [ 1, 2, 2 ]
                        , [ 5, 8, 5 ]
                        ]
        ]

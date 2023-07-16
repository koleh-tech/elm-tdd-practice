module Main exposing (..)

import Browser
import Html exposing (Html, div, text, input, section, h1, h2, h3, label,  pre)
import Html.Events exposing (onInput)
import Html.Attributes exposing (class, name, type_, for, value)
import String
import Debug exposing (log)
import FormatNumber exposing (format)
import FormatNumber.Locales exposing (Decimals(..), Locale, usLocale)
import LeapYear exposing (yearType)
import BestShuffle exposing (bestShuffle, numberOfDifferingCharacters)
import HaikuValidator exposing (updateHaikuReviewModel, HaikuReviewModel, initialHaikuReviewModel, getHaikuValidOrNot, getHaikuSyllables)


type alias BestShuffleModel =
    { originalWord : String
    , isValid : Bool
    , bestShuffle : String
    , numDifferingChars : Int
    }


initialBestShuffleModel : BestShuffleModel
initialBestShuffleModel =
    { originalWord = ""
    , isValid = True
    , bestShuffle = ""
    , numDifferingChars = 0
    }


type alias LeapYearModel =
    { year : Int
    , yearType : String
    }


initialLeapYearModel : LeapYearModel
initialLeapYearModel =
    { year = 0
    , yearType = ""
    }


type alias Model =
    { leapYearModel : LeapYearModel
    , bestShuffleModel : BestShuffleModel
    , haikuReviewModel : HaikuReviewModel
    }


initialModel : Model
initialModel =
    { leapYearModel = initialLeapYearModel
    , bestShuffleModel = initialBestShuffleModel
    , haikuReviewModel = initialHaikuReviewModel
    }


type Msg
    = YearClassification String
    | BestShuffle String
    | HaikuReview String


view : Model -> Html Msg
view model =
    section [ class "section" ]
        [ div [ class "container" ]
            [ h1 [ class "title" ] [ text "Leap year classifier" ]
            , h3 [ class "subtitle" ] [ text "Initial specification:" ]
            , pre [] [text """Write a function that returns true or false depending on whether its input integer is a leap year or not.
A leap year is defined as one that is divisible by 4,
but is not otherwise divisible by 100 unless it is also divisible by 400.
For example, 2001 is a typical common year and 1996 is a typical leap year,
whereas 1900 is an atypical common year and 2000 is an atypical leap year."""]
            , label [ for "Year", class "label" ] [ text "Year:" ]
            , input
                ([ name "Year"
                 , class "input"
                 , type_ "text"
                 , onInput YearClassification
                 ]
                )
                []
            , text model.leapYearModel.yearType
        ]
        , div [ class "container" ]
            [ h1 [ class "title" ] [ text "Best shuffle" ]
            , h3 [ class "subtitle" ] [ text "Initial specification:" ]
            , pre [] [text """Shuffle the characters of a string in such a way that as many of the character values are in a different position as possible.

Display the result as follows:

original string, shuffled string, (score)

The score gives the number of positions whose character value did not change.

Example
  tree, eetr, (0)

Test cases
  abracadabra
  seesaw
  elk
  grrrrrr
  up
  a

[Source https://rosettacode.org/wiki/Best_shuffle]"""]
            , label [ for "ToShuffle", class "label" ] [ text "Word to shuffle:" ]
            , input
                ([ name "ToShuffle"
                 , class "input"
                 , type_ "text"
                 , onInput BestShuffle
                 ]
                    ++ getBestShuffleValidOrNot model
                )
                []
            , bestShuffleText model
                |> text
            ]
        , div [ class "container" ]
            [ h1 [ class "title" ] [ text "Haiku Review" ]
            , h3 [ class "subtitle" ] [ text "Initial specification:" ]
            , pre [] [text """Shuffle the characters of a string in such a way that as many of the character values are in a different position as possible.
Haiku is an ancient form of Japanese poetry. A haiku is a three-line poem with seventeen syllables,
where the first line must contain five syllables, the second line must contain seven syllables,
and the third line must contain five syllables. The lines do not have to rhyme. Here is an example, where slashes separate the lines:

Computer programs/The bugs try to eat my code/I must not let them.


You must write a program that will review a haiku and check that each line contains the correct number of syllables.

Input

The input contains one or more lines, each of which contains a single haiku. A haiku will contain at least three words,
and words will be separated by either a single space or a slash ('/'). Slashes also separate the three lines of a haiku,
so each haiku will contain exactly two slashes. (The three lines of the haiku will be contained within one physical line of the file.)

A haiku will contain only lowercase letters ('a'-'z'), forward slashes ('/'), and spaces,
and will be no more than 200 characters long (not counting the end-of-line characters).

Each haiku is guaranteed to contain three lines, and each line will contain at least one word.
Your job is to determine whether each line has the correct number of syllables (5/7/5). For the purposes of this problem,
every contiguous sequence of one or more vowels counts as one syllable, where the vowels are a, e,
i, o, u, and y. Every word will contain at least one syllable.

(Note that this method of counting syllables does not always agree with English conventions.
In the second example below, your program must consider the word 'code' to have two syllables because the 'o' and the 'e' are not consecutive.
However, in English the 'e' is silent and so 'code' actually has only one syllable.)

Output

For each haiku, output a comma-separated single line that contains the number of syllables in each haiku,
together with the letter Y if it is a haiku, or N if it is not a haiku (see below).


Sample Input

happy purple frog/eating bugs in the marshes/get indigestion
computer programs/the bugs try to eat my code/i will not let them
An old silent pond/A frog jumps into the pond—/Splash! Silence again.

Sample Output

5,7,5,Yes
5,8,5,No


[Source: http://uva.onlinejudge.org/]"""]
            , label [ for "ToShuffle", class "label" ] [ text "Haiku to check:" ]
            , input
                ([ name "HaikuToCheck"
                 , class "input"
                 , type_ "text"
                 , onInput HaikuReview
                 ]
                    ++ getHaikuValidOrNot model.haikuReviewModel
                )
                []
            , getHaikuSyllables model.haikuReviewModel
                |> text
            ]
        ]


bestShuffleText : Model -> String
bestShuffleText model =
    if model.bestShuffleModel.bestShuffle == "" || model.bestShuffleModel.isValid == False then
        ""
    else
        model.bestShuffleModel.bestShuffle ++ " (" ++ String.fromInt model.bestShuffleModel.numDifferingChars ++ ")"


update : Msg -> Model -> Model
update msg model =
    case msg of
        YearClassification userInput ->
            let
                updateYearType yearModel =
                    { yearModel | yearType = yearType userInput }
            in
                { model | leapYearModel = updateYearType (model.leapYearModel) }

        BestShuffle userInput ->
            case bestShuffle userInput of
                Nothing ->
                    let
                        bestShuffleInvalid bestShuffleModel =
                            { bestShuffleModel
                                | isValid = False
                            }
                    in
                        { model
                            | bestShuffleModel = bestShuffleInvalid (model.bestShuffleModel)
                        }

                Just result ->
                    let
                        withBestShuffle bestShuffleModel =
                            { bestShuffleModel
                                | bestShuffle = result
                                , isValid = True
                                , numDifferingChars = numberOfDifferingCharacters userInput result
                            }
                    in
                        { model | bestShuffleModel = withBestShuffle (model.bestShuffleModel) }
        HaikuReview userInput ->
            { model |
                haikuReviewModel = updateHaikuReviewModel model.haikuReviewModel userInput
            }


getBestShuffleValidOrNot : Model -> List (Html.Attribute msg)
getBestShuffleValidOrNot model =
    if model.bestShuffleModel.isValid == True then
        [ class "input" ]
    else
        [ class "input is-danger" ]


main : Program () Model Msg
main =
    Browser.sandbox
        { init = initialModel
        , view = view
        , update = update
        }

module Main exposing (..)

import Browser
import Html exposing (Html, div, text, input, section, h1, h2, label)
import Html.Events exposing (onInput)
import Html.Attributes exposing (class, name, type_, for, value)
import String
import Debug exposing (log)
import FormatNumber exposing (format)
import FormatNumber.Locales exposing (Decimals(..), Locale, usLocale)
import LeapYear exposing (yearType)
import BestShuffle exposing (bestShuffle, numberOfDifferingCharacters)


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
    }


initialModel : Model
initialModel =
    { leapYearModel = initialLeapYearModel
    , bestShuffleModel = initialBestShuffleModel
    }


type Msg
    = YearClassification String
    | BestShuffle String


view : Model -> Html Msg
view model =
    section [ class "section" ]
        [ div [ class "container" ]
            [ h1 [ class "title" ] [ text "Leap year classifier" ]
            , h2 [ class "subtitle" ] [ text "Determine if year is leap year" ]
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
            , h2 [ class "subtitle" ] [ text "Gives the best shuffle of a word." ]
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
        ]

bestShuffleText : Model -> String
bestShuffleText model =  
    if model.bestShuffleModel.bestShuffle == "" || model.bestShuffleModel.isValid == False then
        ""
    else
        model.bestShuffleModel.bestShuffle ++ " (" ++ String.fromInt model.bestShuffleModel.numDifferingChars ++ "]"

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
                            { bestShuffleModel |
                                isValid = False
                            }
                    in
                        { model |
                            bestShuffleModel = bestShuffleInvalid (model.bestShuffleModel)
                        }

                Just result ->
                    let
                        withBestShuffle bestShuffleModel =
                            { bestShuffleModel |
                                bestShuffle = result, isValid = True
                                , numDifferingChars = numberOfDifferingCharacters userInput result
                            }
                    in
                        { model | bestShuffleModel = withBestShuffle (model.bestShuffleModel) }


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

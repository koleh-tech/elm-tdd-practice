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
    { fahrenheitFieldValid : Bool
    , celsiusFieldValid : Bool
    , fahrenheit : Float
    , celsius : Float
    , fahrenheitFieldValue : String
    , celsiusFieldValue : String
    , leapYearModel : LeapYearModel
    }


initialModel : Model
initialModel =
    { fahrenheitFieldValid = True
    , celsiusFieldValid = True
    , fahrenheit = 32
    , celsius = 0
    , fahrenheitFieldValue = "32"
    , celsiusFieldValue = "0"
    , leapYearModel = initialLeapYearModel
    }


type Msg
    = FahreinheitToCelsius String
    | CelsiusToFahrenheit String
    | YearClassification String


view : Model -> Html Msg
view model =
    section [ class "section" ]
        [ div [ class "container" ]
            [ h1 [ class "title" ] [ text "Temperature Converter" ]
            , h2 [ class "subtitle" ] [ text "Converting from Fahrenheit to Celsius" ]
            , label [ for "Fahrenheit", class "label" ] [ text "Fahrenheit:" ]
            , input
                ([ name "Fahrenheit"
                 , class "input"
                 , type_ "text"
                 , onInput FahreinheitToCelsius
                 , value (viewFahrenheit model)
                 ]
                    ++ getFahrenheitFieldValidOrNot model
                )
                []
            , label [ for "Celsius", class "label" ] [ text "Celsius:" ]
            , input
                ([ name "Celsius"
                 , class "input"
                 , type_ "text"
                 , onInput CelsiusToFahrenheit
                 , value (viewCelsius model)
                 ]
                    ++ getCelsiusFieldValidOrNot model
                )
                []
            ]
        , div [ class "container" ]
            [ h1 [ class "title" ] [ text "Leap year classifier" ]
            , h2 [ class "subtitle" ] [ text "Determine if year is leap year" ]
            , label [ for "Year", class "label" ] [ text "Year:" ]
            , input
                ([ name "Year"
                 , class "input"
                 , type_ "text"
                 , onInput YearClassification
                 ]
                    ++ getFahrenheitFieldValidOrNot model
                )
                []
            , text model.leapYearModel.yearType
            ]
        ]


getFahrenheitFieldValidOrNot : Model -> List (Html.Attribute msg)
getFahrenheitFieldValidOrNot model =
    if model.fahrenheitFieldValid == True then
        [ class "input" ]
    else
        [ class "input is-danger" ]


getCelsiusFieldValidOrNot : Model -> List (Html.Attribute msg)
getCelsiusFieldValidOrNot model =
    if model.celsiusFieldValid == True then
        [ class "input" ]
    else
        [ class "input is-danger" ]


viewCelsius : Model -> String
viewCelsius model =
    if model.celsiusFieldValid == True then
        model.celsius |> formatTemperature
    else
        model.celsiusFieldValue


viewFahrenheit : Model -> String
viewFahrenheit model =
    if model.fahrenheitFieldValid == True then
        model.fahrenheit |> formatTemperature
    else
        model.fahrenheitFieldValue


formatTemperature number =
    format { usLocale | decimals = Max 2 } number


update : Msg -> Model -> Model
update msg model =
    case msg of
        FahreinheitToCelsius userInput ->
            case String.toFloat userInput of
                Nothing ->
                    { model | fahrenheitFieldValid = False, fahrenheitFieldValue = userInput }

                Just number ->
                    { model
                        | fahrenheit = number
                        , celsius = fahrenheitToCelsius number
                        , fahrenheitFieldValue = userInput
                        , fahrenheitFieldValid = True
                        , celsiusFieldValid = True
                    }

        CelsiusToFahrenheit userInput ->
            case String.toFloat userInput of
                Nothing ->
                    { model | celsiusFieldValid = False, celsiusFieldValue = userInput }

                Just number ->
                    { model
                        | celsius = number
                        , fahrenheit = celsiusToFahrenheit number
                        , celsiusFieldValue = userInput
                        , celsiusFieldValid = True
                        , fahrenheitFieldValid = True
                    }

        YearClassification userInput ->
            let updateYearType yearModel = { yearModel | yearType = yearType userInput } in
            { model | leapYearModel = updateYearType(model.leapYearModel) }


fahrenheitToCelsius fahrenheit =
    (fahrenheit - 32) / 1.8


celsiusToFahrenheit celsius =
    (celsius - 1.8) / 32


main : Program () Model Msg
main =
    Browser.sandbox
        { init = initialModel
        , view = view
        , update = update
        }

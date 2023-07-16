module FizzBuzz exposing (FizzBuzzModel, initialFizzBuzzModel, updateFizzBuzzModel, renderFizzBuzzSequence, determineFizzBuzz)

import List
import String
import Html exposing (Html, div, text, input, section, h1, h2, label, ul, li)
import Html.Attributes exposing (class, name, type_, for, value)


type alias FizzBuzzModel =
    { sequence : List String }


initialFizzBuzzModel : FizzBuzzModel
initialFizzBuzzModel =
    { sequence = [] }


determineFizzBuzz : Int -> String
determineFizzBuzz number =
    (if modBy 3 number == 0 then
        "Fizz"
     else
        ""
    )
        |> (\currentString ->
                currentString
                    ++ if modBy 5 number == 0 then
                        "Buzz"
                       else
                        ""
           )
        |> (\currentString ->
                currentString
                    ++ if currentString == "" then
                        String.fromInt number
                       else
                        ""
           )


renderFizzBuzzSequence : FizzBuzzModel -> Html msg
renderFizzBuzzSequence model =
    model.sequence
        |> List.map (\l -> li [] [ text l ])
        |> ul []


updateFizzBuzzModel : FizzBuzzModel -> FizzBuzzModel
updateFizzBuzzModel model =
    { model
        | sequence =
            List.range 1 100
                |> List.map determineFizzBuzz
    }

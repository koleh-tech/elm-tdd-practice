module FizzBuzz exposing (FizzBuzzModel, initialFizzBuzzModel, updateFizzBuzzModel, renderFizzBuzzSequence)

import List
import Char
import List.Extra exposing (groupWhile)
import Array
import String
import Html exposing (Html, div, text, input, section, h1, h2, label, ul, li)
import Html.Attributes exposing (class, name, type_, for, value)


type alias FizzBuzzModel =
    { sequence : List String }

initialFizzBuzzModel : FizzBuzzModel
initialFizzBuzzModel =
    { sequence = [] }

renderFizzBuzzSequence : FizzBuzzModel -> Html msg
renderFizzBuzzSequence model =
    renderList model.sequence



updateFizzBuzzModel : FizzBuzzModel -> FizzBuzzModel
updateFizzBuzzModel model = 
    { model |
        sequence = List.range 1 100
            |> List.map String.fromInt
    }

renderList : List String -> Html msg
renderList lst =
    lst
       |> List.map (\l -> li [] [ text l ])
       |> ul []

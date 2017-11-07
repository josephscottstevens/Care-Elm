module Utils.Dropdowns exposing (..)

import Html exposing (Html, select, option, text)
import Html.Attributes exposing (id, value)


ejDropDownListSource : String -> List ( String, String ) -> Html msg
ejDropDownListSource targetId items =
    select [ id targetId ]
        [ option [ value "1" ] [ text "art" ]
        , option [ value "2" ] [ text "science" ]
        ]

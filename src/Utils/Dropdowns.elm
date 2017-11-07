module Utils.Dropdowns exposing (..)

import Html exposing (Html, select, option, text)
import Html.Attributes exposing (id, value)


ejDropDownListSource : String -> List ( String, String ) -> Html msg
ejDropDownListSource targetId items =
    select [ id targetId ] (mapper items)


mapperSelect : ( String, String ) -> Html msg
mapperSelect ( val, txt ) =
    option [ value val ] [ text txt ]


mapper : List ( String, String ) -> List (Html msg)
mapper t =
    List.map (\y -> mapperSelect y) t


facilityDropDownSource : List ( String, String )
facilityDropDownSource =
    [ ( "1", "art" )
    , ( "2", "science" )
    ]

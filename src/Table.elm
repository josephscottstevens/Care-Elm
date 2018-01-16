module Table exposing (..)

import Html exposing (Html, Attribute, div, table, th, td, tr, thead, tbody, text)
import Html.Attributes exposing (class, id, style)
import Html.Events as Events


-- Data Types


type alias Row msg =
    { columns : List (Column msg)
    }


type alias Column msg =
    { node : Html msg
    , isReversed : Bool
    , data : String
    , isSpecial : Bool
    }


type alias Config =
    { domTableId : String
    , headers : List String
    }


stringColumn : String -> String -> Column msg
stringColumn name data =
    { node = text name
    , isReversed = False
    , data = data
    , isSpecial = False
    }



-- VIEW


view : List (Row msg) -> Config -> Html msg
view rows config =
    table [ id config.domTableId, style [ ( "width", "100%" ) ] ]
        [ thead [ class "e-gridheader e-columnheader e-hidelines" ]
            (List.map viewTh config.headers)
        , tbody []
            (List.map (\t -> tr [] (List.map viewTd t.columns)) rows)
        ]


viewTh : String -> Html msg
viewTh name =
    th [ class ("e-columnheader e-default e-filterbarcell " ++ name) ]
        [ div [ class "e-headercelldiv e-gridtooltip headerColumn" ] [ text name ]
        ]


viewTd : Column msg -> Html msg
viewTd column =
    td [] [ text column.data ]

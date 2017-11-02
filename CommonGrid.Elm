module CommonGrid exposing (..)

import Html exposing (Html, text, div, program, button, input, span)
import Html.Attributes exposing (style, class, type_, id)
import Table


defaultString : Maybe String -> String
defaultString str =
    case str of
        Just t ->
            t

        Nothing ->
            ""


simpleTheadHelp : ( String, Table.Status, Html.Attribute msg ) -> Html msg
simpleTheadHelp ( name, status, onClick ) =
    let
        content =
            case status of
                Table.Unsortable ->
                    [ Html.text name ]

                Table.Sortable selected ->
                    [ Html.text name ]

                Table.Reversible Nothing ->
                    [ Html.text name ]

                Table.Reversible (Just isReversed) ->
                    [ Html.text name
                    , if isReversed then
                        div [ class "glyphicon glyphicon-menu-up" ] []
                      else
                        div [ class "glyphicon glyphicon-menu-down" ] []
                    ]
    in
        Html.th [ onClick, class "e-columnheader" ] content


simpleThead : List ( String, Table.Status, Html.Attribute msg ) -> Table.HtmlDetails msg
simpleThead headers =
    Table.HtmlDetails [] (List.map simpleTheadHelp headers)


checkColumn : String -> Table.Column data msg
checkColumn name =
    Table.veryCustomColumn
        { name = name
        , viewData = checkColumnCell
        , sorter = Table.unsortable
        }


checkColumnCell : data -> Table.HtmlDetails msg
checkColumnCell emp =
    Table.HtmlDetails []
        [ input [ type_ "checkbox" ] []
        ]

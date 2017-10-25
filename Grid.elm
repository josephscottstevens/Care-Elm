module Grid exposing (..)

import Model exposing (..)
import HtmlHelper exposing (..)
import Html exposing (Html, text, div, input, program, button, select, option)
import Html.Attributes exposing (style, class, placeholder, id, type_, value)
import Html.Events exposing (onClick, onInput)
import Table


customGrid : { a | employers : List Employer, tableState : Table.State } -> Html Msg
customGrid model =
    Table.view config model.tableState (List.take 20 model.employers)


config : Table.Config Employer Msg
config =
    Table.customConfig
        { toId = .city
        , toMsg = SetTableState
        , columns =
            [ editColumn
            , Table.stringColumn "Date of birth" .dob
            , Table.stringColumn "Email" .email
            , Table.stringColumn "Address Line 1" .addressLine1
            , Table.stringColumn "Address Line 2" .addressLine2
            , Table.stringColumn "City" .city
            , Table.stringColumn "State" .state
            , Table.stringColumn "Zip Code" .zipCode
            , Table.stringColumn "Phone" .phone
            ]
        , customizations = defaultCustomizations
        }


defaultCustomizations : Table.Customizations data msg
defaultCustomizations =
    { tableAttrs = []
    , caption = Nothing
    , thead = simpleThead
    , tfoot = Nothing
    , tbodyAttrs = []
    , rowAttrs = simpleRowAttrs
    }


simpleTheadHelp : ( String, Table.Status, Html.Attribute msg ) -> Html msg
simpleTheadHelp ( name, status, onClick ) =
    let
        content =
            case status of
                Table.Unsortable ->
                    [ Html.text name ]

                Table.Sortable selected ->
                    [ Html.text name
                    , if selected then
                        darkGrey "↓"
                      else
                        lightGrey "↓"
                    ]

                Table.Reversible Nothing ->
                    [ Html.text name
                    , lightGrey "↕"
                    ]

                Table.Reversible (Just isReversed) ->
                    [ Html.text name
                    , darkGrey
                        (if isReversed then
                            "↑"
                         else
                            "↓"
                        )
                    ]
    in
        Html.th [ onClick ] content


darkGrey : String -> Html msg
darkGrey symbol =
    Html.span [ style [ ( "color", "#555" ) ] ] [ Html.text (" " ++ symbol) ]


lightGrey : String -> Html msg
lightGrey symbol =
    Html.span [ style [ ( "color", "#ccc" ) ] ] [ Html.text (" " ++ symbol) ]


simpleThead : List ( String, Table.Status, Html.Attribute msg ) -> Table.HtmlDetails msg
simpleThead headers =
    Table.HtmlDetails [] (List.map simpleTheadHelp headers)


simpleRowAttrs : data -> List (Html.Attribute msg)
simpleRowAttrs _ =
    []


editColumn : Table.Column Employer Msg
editColumn =
    Table.veryCustomColumn
        { name = ""
        , viewData = editButton
        , sorter = Table.unsortable
        }


editButton : Employer -> Table.HtmlDetails Msg
editButton emp =
    Table.HtmlDetails []
        [ button [ class "btn btn-default", controlStyle, onClick (EditStart emp) ] [ text "Edit" ]
        ]

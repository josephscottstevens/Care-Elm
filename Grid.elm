module Grid exposing (customGrid, config)

import Model exposing (..)
import Html exposing (Html, text, div, program, button, input, span)
import Html.Attributes exposing (style, class, type_, id)
import Html.Events exposing (onClick)
import Table


customGrid : { a | employers : List BillingCcm, tableState : Table.State } -> Html Msg
customGrid model =
    Table.view config model.tableState (List.take 12 model.employers)


df : Maybe String -> String
df str =
    case str of
        Just t ->
            t

        Nothing ->
            ""


config : Table.Config BillingCcm Msg
config =
    Table.customConfig
        { toId = .patientName
        , toMsg = SetTableState
        , columns =
            [ checkColumn ""
            , Table.stringColumn "Facility" .facility

            -- , Table.stringColumn "Name" .name
            -- , Table.stringColumn "DoB" .dob
            -- , Table.stringColumn "Provider" .provider
            -- , Table.stringColumn "Prim. Ins." .primaryInsurance
            , editColumn
            ]
        , customizations = defaultCustomizations
        }


defaultCustomizations : Table.Customizations data msg
defaultCustomizations =
    { tableAttrs = [ id "employersTable" ]
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
        Html.th [ onClick ] content


simpleThead : List ( String, Table.Status, Html.Attribute msg ) -> Table.HtmlDetails msg
simpleThead headers =
    Table.HtmlDetails [] (List.map simpleTheadHelp headers)


simpleRowAttrs : data -> List (Html.Attribute msg)
simpleRowAttrs _ =
    []


editColumn : Table.Column BillingCcm Msg
editColumn =
    Table.veryCustomColumn
        { name = ""
        , viewData = editColumnCell
        , sorter = Table.unsortable
        }


editColumnCell : BillingCcm -> Table.HtmlDetails Msg
editColumnCell emp =
    Table.HtmlDetails []
        [ button [ class "btn btn-default", onClick (EditStart emp) ] [ text "Edit" ]
        ]


checkColumn : String -> Table.Column BillingCcm Msg
checkColumn name =
    Table.veryCustomColumn
        { name = name
        , viewData = checkColumnCell
        , sorter = Table.unsortable
        }


checkColumnCell : BillingCcm -> Table.HtmlDetails Msg
checkColumnCell emp =
    Table.HtmlDetails []
        [ input [ type_ "checkbox" ] []
        ]

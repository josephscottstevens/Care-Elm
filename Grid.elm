module Grid exposing (customGrid, config)

import Model exposing (..)
import Html exposing (Html, text, div, program, button, input, span)
import Html.Attributes exposing (style, class, type_, id)
import Html.Events exposing (onClick)
import Table


customGrid : { a | employers : List BillingCcm, tableState : Table.State } -> Html Msg
customGrid model =
    Table.view config model.tableState (List.take 12 model.employers)


defaultString : Maybe String -> String
defaultString str =
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
            , Table.stringColumn "Billing Date" .billingDate
            , Table.stringColumn "Main Provider" .mainProvider
            , Table.stringColumn "Patient Name" .patientName
            , Table.stringColumn "DOB" .dob
            , maybeStringColumn "Patient's Facility Id No" .patientFacilityIdNo
            , maybeStringColumn "AssignedTo" .assignedTo
            , editColumn
            ]
        , customizations = defaultCustomizations
        }


defaultCustomizations : Table.Customizations BillingCcm msg
defaultCustomizations =
    { tableAttrs = [ id "employersTable", class "e-table" ]
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
        Html.th [ onClick, class "e-columnheader" ] content


simpleThead : List ( String, Table.Status, Html.Attribute msg ) -> Table.HtmlDetails msg
simpleThead headers =
    Table.HtmlDetails [] (List.map simpleTheadHelp headers)


simpleRowAttrs : BillingCcm -> List (Html.Attribute msg)
simpleRowAttrs billingCcm =
    if billingCcm.iD % 2 == 0 then
        [ class "e-row" ]
    else
        [ class "e-alt_row" ]


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


maybeStringColumn : String -> (BillingCcm -> Maybe String) -> Table.Column BillingCcm Msg
maybeStringColumn name field =
    Table.veryCustomColumn
        { name = name
        , viewData = maybeStringColumnCell << field
        , sorter = Table.unsortable
        }


maybeStringColumnCell : Maybe String -> Table.HtmlDetails Msg
maybeStringColumnCell str =
    Table.HtmlDetails []
        [ div [] [ text (defaultString str) ]
        ]

module Grid exposing (customGrid, config)

import Model exposing (..)
import Html exposing (Html, text, div, program, button, input, span)
import Html.Attributes exposing (style, class, type_, id)
import Html.Events exposing (onClick)
import Table
import CommonGrid exposing (..)


customGrid : { a | employers : List BillingCcm, tableState : Table.State } -> Html Msg
customGrid model =
    Table.view config model.tableState (List.take 12 model.employers)


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
        [ button [ class "btn btn-sm btn-default fa fa-angle-down btn-context-menu", onClick (EditStart emp), style [ ( "text-align", "right" ) ] ] []
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

module Grid exposing (config, filteredCcm, filteredCcmLength)

import Model exposing (..)
import Html exposing (Html, text, div, program, button, input, span)
import Html.Attributes exposing (style, class, type_, id)
import Html.Events exposing (onClick)
import Table
import CommonGrid exposing (..)


filteredCcm : Model -> List BillingCcm
filteredCcm model =
    let
        lowerQuery =
            String.toLower model.query
    in
        model.billingCcm
            |> List.filter (String.contains lowerQuery << String.toLower << .facility)


filteredCcmLength : Model -> Int
filteredCcmLength model =
    List.length (filteredCcm model)


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
            , Table.stringColumn "Id No" (\t -> defaultString t.patientFacilityIdNo)
            , Table.stringColumn "AssignedTo" (\t -> defaultString t.assignedTo)
            , editColumn
            ]
        , customizations = defaultCustomizations
        }


defaultCustomizations : Table.Customizations BillingCcm msg
defaultCustomizations =
    { tableAttrs = [ id "employersTable", class "e-table e-hidelines" ]
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

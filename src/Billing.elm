module Billing exposing (Msg, Model, emptyModel, subscriptions, init, update, view)

import Html exposing (Html, text, div, input, program, button, select, option, span, a)
import Html.Attributes exposing (style, class, placeholder, id, type_, value, tabindex)
import Html.Events exposing (onClick, onInput)
import Common.Table as Table exposing (defaultCustomizations)
import Common.Grid as Grid
import Common.Functions as Functions exposing (maybeVal, defaultString)
import Common.Types exposing (AddEditDataSource)
import Http
import Json.Decode as Decode
import Json.Decode.Pipeline as Pipeline
import Json.Encode as Encode


init : Int -> Cmd Msg
init patientId =
    load patientId


subscriptions : Sub msg
subscriptions =
    Sub.none


type Page
    = First
    | Previous
    | PreviousBlock
    | Index Int
    | NextBlock
    | Next
    | Last


type alias Model =
    { rows : List Row
    , tableState : Table.State
    , query : String
    , currentPage : Int
    , count : Int
    }


type SortMode
    = SortNone
    | SortAsc
    | SortDesc


type alias Row =
    { id : Int
    , facility : String
    , facilityId : Int
    , practiceLocation : Maybe String
    , mainProvider : String
    , providerId : Int
    , patientName : String
    , patientId : Int
    , dob : String
    , patientFacilityIdNo : Maybe String
    , phone : String
    , assignedTo : Maybe String
    , staffId : Maybe Int
    , openTasks : Int
    , totalTimeSpent : Maybe Int
    , ccmRegistrationDate : String
    , dateOfService : String
    , billingDate : String
    , billingMonth : Int
    , billingYear : Int
    , isClosed : Bool
    , tocId : Maybe Int
    , readmission : Bool
    , isComplexCCM : Bool
    , batchCloseOnInvoiceCompletion : Bool
    , reviewedByStaffName : Maybe String
    , canModifyReviewedStatus : Bool
    , isReviewed : Bool
    , dxPresent : Bool
    , carePlanPresent : Bool
    , medsPresent : Bool
    , allergiesPresent : Bool
    , vitalsPresent : Bool
    , recordingPresent : Bool
    , chartComplete : Bool
    , status : String
    , is24HoursSinceBilled : Bool
    }


view : Model -> Maybe AddEditDataSource -> Html Msg
view model addEditDataSource =
    let
        rows =
            filteredCcm model
                |> List.drop (model.currentPage * pagesPerBlock)
                |> List.take itemsPerPage
    in
        div []
            [ button [ class "btn btn-default", onClick Reset ] [ text "reset" ]
            , input [ class "form-control", placeholder "Search by Facility" ] []
            , div [ class "e-grid e-js e-waitingpopup" ]
                [ Table.view (config addEditDataSource model.tableState) model.tableState rows
                ]
            , pagingView model.currentPage model.count
            ]


type Msg
    = Load (Result Http.Error LoadResult)
    | SetPagingState Page
    | SetQuery String
    | SetTableState Table.State
    | Reset


update : Msg -> Model -> Int -> ( Model, Cmd Msg )
update msg model patientId =
    case msg of
        Load (Ok t) ->
            { model | rows = t.result, count = t.count } ! []

        Load (Err t) ->
            model ! [ Functions.displayErrorMessage (toString t) ]

        SetPagingState page ->
            let
                newPageIndex =
                    getNewState page model.currentPage model.count
            in
                { model | currentPage = newPageIndex } ! []

        SetQuery newQuery ->
            { model | query = newQuery } ! []

        SetTableState newState ->
            { model | tableState = newState } ! []

        Reset ->
            model ! []


filteredCcm : Model -> List Row
filteredCcm model =
    let
        lowerQuery =
            String.toLower model.query
    in
        model.rows
            |> List.filter (String.contains lowerQuery << String.toLower << .facility)



-- Paging stuff


itemsPerPage : Int
itemsPerPage =
    10


pagesPerBlock : Int
pagesPerBlock =
    8


getNewState : Page -> Int -> Int -> Int
getNewState page currentPage totalRows =
    let
        totalPages =
            totalRows // itemsPerPage
    in
        case page of
            First ->
                0

            Previous ->
                if currentPage > 0 then
                    currentPage - 1
                else
                    0

            PreviousBlock ->
                0

            Index t ->
                t

            NextBlock ->
                0

            Next ->
                currentPage + 1

            Last ->
                totalPages - 1


pagingView : Int -> Int -> Html Msg
pagingView currentPage totalVisiblePages =
    let
        totalPages =
            (totalVisiblePages // itemsPerPage) - 1

        activeOrNot pageIndex =
            let
                activeOrNotText =
                    if pageIndex == currentPage then
                        "e-currentitem e-active"
                    else
                        "e-default"
            in
                div
                    [ class ("e-link e-numericitem e-spacing " ++ activeOrNotText), onClick (SetPagingState (Index pageIndex)) ]
                    [ text (toString (pageIndex + 1)) ]

        rng =
            List.range 0 totalPages
                |> List.drop ((currentPage // pagesPerBlock) * pagesPerBlock)
                |> List.take pagesPerBlock
                |> List.map activeOrNot

        firstPageClass =
            if currentPage >= pagesPerBlock then
                "e-icon e-mediaback e-firstpage e-default"
            else
                "e-icon e-mediaback e-firstpagedisabled e-disable"

        leftPageClass =
            if currentPage > 0 then
                "e-icon e-arrowheadleft-2x e-prevpage e-default"
            else
                "e-icon e-arrowheadleft-2x e-prevpagedisabled e-disable"

        leftPageBlockClass =
            if currentPage >= pagesPerBlock then
                "e-link e-spacing e-PP e-numericitem e-default"
            else
                "e-link e-nextprevitemdisabled e-disable e-spacing e-PP"

        rightPageBlockClass =
            if currentPage < totalPages - pagesPerBlock then
                "e-link e-NP e-spacing e-numericitem e-default"
            else
                "e-link e-NP e-spacing e-nextprevitemdisabled e-disable"

        rightPageClass =
            if currentPage < totalPages then
                "e-nextpage e-icon e-arrowheadright-2x e-default"
            else
                "e-icon e-arrowheadright-2x e-nextpagedisabled e-disable"

        lastPageClass =
            if currentPage < totalPages - pagesPerBlock then
                "e-lastpage e-icon e-mediaforward e-default"
            else
                "e-icon e-mediaforward e-animate e-lastpagedisabled e-disable"

        pagerText =
            let
                currentPageText =
                    toString (currentPage + 1)

                totalPagesText =
                    toString (totalPages + 1)

                totalItemsText =
                    toString totalVisiblePages
            in
                currentPageText ++ " of " ++ totalPagesText ++ " pages (" ++ totalItemsText ++ " items)"
    in
        div [ class "e-pager e-js e-pager" ]
            [ div [ class "e-pagercontainer" ]
                [ div [ class firstPageClass, onClick (SetPagingState First) ] []
                , div [ class leftPageClass, onClick (SetPagingState Previous) ] []
                , a [ class leftPageBlockClass, onClick (SetPagingState PreviousBlock) ] [ text "..." ]
                , div [ class "e-numericcontainer e-default" ] rng
                , a [ class rightPageBlockClass, onClick (SetPagingState NextBlock) ] [ text "..." ]
                , div [ class rightPageClass, onClick (SetPagingState Next) ] []
                , div [ class lastPageClass, onClick (SetPagingState Last) ] []
                ]
            , div [ class "e-parentmsgbar", style [ ( "text-align", "right" ) ] ]
                [ span [ class "e-pagermsg" ] [ text pagerText ]
                ]
            ]


decodeBillingCcm : Decode.Decoder Row
decodeBillingCcm =
    Pipeline.decode Row
        |> Pipeline.required "ID" (Decode.int)
        |> Pipeline.required "Facility" (Decode.string)
        |> Pipeline.required "FacilityId" (Decode.int)
        |> Pipeline.required "PracticeLocation" (Decode.maybe Decode.string)
        |> Pipeline.required "MainProvider" (Decode.string)
        |> Pipeline.required "ProviderId" (Decode.int)
        |> Pipeline.required "PatientName" (Decode.string)
        |> Pipeline.required "PatientId" (Decode.int)
        |> Pipeline.required "DoB" (Decode.string)
        |> Pipeline.required "PatientFacilityIdNo" (Decode.maybe Decode.string)
        |> Pipeline.required "Phone" (Decode.string)
        |> Pipeline.required "AssignedTo" (Decode.maybe Decode.string)
        |> Pipeline.required "StaffId" (Decode.maybe Decode.int)
        |> Pipeline.required "OpenTasks" (Decode.int)
        |> Pipeline.required "TotalTimeSpent" (Decode.maybe Decode.int)
        |> Pipeline.required "CcmRegistrationDate" (Decode.string)
        |> Pipeline.required "DateOfService" (Decode.string)
        |> Pipeline.required "BillingDate" (Decode.string)
        |> Pipeline.required "BillingMonth" (Decode.int)
        |> Pipeline.required "BillingYear" (Decode.int)
        |> Pipeline.required "IsClosed" (Decode.bool)
        |> Pipeline.required "TocId" (Decode.maybe Decode.int)
        |> Pipeline.required "Readmission" (Decode.bool)
        |> Pipeline.required "IsComplexCCM" (Decode.bool)
        |> Pipeline.required "BatchCloseOnInvoiceCompletion" (Decode.bool)
        |> Pipeline.required "ReviewedByStaffName" (Decode.maybe Decode.string)
        |> Pipeline.required "CanModifyReviewedStatus" (Decode.bool)
        |> Pipeline.required "IsReviewed" (Decode.bool)
        |> Pipeline.required "DxPresent" (Decode.bool)
        |> Pipeline.required "CarePlanPresent" (Decode.bool)
        |> Pipeline.required "MedsPresent" (Decode.bool)
        |> Pipeline.required "AllergiesPresent" (Decode.bool)
        |> Pipeline.required "VitalsPresent" (Decode.bool)
        |> Pipeline.required "RecordingPresent" Decode.bool
        |> Pipeline.required "ChartComplete" (Decode.bool)
        |> Pipeline.required "Status" (Decode.string)
        |> Pipeline.required "Is24HoursSinceBilled" (Decode.bool)


type alias GridOperations =
    { skip : Int
    , pageSize : Int
    , sortField : Maybe String
    , sortAscending : Maybe Bool
    }


testGridOperations : GridOperations
testGridOperations =
    { skip = 0
    , pageSize = 10
    , sortField = Just "DoB"
    , sortAscending = Just False
    }


testBody : GridOperations -> Encode.Value
testBody gridOperations =
    Encode.object
        [ ( "Skip", Encode.int gridOperations.skip )
        , ( "PageSize", Encode.int gridOperations.pageSize )
        , ( "SortField", maybeVal Encode.string gridOperations.sortField )
        , ( "SortAscending", maybeVal Encode.bool gridOperations.sortAscending )
        ]


type alias LoadResult =
    { result : List Row
    , count : Int
    }


jsonDecodeLoad : Decode.Decoder LoadResult
jsonDecodeLoad =
    Pipeline.decode LoadResult
        |> Pipeline.required "result" (Decode.list decodeBillingCcm)
        |> Pipeline.required "count" Decode.int


load : Int -> Cmd Msg
load patientId =
    jsonDecodeLoad
        |> Functions.postJsonRequest (testBody testGridOperations) ("/People/BillingTest?patientId=" ++ toString patientId)
        |> Http.send Load


emptyModel : Model
emptyModel =
    { rows = []
    , tableState = Table.initialSort "Date"
    , query = ""
    , currentPage = 0
    , count = 10
    }


config : Maybe AddEditDataSource -> Table.State -> Table.Config Row Msg
config addEditDataSource _ =
    let
        buttons =
            case addEditDataSource of
                Just _ ->
                    []

                --[ ( "e-addnew", onClick (Add t) ) ]
                Nothing ->
                    []
    in
        Table.customConfig
            { toId = \t -> toString t.id
            , toMsg = SetTableState
            , columns =
                [ --checkColumn "" ,
                  Table.stringColumn "Facility" .facility
                , Table.stringColumn "Billing Date" .billingDate
                , Table.stringColumn "Main Provider" .mainProvider
                , Table.stringColumn "Patient Name" .patientName
                , Table.stringColumn "DOB" .dob
                , Table.stringColumn "Id No" (\t -> defaultString t.patientFacilityIdNo)
                , Table.stringColumn "AssignedTo" (\t -> defaultString t.assignedTo)
                ]
            , customizations =
                { defaultCustomizations
                    | tableAttrs = Grid.standardTableAttrs "RecordTable"
                    , thead = Grid.standardTheadNoFilters
                    , theadButtons = buttons
                }
            }

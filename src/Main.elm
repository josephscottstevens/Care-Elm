port module Main exposing (main)

import Html exposing (Html, div, img, ul, li, a, text)
import Html.Attributes exposing (id, src, class, href)
import Records
import Demographics
import Common.Types exposing (..)


port openPage : (String -> msg) -> Sub msg


port cancelPage : (String -> msg) -> Sub msg


getSub : Model -> Sub Msg
getSub model =
    case model.page of
        RecordsPage ->
            Sub.map RecordsMsg Records.subscriptions

        DemographicsPage ->
            Sub.map DemographicsMsg (Demographics.subscriptions model.demographicsState model.flags.patientId)

        NoPage ->
            Sub.none


subscriptions : Model -> Sub Msg
subscriptions model =
    Sub.batch
        [ getSub model
        , cancelPage CancelPage
        ]


init : ( Model, Cmd Msg )
init =
    let
        flags =
            { pageFlag = "", patientId = 1, recordType = Nothing }

        model =
            emptyModel flags
    in
        if flags.pageFlag == "records" then
            ( { model | page = RecordsPage }, Cmd.map RecordsMsg (Records.init flags.recordType flags.patientId) )
        else if flags.pageFlag == "demographics" then
            ( { model
                | page = DemographicsPage
                , demographicsState = Demographics.init model.demographicsState flags.patientId
              }
            , Cmd.none
            )
        else
            ( model, Cmd.none )


type Page
    = NoPage
    | RecordsPage
    | DemographicsPage


type alias Model =
    { page : Page
    , recordsState : Records.Model
    , demographicsState : Demographics.Model
    , flags : Flags
    }


type Msg
    = RecordsMsg Records.Msg
    | DemographicsMsg Demographics.Msg
    | CancelPage String


main : Program Never Model Msg
main =
    Html.program
        { init = init
        , view = view
        , update = update
        , subscriptions = subscriptions
        }


type alias NavItem =
    { displayText : String, urlPath : String }


topUrls : List NavItem
topUrls =
    [ { displayText = "Home", urlPath = "/" }
    , { displayText = "Search", urlPath = "/search" }
    , { displayText = "Enrollment", urlPath = "/enrollment" }
    , { displayText = "Communications", urlPath = "/communications" }
    , { displayText = "Records", urlPath = "/records" }
    , { displayText = "Billing", urlPath = "/billing" }
    , { displayText = "Settings", urlPath = "/settings" }
    , { displayText = "Admin", urlPath = "/admin" }
    , { displayText = "Resources", urlPath = "/resources" }
    , { displayText = "Account", urlPath = "/account" }
    ]


ulTag : NavItem -> Html Msg
ulTag t =
    li [] [ a [ class "pointer", href t.urlPath ] [ text t.displayText ] ]


aTagUrls : List (Html Msg)
aTagUrls =
    List.map ulTag topUrls


view : Model -> Html Msg
view model =
    let
        pageBody =
            case model.page of
                NoPage ->
                    div [] []

                RecordsPage ->
                    Html.map RecordsMsg (Records.view model.recordsState model.flags.recordType)

                DemographicsPage ->
                    Html.map DemographicsMsg (Demographics.view model.demographicsState)
    in
        div [ id "mainHeader" ]
            [ div [ id "logoContainer", class "hidden-xs hidden-sm col-md-2 col-lg-3 full-height vertical-align-middle" ]
                [ a [ href "/" ]
                    [ img [ id "clientLogo", src "/Images/Logos/Logo-ncn.png", class "pointer" ] [] --click: function() { navigate('/'); }
                    ]
                ]
            , div [ class "hidden-xs hidden-sm col-md-10 col-lg-9 full-height padding-h-0" ]
                [ ul [ id "mainNav", class "nav nav-pills pull-right" ] aTagUrls
                ]
            ]



--, pageBody


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        RecordsMsg recordsMsg ->
            let
                ( newRecordModel, widgetCmd ) =
                    Records.update recordsMsg model.recordsState model.flags.patientId
            in
                ( { model | recordsState = newRecordModel }, Cmd.map RecordsMsg widgetCmd )

        DemographicsMsg demographicsMsg ->
            let
                ( newDemographicsModel, widgetCmd ) =
                    Demographics.update demographicsMsg model.demographicsState
            in
                ( { model | demographicsState = newDemographicsModel }, Cmd.map DemographicsMsg widgetCmd )

        CancelPage _ ->
            let
                demographicsModel =
                    model.demographicsState
            in
                { model | demographicsState = { demographicsModel | demographicsUrl = Nothing } } ! []


emptyModel : Flags -> Model
emptyModel flags =
    { page = NoPage
    , recordsState = Records.emptyModel
    , demographicsState = Demographics.emptyModel flags.patientId
    , flags = flags
    }

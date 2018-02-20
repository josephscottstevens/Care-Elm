port module Main exposing (main)

import Html exposing (Html)
import Element exposing (column, el, image, row, text, link, empty)
import Element.Attributes exposing (center, fill, fillPortion, width, height, class, padding, spacing, px, verticalCenter, spacingXY, paddingLeft, paddingRight, paddingBottom, paddingTop)
import Color
import Style exposing (style, styleSheet)
import Style.Border as Border
import Style.Color as Color
import Style.Font as Font
import Records
import Demographics
import Common.Types exposing (..)
import Common.Route as Route


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


type MyStyles
    = Root
    | HeaderNav
    | HeaderNavActive
    | HeaderBreadQuick
    | SideNav
    | SideNavActive
    | Body
    | None


navBlue =
    Color.rgb 51 122 183


navBlueActive =
    Color.rgb 187 217 238


stylesheet =
    styleSheet
        [ style Root
            [ --Font.typeface [ Font.importUrl { url = "https://fonts.googleapis.com/css", name = "eb garamond" } ]
              Font.size 20
            ]
        , style HeaderNav
            [ Color.text navBlue
            , Color.background Color.white
            ]
        , style HeaderNavActive
            [ Color.text Color.white
            , Color.background navBlue
            , Border.rounded 4.0
            ]
        , style HeaderBreadQuick
            [ Color.text Color.white
            , Color.background navBlue
            ]
        , style SideNav
            [ Color.text navBlue
            , Color.background Color.white
            ]
        , style SideNavActive
            [ Color.text navBlue
            , Color.background navBlueActive
            ]
        , style Body
            [ Color.text Color.black
            ]
        , style None []
        ]


view model =
    let
        --     pageBody =
        --         case model.page of
        --             NoPage ->
        --                 div [] []
        --             RecordsPage ->
        --                 Html.map RecordsMsg (Records.view model.recordsState model.flags.recordType)
        --             DemographicsPage ->
        --                 Html.map DemographicsMsg (Demographics.view model.demographicsState)
        fr amount =
            width <| fillPortion amount

        toTopUrl navUrl navText =
            let
                activeClass =
                    if navText == "Search" then
                        HeaderNavActive
                    else
                        HeaderNav
            in
                el activeClass [] <| link navUrl <| el None [] (text navText)

        toSideUrl navUrl navText =
            let
                activeClass =
                    if navText == "Profile" then
                        SideNavActive
                    else
                        SideNav
            in
                link navUrl <|
                    el activeClass
                        [ height <| px 40
                        , verticalCenter
                        , paddingLeft 25.0
                        , paddingTop 10.0
                        , paddingBottom 10.0
                        , paddingRight 0.0
                        , width fill
                        ]
                        (text navText)
    in
        Element.layout stylesheet <|
            column None
                []
                [ row None
                    [ width fill, height <| px 59 ]
                    [ column None
                        [ fr 5 ]
                        [ row None
                            []
                            [ image None [ class "pointer" ] { src = "/Images/Logos/Logo-ncn.png", caption = "" } ]
                        ]
                    , column None
                        [ fr 6 ]
                        [ row None
                            [ spacing 14 ]
                            [ toTopUrl "/" "Home"
                            , toTopUrl "/search" "Search"
                            , toTopUrl "/enrollment" "Enrollment"
                            , toTopUrl "/communications" "Communications"
                            , toTopUrl "/records" "Records"
                            , toTopUrl "/billing" "Billing"
                            , toTopUrl "/settings" "Settings"
                            , toTopUrl "/admin" "Admin"
                            , toTopUrl "/resources" "Resources"
                            , toTopUrl "/account" "Account"
                            ]
                        ]
                    ]
                , row HeaderBreadQuick
                    []
                    [ column None
                        []
                        [ row None
                            []
                            [ text "Home..Profile..Dem"
                            ]
                        ]
                    , column None
                        []
                        [ row None
                            []
                            [ text "Home..Profile..Dem"
                            ]
                        ]
                    ]
                , row None
                    []
                    [ column None
                        [ fr 2 ]
                        [ toSideUrl "/" "Profile"
                        , toSideUrl "/" "Services"
                        , toSideUrl "/" "Providers"
                        , toSideUrl "/" "Clinical Summary"
                        , toSideUrl "/" "Tasks"
                        , toSideUrl "/" "Appointments"
                        , toSideUrl "/" "Records"
                        , toSideUrl "/" "Notes"
                        ]
                    , column None
                        [ fr 10 ]
                        []
                    ]
                ]


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

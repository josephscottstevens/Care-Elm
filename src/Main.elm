port module Main exposing (main)

import Html exposing (Html, div)
import Billing
import Demographics
import ClinicalSummary
import Records
import PastMedicalHistory
import Hospitilizations
import Allergies
import Immunizations
import LastKnownVitals
import Common.Functions as Functions
import Common.Types as Common exposing (AddEditDataSource)
import Common.Route as Route exposing (Route)
import Navigation
import Http exposing (Error)
import Json.Decode as Decode exposing (maybe, int, list)
import Json.Decode.Pipeline as Pipeline exposing (required, decode)
import Element exposing (column, el, image, row, text, link, empty, below)
import Element.Input as Input
import Element.Attributes exposing (center, fill, fillPortion, width, height, class, padding, spacing, px, verticalCenter, spacingXY, paddingLeft, paddingRight, paddingBottom, paddingTop, hidden, alignRight, clipX, id)
import Color
import Style exposing (style, styleSheet)
import Style.Border as Border
import Style.Color as Color
import Style.Font as Font


type alias PageInfo =
    { action : String, patientId : Int }


port loadPage : PageInfo -> Cmd msg


port initHeader : Common.PersonHeaderDetails -> Cmd msg


type alias Model =
    { patientId : Int
    , page : Page
    , addEditDataSource : Maybe AddEditDataSource
    , route : Route
    , activePerson : Maybe Common.PersonHeaderDetails
    , selectMenu : Input.SelectWith String Msg
    }


type Page
    = Demographics Demographics.Model
    | Billing Billing.Model
    | ClinicalSummary ClinicalSummary.Model
    | Records Records.Model
    | Hospitilizations Hospitilizations.Model
    | PastMedicalHistory PastMedicalHistory.Model
    | Allergies Allergies.Model
    | Immunizations Immunizations.Model
    | LastKnownVitals LastKnownVitals.Model
      -- Non Elm Routes
    | Home
    | Contacts
    | SocialHistory
    | Employment
    | Insurance
    | Services
    | CCM
    | TCM
    | Providers
    | Tasks
    | Appointments
    | ProblemList
    | Medications
    | Notes
      -- Other
    | NoPage
    | Error String


init : Navigation.Location -> ( Model, Cmd Msg )
init location =
    case Route.getPatientId location of
        Just patientId ->
            setRoute (Route.fromLocation location)
                { patientId = patientId
                , page = NoPage
                , addEditDataSource = Nothing
                , route = Route.None
                , activePerson = Nothing
                , selectMenu = Input.dropMenu Nothing SelectOne
                }

        Nothing ->
            { patientId = 0
            , page = NoPage
            , addEditDataSource = Nothing
            , route = Route.None
            , activePerson = Nothing
            , selectMenu = Input.dropMenu (Just "aa") SelectOne
            }
                ! [ Functions.setLoadingStatus False ]


subscriptions : Model -> Sub Msg
subscriptions model =
    Sub.batch
        [ pageSubscriptions model.page
        ]


view : Model -> Html Msg
view model =
    let
        jsView =
            div [] []

        innerView =
            case model.page of
                Records subModel ->
                    Html.map RecordsMsg (Records.view subModel model.addEditDataSource)

                Demographics subModel ->
                    Html.map DemographicsMsg (Demographics.view subModel)

                Billing subModel ->
                    Html.map BillingMsg (Billing.view subModel model.addEditDataSource)

                ClinicalSummary subModel ->
                    Html.map ClinicalSummaryMsg (ClinicalSummary.view subModel model.patientId)

                PastMedicalHistory subModel ->
                    Html.map PastMedicalHistoryMsg (PastMedicalHistory.view subModel model.addEditDataSource)

                Hospitilizations subModel ->
                    Html.map HospitilizationsMsg (Hospitilizations.view subModel model.addEditDataSource)

                Allergies subModel ->
                    Html.map AllergiesMsg (Allergies.view subModel model.addEditDataSource)

                Immunizations subModel ->
                    Html.map ImmunizationsMsg (Immunizations.view subModel model.addEditDataSource)

                LastKnownVitals subModel ->
                    Html.map LastKnownVitalsMsg (LastKnownVitals.view subModel model.addEditDataSource)

                -- Non Elm Pages
                Home ->
                    jsView

                Contacts ->
                    jsView

                SocialHistory ->
                    jsView

                Employment ->
                    jsView

                Insurance ->
                    jsView

                Services ->
                    jsView

                CCM ->
                    jsView

                TCM ->
                    jsView

                Providers ->
                    jsView

                Tasks ->
                    jsView

                Appointments ->
                    jsView

                ProblemList ->
                    jsView

                Medications ->
                    jsView

                Notes ->
                    jsView

                -- Other
                NoPage ->
                    jsView

                Error str ->
                    div [] [ Html.text str ]
    in
        viewHeader innerView model.route model


pageSubscriptions : Page -> Sub Msg
pageSubscriptions page =
    case page of
        Demographics _ ->
            Sub.map DemographicsMsg Demographics.subscriptions

        ClinicalSummary _ ->
            Sub.map ClinicalSummaryMsg ClinicalSummary.subscriptions

        PastMedicalHistory _ ->
            Sub.map PastMedicalHistoryMsg PastMedicalHistory.subscriptions

        Hospitilizations _ ->
            Sub.map HospitilizationsMsg Hospitilizations.subscriptions

        Allergies _ ->
            Sub.map AllergiesMsg Allergies.subscriptions

        Immunizations _ ->
            Sub.map ImmunizationsMsg Immunizations.subscriptions

        LastKnownVitals _ ->
            Sub.map LastKnownVitalsMsg LastKnownVitals.subscriptions

        Records _ ->
            Sub.map RecordsMsg Records.subscriptions

        Billing _ ->
            Sub.map BillingMsg Billing.subscriptions

        --Non Elm Subs
        Home ->
            Sub.none

        Contacts ->
            Sub.none

        SocialHistory ->
            Sub.none

        Employment ->
            Sub.none

        Insurance ->
            Sub.none

        Services ->
            Sub.none

        CCM ->
            Sub.none

        TCM ->
            Sub.none

        Providers ->
            Sub.none

        Tasks ->
            Sub.none

        Appointments ->
            Sub.none

        ProblemList ->
            Sub.none

        Medications ->
            Sub.none

        Notes ->
            Sub.none

        -- Other
        NoPage ->
            Sub.none

        Error _ ->
            Sub.none


type Msg
    = SetRoute (Maybe Route)
    | BillingMsg Billing.Msg
    | ClinicalSummaryMsg ClinicalSummary.Msg
    | PastMedicalHistoryMsg PastMedicalHistory.Msg
    | HospitilizationsMsg Hospitilizations.Msg
    | AllergiesMsg Allergies.Msg
    | ImmunizationsMsg Immunizations.Msg
    | LastKnownVitalsMsg LastKnownVitals.Msg
    | RecordsMsg Records.Msg
    | DemographicsMsg Demographics.Msg
    | AddEditDataSourceLoaded (Result Http.Error AddEditDataSource)
    | PersonHeaderLoaded (Result Http.Error Common.PersonHeaderDetails)
    | SelectOne (Input.SelectMsg String)


setRoute : Maybe Route -> Model -> ( Model, Cmd Msg )
setRoute maybeRoute model =
    let
        getDropdownsCmd =
            case model.addEditDataSource of
                Just _ ->
                    Cmd.none

                Nothing ->
                    getDropDowns model.patientId

        getPersonHeaderDetailsCmd =
            case model.activePerson of
                Just _ ->
                    Cmd.none

                Nothing ->
                    getPersonHeaderDetails model.patientId

        cmds t =
            [ getDropdownsCmd
            , getPersonHeaderDetailsCmd
            , Functions.setLoadingStatus False
            ]
                ++ t

        setModel route page =
            { model | page = page, route = route }

        jsLoad route page =
            { model | page = page, route = route }
                ! [ loadPage { action = String.dropLeft 1 <| Route.routeUrl route, patientId = model.patientId }
                  ]
    in
        case maybeRoute of
            Just Route.Home ->
                jsLoad Route.Home Home

            -- Patients\Profile
            Just Route.Profile ->
                setModel Route.Demographics (Demographics (Demographics.emptyModel model.patientId))
                    ! cmds [ Cmd.map DemographicsMsg (Demographics.init model.patientId) ]

            Just Route.Demographics ->
                setModel Route.Demographics (Demographics (Demographics.emptyModel model.patientId))
                    ! cmds [ Cmd.map DemographicsMsg (Demographics.init model.patientId) ]

            Just Route.Contacts ->
                jsLoad Route.Contacts Contacts

            Just Route.SocialHistory ->
                jsLoad Route.SocialHistory SocialHistory

            Just Route.Employment ->
                jsLoad Route.Employment Employment

            Just Route.Insurance ->
                jsLoad Route.Insurance Insurance

            -- People/Services
            Just Route.Services ->
                jsLoad Route.Services Services

            Just Route.CCM ->
                jsLoad Route.CCM CCM

            Just Route.TCM ->
                jsLoad Route.TCM TCM

            -- People/Providers
            Just Route.Providers ->
                jsLoad Route.Providers Providers

            --People/ClinicalSummary
            Just Route.ClinicalSummaryRoot ->
                setModel Route.ClinicalSummaryRoot (ClinicalSummary ClinicalSummary.emptyModel)
                    ! cmds [ Cmd.map ClinicalSummaryMsg (ClinicalSummary.init model.patientId) ]

            Just Route.ClinicalSummary ->
                setModel Route.ClinicalSummary (ClinicalSummary ClinicalSummary.emptyModel)
                    ! cmds [ Cmd.map ClinicalSummaryMsg (ClinicalSummary.init model.patientId) ]

            Just Route.ProblemList ->
                jsLoad Route.ProblemList ProblemList

            Just Route.Medications ->
                jsLoad Route.Medications Medications

            Just Route.PastMedicalHistory ->
                setModel Route.PastMedicalHistory (PastMedicalHistory PastMedicalHistory.emptyModel)
                    ! cmds [ Cmd.map PastMedicalHistoryMsg (PastMedicalHistory.init model.patientId) ]

            Just Route.Hospitilizations ->
                setModel Route.Hospitilizations (Hospitilizations Hospitilizations.emptyModel)
                    ! cmds [ Cmd.map HospitilizationsMsg (Hospitilizations.init model.patientId) ]

            Just Route.Immunizations ->
                setModel Route.Immunizations (Immunizations Immunizations.emptyModel)
                    ! cmds [ Cmd.map ImmunizationsMsg (Immunizations.init model.patientId) ]

            Just Route.Allergies ->
                setModel Route.Allergies (Allergies Allergies.emptyModel)
                    ! cmds [ Cmd.map AllergiesMsg (Allergies.init model.patientId) ]

            Just Route.LastKnownVitals ->
                setModel Route.LastKnownVitals (LastKnownVitals LastKnownVitals.emptyModel)
                    ! cmds [ Cmd.map LastKnownVitalsMsg (LastKnownVitals.init model.patientId) ]

            --People/Tasks
            Just Route.Tasks ->
                jsLoad Route.Tasks Tasks

            --People/Appointments
            Just Route.Appointments ->
                jsLoad Route.Appointments Appointments

            --People/Records
            Just Route.RecordsRoot ->
                setModel (Route.Records Common.PrimaryCare) (Records (Records.emptyModel Common.PrimaryCare))
                    ! cmds [ Cmd.map RecordsMsg (Records.init Common.PrimaryCare model.patientId) ]

            Just (Route.Records t) ->
                setModel (Route.Records t) (Records (Records.emptyModel t))
                    ! cmds [ Cmd.map RecordsMsg (Records.init t model.patientId) ]

            --People/Notes
            Just Route.Notes ->
                jsLoad Route.Notes Notes

            --Other
            Just Route.Billing ->
                setModel Route.Billing (Billing Billing.emptyModel)
                    ! cmds [ Cmd.map BillingMsg (Billing.init model.patientId) ]

            Just Route.None ->
                setModel Route.None NoPage
                    ! []

            Just (Route.Error str) ->
                setModel (Route.Error str) (Error str)
                    ! []

            Nothing ->
                setModel (Route.Error "no route provided, map me in Route.routeHash")
                    (Error "no route provided, map me in Route.routeHash")
                    ! []


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    updatePage model.page msg model


updatePage : Page -> Msg -> Model -> ( Model, Cmd Msg )
updatePage page msg model =
    let
        toPage toModel toMsg subUpdate subMsg subModel =
            let
                ( newModel, newCmd ) =
                    subUpdate subMsg subModel model.patientId
            in
                { model | page = toModel newModel } ! [ Cmd.map toMsg newCmd ]
    in
        case ( msg, page ) of
            ( SetRoute route, _ ) ->
                setRoute route model

            ( AddEditDataSourceLoaded response, _ ) ->
                case response of
                    Ok t ->
                        { model | addEditDataSource = Just t } ! []

                    Err t ->
                        { model | page = Error (toString t) } ! []

            ( PersonHeaderLoaded response, _ ) ->
                case response of
                    Ok t ->
                        { model | activePerson = Just t } ! [ initHeader t ]

                    Err t ->
                        { model | page = Error (toString t) } ! []

            ( DemographicsMsg subMsg, Demographics subModel ) ->
                toPage Demographics DemographicsMsg Demographics.update subMsg subModel

            ( PastMedicalHistoryMsg subMsg, PastMedicalHistory subModel ) ->
                toPage PastMedicalHistory PastMedicalHistoryMsg PastMedicalHistory.update subMsg subModel

            ( BillingMsg subMsg, Billing subModel ) ->
                toPage Billing BillingMsg Billing.update subMsg subModel

            ( HospitilizationsMsg subMsg, Hospitilizations subModel ) ->
                toPage Hospitilizations HospitilizationsMsg Hospitilizations.update subMsg subModel

            ( ClinicalSummaryMsg subMsg, ClinicalSummary subModel ) ->
                toPage ClinicalSummary ClinicalSummaryMsg ClinicalSummary.update subMsg subModel

            ( RecordsMsg subMsg, Records subModel ) ->
                toPage Records RecordsMsg Records.update subMsg subModel

            ( AllergiesMsg subMsg, Allergies subModel ) ->
                toPage Allergies AllergiesMsg Allergies.update subMsg subModel

            ( ImmunizationsMsg subMsg, Immunizations subModel ) ->
                toPage Immunizations ImmunizationsMsg Immunizations.update subMsg subModel

            ( LastKnownVitalsMsg subMsg, LastKnownVitals subModel ) ->
                toPage LastKnownVitals LastKnownVitalsMsg LastKnownVitals.update subMsg subModel

            ( SelectOne searchMsg, _ ) ->
                { model | selectMenu = Input.updateSelection searchMsg model.selectMenu }
                    ! []

            _ ->
                { model | page = Error <| "Missing Page\\Message " ++ toString page ++ " !!!__-__!!! " ++ toString msg } ! []


getDropDowns : Int -> Cmd Msg
getDropDowns patientId =
    decode AddEditDataSource
        |> required "facilityId" (maybe Decode.int)
        |> required "patientId" Decode.int
        |> required "facilityDropdown" (Decode.list Functions.decodeDropdownItem)
        |> required "providersDropdown" (Decode.list Functions.decodeDropdownItem)
        |> required "recordTypeDropdown" (Decode.list Functions.decodeDropdownItem)
        |> required "userDropDown" (Decode.list Functions.decodeDropdownItem)
        |> required "taskDropDown" (Decode.list Functions.decodeDropdownItem)
        |> required "hospitilizationServiceTypeDropdown" (Decode.list Functions.decodeDropdownItem)
        |> required "hospitalizationDischargePhysicianDropdown" (Decode.list Functions.decodeDropdownItem)
        |> required "hospitilizations" (Decode.list Functions.decodeDropdownItem)
        |> Http.get ("/People/PatientRecordsDropdowns?patientId=" ++ toString patientId)
        |> Http.send AddEditDataSourceLoaded


getPersonHeaderDetails : Int -> Cmd Msg
getPersonHeaderDetails patientId =
    Pipeline.decode Common.PersonHeaderDetails
        |> Pipeline.required "Id" Decode.int
        |> Pipeline.required "FullName" (Decode.maybe Decode.string)
        |> Pipeline.required "DateOfBirth" (Decode.maybe Decode.string)
        |> Pipeline.required "Age" (Decode.maybe Decode.int)
        |> Pipeline.required "Nickname" (Decode.maybe Decode.string)
        |> Pipeline.required "FacilityName" (Decode.maybe Decode.string)
        |> Pipeline.required "IsVIP" Decode.bool
        |> Pipeline.required "HasNDA" Decode.bool
        |> Pipeline.required "ContactHours" (Decode.list Decode.string)
        |> Pipeline.required "MRN" (Decode.maybe Decode.string)
        |> Pipeline.required "PAN" (Decode.maybe Decode.string)
        |> Pipeline.required "PFID" (Decode.maybe Decode.string)
        |> Pipeline.required "PrimaryResource" (Decode.maybe Decode.string)
        |> Pipeline.required "RestrictionsCount" Decode.int
        |> Pipeline.required "EmailAddress" (Decode.maybe Decode.string)
        |> Pipeline.required "PreferredLanguage" (Decode.maybe Decode.string)
        |> Pipeline.required "FacilityId" Decode.int
        |> Pipeline.required "DateOfDeath" (Decode.maybe Decode.string)
        |> Pipeline.required "MainProvider" (Decode.maybe Decode.string)
        |> Http.get ("/People/GetPersonHeaderDetails?patientId=" ++ toString patientId)
        |> Http.send PersonHeaderLoaded


main : Program Never Model Msg
main =
    Navigation.program (Route.fromLocation >> SetRoute)
        { init = init
        , view = view
        , update = update
        , subscriptions = subscriptions
        }



-- Style STuff


type MyStyles
    = Root
    | HeaderNav
    | HeaderNavActive
    | HeaderBreadQuick
    | SideNav
    | SideNavActive
    | SideNavParentActive
    | SideNavChildActive
    | HeaderPatient
    | HeaderPatientLarge
    | BoldText
    | Body
    | None


navBlue : Color.Color
navBlue =
    Color.rgb 51 122 183


navLightBlue : Color.Color
navLightBlue =
    Color.rgb 160 216 250


navBlueActive : Color.Color
navBlueActive =
    Color.rgb 187 217 238


veryLightBlue : Color.Color
veryLightBlue =
    Color.rgb 235 244 250


veryLightGray : Color.Color
veryLightGray =
    Color.rgb 238 238 238


navBlueActiveText : Color.Color
navBlueActiveText =
    Color.rgb 135 206 250


stylesheet : Style.StyleSheet MyStyles variation
stylesheet =
    styleSheet
        [ style Root
            [ --Font.typeface [ Font.importUrl { url = "https://fonts.googleapis.com/css", name = "eb garamond" } ]
              Font.size 14
            , Border.left 1.0
            , Color.border Color.lightGray
            ]
        , style HeaderNav
            [ Color.text navBlue
            , Color.background Color.white
            , Border.rounded 4.0
            , Style.hover [ Color.background veryLightGray ]
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
            , Style.hover [ Color.background navLightBlue ]
            ]
        , style SideNavActive
            [ Color.text navBlue
            , Color.background navLightBlue
            , Style.hover [ Color.background navLightBlue ]
            ]
        , style SideNavParentActive
            [ Color.text navBlue
            , Color.background navBlueActive
            , Style.hover [ Color.background navLightBlue ]
            ]
        , style SideNavChildActive
            [ Color.text navBlue
            , Color.background veryLightBlue
            , Style.hover [ Color.background navLightBlue ]
            ]
        , style HeaderPatient
            [ Color.background (Color.rgb 245 245 220)
            ]
        , style HeaderPatientLarge
            [ Font.size 24
            , Font.weight 600
            , Color.text (Color.rgb 51 51 51)
            ]
        , style BoldText
            [ Font.bold
            ]
        , style Body
            [ Color.text Color.black
            ]
        , style None []
        ]


findActiveClass : Route.RouteDesc -> Route.Route -> MyStyles
findActiveClass route activeRoute =
    let
        parentId : Maybe Int
        parentId =
            Route.getParentFromRoute activeRoute
                |> Maybe.map Route.routeId
    in
        if route.id == Route.routeId activeRoute then
            SideNavActive
        else if Just route.id == parentId then
            SideNavParentActive
        else if Route.getParentFromRoute route.route == Route.getParentFromRoute activeRoute then
            SideNavChildActive
        else
            SideNav


fr : Int -> Element.Attribute variation msg
fr amount =
    width <| fillPortion amount


viewHeader innerView activeRoute model =
    let
        toTopUrl navUrl navText =
            let
                activeClass =
                    -- TODO, this needs to bubble up to the parent
                    if navText == "Home" then
                        HeaderNavActive
                    else
                        HeaderNav
            in
                el activeClass
                    [ paddingLeft 7
                    , paddingRight 7
                    , paddingTop 10
                    , paddingBottom 10
                    ]
                    (link navUrl <| el None [] (text navText))

        toSideUrl t =
            link t.url <|
                el (findActiveClass t activeRoute)
                    [ height <| px 40
                    , verticalCenter
                    , paddingLeft (10 + (t.depth * 15))
                    , paddingTop 10.0
                    , paddingBottom 10.0
                    , paddingRight 0.0
                    , width fill
                    ]
                    (text t.navText)

        sideNav =
            Route.getSideNav
                |> List.filter
                    (\t ->
                        t.depth
                            == 0.0
                            || case findActiveClass t activeRoute of
                                SideNav ->
                                    False

                                _ ->
                                    True
                    )
                |> List.map toSideUrl

        toBreadCrumbs { route } =
            el HeaderNavActive [] <| link (Route.routeUrl route) <| el None [] (text (Route.routeDescription route))

        headerBreakQuick =
            Route.getBreadcrumbsFromRoute activeRoute
                |> List.map toBreadCrumbs
                |> List.intersperse (el HeaderNavActive [] (text "|"))
    in
        Element.layout stylesheet <|
            column Root
                [ clipX ]
                [ row None
                    [ width fill, height <| px 59 ]
                    [ column None
                        [ fr 5 ]
                        [ row None
                            []
                            [ image None [ class "pointer" ] { src = "/Images/Logos/Logo-ncn.png", caption = "" } ]
                        ]
                    , column None
                        [ fr 6, alignRight ]
                        [ row None
                            []
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

                            --TODO, I think special logic goes here
                            , toTopUrl "/logout" "Logout"
                            ]
                        ]
                    ]
                , row HeaderBreadQuick
                    [ spacing 6, paddingTop 9, paddingBottom 9, paddingLeft 10 ]
                    headerBreakQuick
                , row None
                    []
                    [ column None
                        [ fr 2 ]
                        sideNav
                    , column None
                        [ fr 10 ]
                        (viewPatientHeader model
                            ++ [ row None
                                    [ paddingLeft 10, paddingTop 10, paddingRight 10 ]
                                    [ el None [ class "body-content" ] <| Element.html innerView
                                    ]
                               ]
                        )
                    ]
                ]


viewPatientHeader model =
    case model.activePerson of
        Just p ->
            let
                headerPad =
                    [ paddingTop 5, paddingBottom 5 ]

                headerPadRight =
                    [ paddingTop 5, paddingBottom 5, paddingRight 10 ]

                maybeText t =
                    text <| Maybe.withDefault "" t

                contactHoursFormat t =
                    Html.li [] [ Html.text t ]

                contactHours =
                    List.map contactHoursFormat p.contactHours
            in
                [ row HeaderPatient
                    [ paddingLeft 10 ]
                    [ column None
                        [ fr 2 ]
                        [ el HeaderPatientLarge [] <| maybeText p.fullName
                        ]
                    , column None
                        [ fr 3, alignRight ]
                        [ Input.select None
                            [ padding 10
                            , spacing 20
                            ]
                            { label = Input.labelLeft <| text "Lunch"
                            , with = model.selectMenu
                            , max = 20
                            , options = []
                            , menu =
                                Input.menu None
                                    []
                                    [ Input.choice "1" (text "Taco!")
                                    , Input.choice "2" (text "Gyro")
                                    , Input.styledChoice "3" <|
                                        \selected ->
                                            Element.row None
                                                [ spacing 5 ]
                                                [ el None [] <|
                                                    if selected then
                                                        text ":D"
                                                    else
                                                        text ":("
                                                , text "burrito"
                                                ]
                                    ]
                            }
                        ]
                    ]
                , row HeaderPatient
                    [ width fill, paddingLeft 10 ]
                    [ el BoldText headerPad <| text "Date of Birth: "
                    , el None headerPadRight <| maybeText p.dateOfBirth
                    , el BoldText headerPad <| text "Age: "
                    , el None headerPadRight <| text (toString p.age)
                    ]
                , row HeaderPatient
                    []
                    [ el BoldText headerPad <| text "Current Service: "
                    , el None headerPadRight <| el None [ id "bob" ] empty
                    ]
                ]

        Nothing ->
            []



--                            (Element.html (Html.ul [ Attributes.id "contactHoursMenu" ] contactHours))

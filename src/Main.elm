port module Main exposing (main)

import Html exposing (Html, div)
import Html.Attributes as Attribute
import Html.Events as Events
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
import Json.Encode as Encode
import Element exposing (column, el, image, row, text, link, empty, below)
import Element.Input as Input
import Element.Attributes exposing (center, fill, fillPortion, width, height, class, padding, spacing, px, verticalCenter, spacingXY, paddingLeft, paddingRight, paddingBottom, paddingTop, hidden, alignRight, clipX, id)
import Color
import Style exposing (style, styleSheet)
import Style.Border as Border
import Style.Color as Color
import Style.Font as Font
import Element.Events exposing (onClick)


type alias PageInfo =
    { action : String, patientId : Int }


port loadPage : PageInfo -> Cmd msg


port initHeader : PersonHeaderDetails -> Cmd msg


port openServiceHistory : String -> Cmd msg


port openRestrictions : String -> Cmd msg


type alias Model =
    { patientId : Int
    , page : Page
    , addEditDataSource : Maybe AddEditDataSource
    , route : Route
    , activePerson : Maybe PersonHeaderDetails
    , selectMenu : Input.SelectWith String Msg
    , restrictions : List RestrictionDetail
    , serviceDetails : List ServiceDetail
    , currentServiceDetail : ServiceDetail
    }


type alias PersonHeaderDetails =
    { patientId : Int
    , fullName : Maybe String
    , dateOfBirth : Maybe String
    , age : Maybe Int
    , nickname : Maybe String
    , facilityName : Maybe String
    , isVIP : Bool
    , hasNDA : Bool
    , contactHours : List String
    , mRN : Maybe String
    , pAN : Maybe String
    , pFID : Maybe String
    , primaryResource : Maybe String
    , restrictionsCount : Int
    , emailAddress : Maybe String
    , preferredLanguage : Maybe String
    , facilityId : Int
    , dateOfDeath : Maybe String
    , mainProvider : Maybe String
    }


type alias RestrictionDetail =
    { contactName : Maybe String
    , roleTypeName : Maybe String
    }


type alias ServiceDetail =
    { serviceType : Maybe String
    , serviceStart : Maybe String
    , serviceEnd : Maybe String
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
    let
        maybePatientId =
            Route.getPatientId location
    in
        case maybePatientId of
            Just patientId ->
                setRoute (Route.fromLocation location)
                    { patientId = patientId
                    , page = NoPage
                    , addEditDataSource = Nothing
                    , route = Route.None
                    , activePerson = Nothing
                    , selectMenu = Input.dropMenu (Just "Contact Hours") SelectOne
                    , restrictions = []
                    , serviceDetails = []
                    , currentServiceDetail = emptyCurrentServiceDetail
                    }

            Nothing ->
                { patientId = 0
                , page = NoPage
                , addEditDataSource = Nothing
                , route = Route.None
                , activePerson = Nothing
                , selectMenu = Input.dropMenu (Just "Contact Hours") SelectOne
                , restrictions = []
                , serviceDetails = []
                , currentServiceDetail = emptyCurrentServiceDetail
                }
                    ! [ Functions.setLoadingStatus False
                      , getRestrictionDetail maybePatientId
                      , getServiceDetails maybePatientId
                      , getCurrentServiceDetail maybePatientId
                      ]


subscriptions : Model -> Sub Msg
subscriptions model =
    Sub.batch
        [ pageSubscriptions model.page
        ]


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
    | OpenServiceHistory
    | OpenRestrictions
    | AddEditDataSourceLoaded (Result Http.Error AddEditDataSource)
    | PersonHeaderLoaded (Result Http.Error PersonHeaderDetails)
    | RestrictionLoaded (Result Http.Error (List RestrictionDetail))
    | ServiceDetailsLoaded (Result Http.Error (List ServiceDetail))
    | CurrentServiceDetailLoaded (Result Http.Error ServiceDetail)
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
                ! [ loadPage { action = String.filter Functions.noHash <| Route.routeUrl route, patientId = model.patientId }
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

            ( OpenServiceHistory, _ ) ->
                model ! [ openServiceHistory "" ]

            ( OpenRestrictions, _ ) ->
                model ! [ openRestrictions "" ]

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

            ( RestrictionLoaded response, _ ) ->
                case response of
                    Ok t ->
                        { model | restrictions = t } ! []

                    Err t ->
                        { model | page = Error (toString t) } ! []

            ( ServiceDetailsLoaded response, _ ) ->
                case response of
                    Ok t ->
                        { model | serviceDetails = t } ! []

                    Err t ->
                        { model | page = Error (toString t) } ! []

            ( CurrentServiceDetailLoaded response, _ ) ->
                case response of
                    Ok t ->
                        { model | currentServiceDetail = t } ! []

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
    Pipeline.decode PersonHeaderDetails
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


decodeRestrictionDetail : Decode.Decoder RestrictionDetail
decodeRestrictionDetail =
    Pipeline.decode RestrictionDetail
        |> Pipeline.required "ContactName" (Decode.maybe Decode.string)
        |> Pipeline.required "RoleTypeName" (Decode.maybe Decode.string)


decodeServiceDetail : Decode.Decoder ServiceDetail
decodeServiceDetail =
    Pipeline.decode ServiceDetail
        |> Pipeline.required "ServiceType" (Decode.maybe Decode.string)
        |> Pipeline.required "ServiceStart" (Decode.maybe Decode.string)
        |> Pipeline.required "ServiceEnd" (Decode.maybe Decode.string)


emptyPersonHeaderDetails : PersonHeaderDetails
emptyPersonHeaderDetails =
    { patientId = 0
    , fullName = Nothing
    , dateOfBirth = Nothing
    , age = Nothing
    , nickname = Nothing
    , facilityName = Nothing
    , isVIP = False
    , hasNDA = False
    , contactHours = []
    , mRN = Nothing
    , pAN = Nothing
    , pFID = Nothing
    , primaryResource = Nothing
    , restrictionsCount = 0
    , emailAddress = Nothing
    , preferredLanguage = Nothing
    , facilityId = 0
    , dateOfDeath = Nothing
    , mainProvider = Nothing
    }


emptyCurrentServiceDetail : ServiceDetail
emptyCurrentServiceDetail =
    { serviceType = Nothing
    , serviceStart = Nothing
    , serviceEnd = Nothing
    }


getRestrictionDetail : Maybe Int -> Cmd Msg
getRestrictionDetail maybePatientId =
    case maybePatientId of
        Just patientId ->
            Decode.list decodeRestrictionDetail
                |> Http.get ("/People/getrestrictions?patientId=" ++ toString patientId)
                |> Http.send RestrictionLoaded

        Nothing ->
            Cmd.none


getServiceDetails : Maybe Int -> Cmd Msg
getServiceDetails maybePatientId =
    case maybePatientId of
        Just patientId ->
            Decode.list decodeServiceDetail
                |> Http.get ("/People/GetPatientServiceHistory?patientId=" ++ toString patientId)
                |> Http.send ServiceDetailsLoaded

        Nothing ->
            Cmd.none


getCurrentServiceDetail : Maybe Int -> Cmd Msg
getCurrentServiceDetail maybePatientId =
    case maybePatientId of
        Just patientId ->
            Pipeline.decode ServiceDetail
                |> Pipeline.requiredAt [ "ServiceInfo", "ServiceType" ] (Decode.maybe Decode.string)
                |> Pipeline.requiredAt [ "ServiceInfo", "ServiceStart" ] (Decode.maybe Decode.string)
                |> Pipeline.requiredAt [ "ServiceInfo", "ServiceEnd" ] (Decode.maybe Decode.string)
                |> Http.get ("/People/getcurrentservicedetails?patientId=" ++ toString patientId)
                |> Http.send CurrentServiceDetailLoaded

        Nothing ->
            Cmd.none


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
    | HeaderPatientLabel
    | HeaderPatientText
    | HeaderPatientLarge
    | Body
    | Hyperlink
    | ContactHours
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
              [ "Helvetica Neue", "Helvetica", "Arial", "sans-serif" ] |> List.map Font.font |> Font.typeface
            , Font.size 14
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
        , style HeaderPatientLabel
            [ Font.weight 600
            , Font.size 12.0
            ]
        , style HeaderPatientText
            [ Font.weight 400
            , Font.size 12.0
            ]
        , style HeaderPatientLarge
            [ Font.size 24
            , Font.weight 500
            , Color.text (Color.rgb 51 51 51)
            ]
        , style Body
            [ Color.text Color.black
            ]
        , style Hyperlink
            [ Color.text <| Color.rgb 51 122 183
            , Style.cursor "pointer"
            ]
        , style ContactHours
            [ Color.background Color.white
            , [ "Arial" ] |> List.map Font.font |> Font.typeface
            , Font.weight 400
            , Font.size 13.0
            , Border.all 1.0
            , Border.solid
            , Color.border <| Color.rgb 206 206 206
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


view : Model -> Html Msg
view model =
    let
        jsView =
            div [ Attribute.class "body-content" ] []

        innerView : Html Msg
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
        viewHeader innerView model


viewHeader : Html Msg -> Model -> Html Msg
viewHeader innerView model =
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
                el (findActiveClass t model.route)
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
            case model.activePerson of
                Just _ ->
                    Route.getSideNav
                        |> List.filter
                            (\t ->
                                t.depth
                                    == 0.0
                                    || case findActiveClass t model.route of
                                        SideNav ->
                                            False

                                        _ ->
                                            True
                            )
                        |> List.map toSideUrl

                Nothing ->
                    []

        toBreadCrumbs { route } =
            el HeaderNavActive [] <| link (Route.routeUrl route) <| el None [] (text (Route.routeDescription route))

        headerBreakQuick =
            Route.getBreadcrumbsFromRoute model.route
                |> List.map toBreadCrumbs
                |> List.intersperse (el HeaderNavActive [] (text "|"))

        findServiceStatus maybeServiceType =
            case maybeServiceType of
                Just serviceType ->
                    if String.contains "CCM" serviceType then
                        model.serviceDetails
                            |> List.filterMap
                                (\t ->
                                    case ( t.serviceType, t.serviceEnd ) of
                                        ( Just serviceType, Nothing ) ->
                                            if String.contains "TCM" serviceType then
                                                Just "Pending TCM Discharge"
                                            else
                                                Nothing

                                        _ ->
                                            Nothing
                                )
                            |> List.head
                            |> Maybe.withDefault "Present"
                    else
                        "Present"

                Nothing ->
                    "Present"
    in
        div []
            [ Element.layout stylesheet <|
                column Root
                    [ clipX ]
                    [ row None
                        [ width fill, height <| px 52 ]
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
                                        [ el None [ width fill, height fill ] <| Element.html innerView
                                        ]
                                   ]
                            )
                        ]
                    ]
            , div
                [ Attribute.id "restrictions"
                , Attribute.style [ ( "display", "none" ) ]
                ]
                (List.map
                    (\t ->
                        div [ Attribute.class "padding-top-5 padding-bottom-5" ]
                            [ Html.label [] [ Html.text <| Functions.defaultString t.roleTypeName ]
                            , Html.span [] [ Html.text <| Functions.defaultString t.contactName ]
                            ]
                    )
                    model.restrictions
                )
            , div
                [ Attribute.id "patientServiceHistory"
                , Attribute.style [ ( "display", "none" ) ]
                ]
                ((List.map
                    (\t ->
                        div [ Attribute.class "padding-top-5 padding-bottom-5" ]
                            [ Html.span [ Attribute.class "col-xs-6 text-align-right" ]
                                [ Html.span [] [ Html.text <| Functions.defaultString t.serviceType ]
                                , Html.span [ Encode.string "&nbsp;-&nbsp;" |> Attribute.property "innerHTML" ] []
                                , case t.serviceEnd of
                                    Just serviceEnd ->
                                        Html.span [] [ Html.text serviceEnd ]

                                    Nothing ->
                                        Html.span [] [ Html.text <| findServiceStatus t.serviceType ]
                                ]
                            ]
                    )
                    model.serviceDetails
                 )
                    ++ [ Html.br [] [], Html.br [] [], Html.br [] [] ]
                )
            ]


viewPatientHeader : Model -> List (Element.Element MyStyles variation Msg)
viewPatientHeader model =
    case model.activePerson of
        Just p ->
            let
                headerPad =
                    [ paddingTop 5, paddingBottom 5 ]

                headerPadRight =
                    [ paddingTop 5, paddingBottom 5, paddingRight 10 ]

                contactHoursFormat t =
                    Input.choice t (text t)

                contactHours =
                    List.map contactHoursFormat p.contactHours

                label displayText =
                    el HeaderPatientLabel headerPad <| text (displayText ++ ": ")

                value displayValue =
                    el HeaderPatientText headerPadRight <| text <| displayValue

                currentServiceDetailComputed =
                    case model.currentServiceDetail.serviceType of
                        Just serviceType ->
                            serviceType ++ ": Started " ++ (Functions.defaultString model.currentServiceDetail.serviceStart)

                        Nothing ->
                            "No current service"
            in
                [ row HeaderPatient
                    [ paddingLeft 10 ]
                    [ column None
                        [ fr 5 ]
                        [ row None
                            [ paddingTop 10 ]
                            [ el HeaderPatientLarge [] <| text <| Functions.defaultString p.fullName ]
                        ]
                    , column None
                        [ fr 1 ]
                        [ row None
                            [ paddingTop 10, paddingRight 5, spacing 5 ]
                            [ el None [] <|
                                Element.html <|
                                    Html.button
                                        [ Attribute.class "btn btn-danger btn-sm margin-bottom-5 header-button"
                                        , Events.onClick OpenRestrictions
                                        ]
                                        [ Html.text "Restrictions (0)" ]

                            --TODO, count of actual restrictions
                            , el None [] <|
                                Element.html <|
                                    Html.button
                                        [ Attribute.class "btn btn-default btn-sm fa fa-phone margin-bottom-5 header-button"
                                        , Attribute.style [ ( "color", "green" ), ( "padding-top", "8px" ), ( "padding-bottom", "7px" ) ]
                                        ]
                                        []
                            , Input.select ContactHours
                                [ spacing 5
                                , paddingRight 9
                                , paddingTop 7
                                , paddingLeft 9
                                , paddingBottom 7
                                ]
                                { label = Input.hiddenLabel ""
                                , with = model.selectMenu
                                , max = 20
                                , options = []
                                , menu =
                                    Input.menu HeaderPatientText
                                        []
                                        ([ Input.styledChoice "Contact Hours" <|
                                            \t ->
                                                row None
                                                    [ paddingRight 7 ]
                                                    [ el None [] (text "Contact Hours")
                                                    ]
                                         ]
                                            ++ contactHours
                                        )
                                }
                            ]
                        ]
                    ]
                , row HeaderPatient
                    [ width fill, paddingLeft 10 ]
                    [ label "Date of Birth"
                    , value (Functions.defaultString p.dateOfBirth)
                    , label "Age"
                    , value (Functions.defaultIntToString p.age)
                    , label "Preferred Language"
                    , value (Functions.defaultString p.preferredLanguage)
                    ]
                , row HeaderPatient
                    [ width fill, paddingLeft 10 ]
                    [ label "Facility"
                    , value (Functions.defaultString p.facilityName)
                    , label "Main Provider"
                    , value (Functions.defaultString p.mainProvider)
                    , label "Care Coordinator"
                    , value (Functions.defaultString p.primaryResource)
                    , label "Medical Record No"
                    , value (Functions.defaultString p.mRN)
                    , label "Patient's Facility ID No"
                    , value (Functions.defaultString p.pFID)
                    ]
                , row HeaderPatient
                    [ width fill, paddingLeft 10 ]
                    [ label "Current Service"
                    , value "No current service"
                    , el Hyperlink
                        (headerPadRight ++ [ onClick OpenServiceHistory ])
                      <|
                        text <|
                            currentServiceDetailComputed
                    ]
                ]

        Nothing ->
            []

port module Main exposing (main)

import Html exposing (Html, text, div)
import Billing
import Demographics
import ClinicalSummary
import Records
import PastMedicalHistory
import Hospitilizations
import Allergies
import Immunizations
import LastKnownVitals
import Common.SharedView as SharedView
import Common.Functions as Functions
import Common.Types exposing (AddEditDataSource)
import Common.Route as Route exposing (Route)
import Navigation
import Http exposing (Error)
import Json.Decode as Decode exposing (maybe, int, list)
import Json.Decode.Pipeline exposing (required, decode)


type alias PageInfo =
    { action : String, patientId : Int }


port loadPage : PageInfo -> Cmd msg


type alias Model =
    { patientId : Int
    , page : Page
    , addEditDataSource : Maybe AddEditDataSource
    , route : Route
    , activePerson : Common.Types.ActivePerson
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
    | None
    | Error String


examplePerson : Common.Types.ActivePerson
examplePerson =
    { patientId = 11934
    , firstName = "testFirstNameA"
    , lastName = "testLastNameA"
    , dateOfBirth = "2/1/2018"
    , age = 0
    , preferredLanguage = "Spanish; Castilian"
    , facilityId = 0
    , facilityText = "Advanced Internal Medicine"
    , mainProviderId = 0
    , mainProviderText = "Abbot, Joel"
    , careCoordinatorId = 0
    , careCoordinatorText = "Rayos CMA, Kellie"
    , medicalRecordNo = "1241240124"
    , patientsFacilityIdNo = 12345

    --todo, More than just a string here
    , currentService = "No Current Service"
    }


init : Navigation.Location -> ( Model, Cmd Msg )
init location =
    case Route.getPatientId location of
        Just patientId ->
            setRoute (Route.fromLocation location)
                { patientId = patientId
                , page = None
                , addEditDataSource = Nothing
                , route = Route.None
                , activePerson = examplePerson
                }

        Nothing ->
            { patientId = 0
            , page = None
            , addEditDataSource = Nothing
            , route = Route.None
            , activePerson = examplePerson
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
                None ->
                    jsView

                Error str ->
                    div [] [ text str ]
    in
        SharedView.view innerView model.route model.activePerson


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
        None ->
            Sub.none

        Error _ ->
            Sub.none


type Msg
    = SetRoute (Maybe Route)
    | BillingMsg Billing.Msg
    | ClinicalSummaryMsg ClinicalSummary.Msg
    | AddEditDataSourceLoaded (Result Http.Error AddEditDataSource)
    | PastMedicalHistoryMsg PastMedicalHistory.Msg
    | HospitilizationsMsg Hospitilizations.Msg
    | AllergiesMsg Allergies.Msg
    | ImmunizationsMsg Immunizations.Msg
    | LastKnownVitalsMsg LastKnownVitals.Msg
    | RecordsMsg Records.Msg
    | DemographicsMsg Demographics.Msg


setRoute : Maybe Route -> Model -> ( Model, Cmd Msg )
setRoute maybeRoute model =
    let
        getDropdownsCmd =
            case model.addEditDataSource of
                Just _ ->
                    Cmd.none

                Nothing ->
                    getDropDowns model.patientId AddEditDataSourceLoaded

        cmds t =
            [ getDropdownsCmd, Functions.setLoadingStatus False ] ++ t

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
                setModel (Route.Records Common.Types.PrimaryCare) (Records (Records.emptyModel Common.Types.PrimaryCare))
                    ! cmds [ Cmd.map RecordsMsg (Records.init Common.Types.PrimaryCare model.patientId) ]

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
                setModel Route.None None
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

            _ ->
                { model | page = Error <| "Missing Page\\Message " ++ toString page ++ " !!!__-__!!! " ++ toString msg } ! []


getDropDowns : Int -> (Result Http.Error AddEditDataSource -> msg) -> Cmd msg
getDropDowns patientId t =
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
        |> Http.send t


main : Program Never Model Msg
main =
    Navigation.program (Route.fromLocation >> SetRoute)
        { init = init
        , view = view
        , update = update
        , subscriptions = subscriptions
        }

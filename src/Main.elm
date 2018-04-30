module Main exposing (main)

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
import Window
import Common.Dialog as Dialog
import Common.Types exposing (DialogClose)
import Task
import Http exposing (Error)
import Json.Decode as Decode
import Json.Decode.Pipeline exposing (required, decode)


type alias Model =
    { patientId : Int
    , rootDialog : Dialog.RootDialog
    , page : Page
    , addEditDataSource : Maybe AddEditDataSource
    , route : Route
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
      -- Other
    | NoPage
    | Error String


init : Navigation.Location -> ( Model, Cmd Msg )
init location =
    let
        maybePatientId =
            Route.getPatientId location

        patientId =
            Maybe.withDefault -1 maybePatientId

        ( model, cmds ) =
            setRoute (Route.fromLocation location)
                { patientId = patientId
                , rootDialog = { windowSize = Window.Size 0 0, top = 0, left = 0 }
                , page = NoPage
                , addEditDataSource = Nothing
                , route = Route.None
                }
    in
        model
            ! [ Functions.setLoadingStatus False
              , cmds
              , Task.perform Resize Window.size
              ]


subscriptions : Model -> Sub Msg
subscriptions model =
    Sub.batch
        [ pageSubscriptions model.page
        , Window.resizes Resize
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

        -- Other
        NoPage ->
            Sub.none

        Error _ ->
            Sub.none


view : Model -> Html Msg
view model =
    case model.page of
        Records subModel ->
            Html.map RecordsMsg (Records.view subModel model.addEditDataSource)

        Demographics subModel ->
            Html.map DemographicsMsg (Demographics.view subModel)

        Billing subModel ->
            Html.map BillingMsg (Billing.view subModel model.patientId model.addEditDataSource model.rootDialog)

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

        -- Other
        NoPage ->
            div [] []

        Error str ->
            div [] [ Html.text str ]


type Msg
    = SetRoute (Maybe Route)
    | Resize Window.Size
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


setRoute : Maybe Route -> Model -> ( Model, Cmd Msg )
setRoute maybeRoute model =
    let
        getDropdownsCmd =
            case model.addEditDataSource of
                Just _ ->
                    Cmd.none

                Nothing ->
                    getDropDowns model.patientId

        cmds t =
            [ getDropdownsCmd
            , Functions.setLoadingStatus False
            ]
                ++ t

        setModel route page =
            { model | page = page, route = route }
    in
        case maybeRoute of
            -- Patients\Profile
            Just Route.Profile ->
                setModel Route.Demographics (Demographics (Demographics.emptyModel model.patientId))
                    ! cmds [ Cmd.map DemographicsMsg (Demographics.init model.patientId) ]

            Just Route.Demographics ->
                setModel Route.Demographics (Demographics (Demographics.emptyModel model.patientId))
                    ! cmds [ Cmd.map DemographicsMsg (Demographics.init model.patientId) ]

            --People/ClinicalSummary
            Just Route.ClinicalSummaryRoot ->
                setModel Route.ClinicalSummaryRoot (ClinicalSummary ClinicalSummary.emptyModel)
                    ! cmds [ Cmd.map ClinicalSummaryMsg (ClinicalSummary.init model.patientId) ]

            Just Route.ClinicalSummary ->
                setModel Route.ClinicalSummary (ClinicalSummary ClinicalSummary.emptyModel)
                    ! cmds [ Cmd.map ClinicalSummaryMsg (ClinicalSummary.init model.patientId) ]

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

            --People/Records
            Just Route.RecordsRoot ->
                setModel (Route.Records Common.PrimaryCare) (Records (Records.emptyModel Common.PrimaryCare))
                    ! cmds [ Cmd.map RecordsMsg (Records.init Common.PrimaryCare model.patientId) ]

            Just (Route.Records t) ->
                setModel (Route.Records t) (Records (Records.emptyModel t))
                    ! cmds [ Cmd.map RecordsMsg (Records.init t model.patientId) ]

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
                -- setModel (Route.Error "no route provided, map me in Route.routeHash")
                --     (Error "no route provided, map me in Route.routeHash")
                --     ! []
                setModel Route.None NoPage
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

            ( Resize windowSize, _ ) ->
                let
                    rootDialog =
                        model.rootDialog
                in
                    { model | rootDialog = { rootDialog | windowSize = windowSize } } ! []

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
                --{ model | page = Error <| "Missing Page\\Message " ++ toString page ++ " !!!__-__!!! " ++ toString msg } ! []
                -- above line is useful for debugging, but when releasing, needs to be this
                -- because, what if you save, move away from the page, then receive confirmation previous thing saved, we don't care at this point
                model ! []


getDropDowns : Int -> Cmd Msg
getDropDowns patientId =
    decode AddEditDataSource
        |> required "facilityId" (Decode.maybe Decode.int)
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


main : Program Never Model Msg
main =
    Navigation.program (Route.fromLocation >> SetRoute)
        { init = init
        , view = view
        , update = update
        , subscriptions = subscriptions
        }

module Main exposing (main)

import Html exposing (Html, text, div)
import ClinicalSummary
import Records
import PastMedicalHistory
import Hospitilizations
import Allergies
import Immunizations
import LastKnownVitals
import Billing
import Common.Functions as Functions
import Common.Types exposing (AddEditDataSource)
import Common.Route as Route exposing (Route)
import Navigation exposing (Location)
import Http exposing (Error)
import Json.Decode exposing (maybe, int, list)
import Json.Decode.Pipeline exposing (required, decode)


type alias Model =
    { patientId : Int
    , page : Page
    , addEditDataSource : Maybe AddEditDataSource
    , location : Location
    }


type Page
    = None
    | Billing Billing.Model
    | ClinicalSummary ClinicalSummary.Model
    | Records Records.Model
    | PastMedicalHistory PastMedicalHistory.Model
    | Hospitilizations Hospitilizations.Model
    | Allergies Allergies.Model
    | Immunizations Immunizations.Model
    | LastKnownVitals LastKnownVitals.Model
    | Error String


init : Location -> ( Model, Cmd Msg )
init location =
    case Route.getPatientId location of
        Just patientId ->
            setRoute (Route.fromLocation location)
                { patientId = patientId
                , page = None
                , addEditDataSource = Nothing
                , location = location
                }

        Nothing ->
            { patientId = 0
            , page = Error "Cannot load page without patientId"
            , addEditDataSource = Nothing
            , location = location
            }
                ! [ Functions.setLoadingStatus False ]


subscriptions : Model -> Sub Msg
subscriptions model =
    Sub.batch
        [ pageSubscriptions model.page
        ]


view : Model -> Html Msg
view model =
    case model.page of
        None ->
            div [] [ text "no view here" ]

        Billing subModel ->
            Html.map BillingMsg (Billing.view subModel model.addEditDataSource)

        ClinicalSummary subModel ->
            Html.map ClinicalSummaryMsg (ClinicalSummary.view subModel model.patientId)

        Records subModel ->
            Html.map RecordsMsg (Records.view subModel model.addEditDataSource)

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

        Error str ->
            div [] [ text str ]


pageSubscriptions : Page -> Sub Msg
pageSubscriptions page =
    case page of
        None ->
            Sub.none

        Billing _ ->
            Sub.none

        ClinicalSummary _ ->
            Sub.map ClinicalSummaryMsg ClinicalSummary.subscriptions

        Records _ ->
            Sub.map RecordsMsg Records.subscriptions

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

        Error _ ->
            Sub.none



-- UPDATE --


type Msg
    = SetRoute (Maybe Route)
    | BillingMsg Billing.Msg
    | ClinicalSummaryMsg ClinicalSummary.Msg
    | RecordsMsg Records.Msg
    | AddEditDataSourceLoaded (Result Http.Error AddEditDataSource)
    | PastMedicalHistoryMsg PastMedicalHistory.Msg
    | HospitilizationsMsg Hospitilizations.Msg
    | AllergiesMsg Allergies.Msg
    | ImmunizationsMsg Immunizations.Msg
    | LastKnownVitalsMsg LastKnownVitals.Msg


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
    in
        case maybeRoute of
            Just Route.Billing ->
                { model | page = Billing Billing.emptyModel }
                    ! cmds [ Cmd.map BillingMsg (Billing.init model.patientId) ]

            Just Route.ClinicalSummary ->
                { model | page = ClinicalSummary ClinicalSummary.emptyModel }
                    ! cmds [ Cmd.map ClinicalSummaryMsg (ClinicalSummary.init model.patientId) ]

            Just Route.Hospitilizations ->
                { model | page = Hospitilizations Hospitilizations.emptyModel }
                    ! cmds [ Cmd.map HospitilizationsMsg (Hospitilizations.init model.patientId) ]

            Just Route.PastMedicalHistory ->
                { model | page = PastMedicalHistory PastMedicalHistory.emptyModel }
                    ! cmds [ Cmd.map PastMedicalHistoryMsg (PastMedicalHistory.init model.patientId) ]

            Just (Route.Records t) ->
                { model | page = Records (Records.emptyModel t model.addEditDataSource) }
                    ! cmds [ Cmd.map RecordsMsg (Records.init t model.patientId) ]

            Just Route.Allergies ->
                { model | page = Allergies Allergies.emptyModel }
                    ! cmds [ Cmd.map AllergiesMsg (Allergies.init model.patientId) ]

            Just Route.Immunizations ->
                { model | page = Immunizations Immunizations.emptyModel }
                    ! cmds [ Cmd.map ImmunizationsMsg (Immunizations.init model.patientId) ]

            Just Route.LastKnownVitals ->
                { model | page = LastKnownVitals LastKnownVitals.emptyModel }
                    ! cmds [ Cmd.map LastKnownVitalsMsg (LastKnownVitals.init model.patientId) ]

            Just Route.None ->
                model ! []

            Just (Route.Error str) ->
                { model | page = Error str } ! []

            Nothing ->
                { model | page = Error "no route provided" } ! []


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
        |> required "facilityId" (maybe int)
        |> required "patientId" int
        |> required "facilityDropdown" (list Functions.decodeDropdownItem)
        |> required "providersDropdown" (list Functions.decodeDropdownItem)
        |> required "recordTypeDropdown" (list Functions.decodeDropdownItem)
        |> required "userDropDown" (list Functions.decodeDropdownItem)
        |> required "taskDropDown" (list Functions.decodeDropdownItem)
        |> required "hospitilizationServiceTypeDropdown" (list Functions.decodeDropdownItem)
        |> required "hospitalizationDischargePhysicianDropdown" (list Functions.decodeDropdownItem)
        |> required "hospitilizations" (list Functions.decodeDropdownItem)
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

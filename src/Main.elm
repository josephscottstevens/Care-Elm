module Main exposing (main)

import Allergies
import Billing
import Browser
import ClinicalSummary
import Common.Dialog as Dialog
import Common.Functions as Functions
import Common.Route as Route exposing (Route)
import Common.Types as Common exposing (AddEditDataSource, WindowSize)
import Demographics
import Hospitilizations
import Html exposing (Html, div, text)
import Http exposing (Error)
import Immunizations
import Json.Decode as Decode
import Json.Decode.Pipeline exposing (required)
import LastKnownVitals
import PastMedicalHistory
import Records
import Task
import Url.Parser exposing (Url)



-- window.innerWidth
-- window.innerHeight


type alias Flags =
    { width : Int
    , height : Int
    , patientId : Int
    }


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


init : Flags -> ( Model, Cmd Msg )
init flags =
    --let
    -- maybePatientId =
    --     Route.getPatientId url
    -- patientId =
    --     Maybe.withDefault -1 maybePatientId
    -- ( model, cmds ) =
    --     -- setRoute (Route.fromLocation location)
    --     --TODO
    --     (
    --     , Cmd.none
    --     )
    --in
    ( { patientId = flags.patientId
      , rootDialog = { windowSize = WindowSize flags.width flags.height, top = 0, left = 0 }
      , page = NoPage
      , addEditDataSource = Nothing
      , route = Route.None
      }
    , Functions.setLoadingStatus False
    )


subscriptions : Model -> Sub Msg
subscriptions model =
    Sub.batch
        [-- pageSubscriptions model.page
         -- TODO
         -- , Window.resizes Resize
         -- , Browser.onWindow "onload" Decode.map
        ]


pageSubscriptions : Page -> Sub Msg
pageSubscriptions page =
    Sub.none



-- case page of
--     Demographics _ ->
--         Sub.map DemographicsMsg Demographics.subscriptions
--     ClinicalSummary _ ->
--         Sub.map ClinicalSummaryMsg ClinicalSummary.subscriptions
--     PastMedicalHistory _ ->
--         Sub.map PastMedicalHistoryMsg PastMedicalHistory.subscriptions
--     Hospitilizations _ ->
--         Sub.map HospitilizationsMsg Hospitilizations.subscriptions
--     Allergies _ ->
--         Sub.map AllergiesMsg Allergies.subscriptions
--     Immunizations _ ->
--         Sub.map ImmunizationsMsg Immunizations.subscriptions
--     LastKnownVitals _ ->
--         Sub.map LastKnownVitalsMsg LastKnownVitals.subscriptions
--     Records _ ->
--         Sub.map RecordsMsg Records.subscriptions
--     Billing _ ->
--         Sub.map BillingMsg Billing.subscriptions
--     -- Other
--     NoPage ->
--         Sub.none
--     Error _ ->
--         Sub.none


view : Model -> Html Msg
view model =
    text ""



-- case model.page of
--     Records subModel ->
--         Html.map RecordsMsg (Records.view subModel model.addEditDataSource)
--     Demographics subModel ->
--         Html.map DemographicsMsg (Demographics.view subModel)
--     Billing subModel ->
--         Html.map BillingMsg (Billing.view subModel model.patientId model.addEditDataSource model.rootDialog)
--     ClinicalSummary subModel ->
--         Html.map ClinicalSummaryMsg (ClinicalSummary.view subModel model.patientId)
--     PastMedicalHistory subModel ->
--         Html.map PastMedicalHistoryMsg (PastMedicalHistory.view subModel model.addEditDataSource)
--     Hospitilizations subModel ->
--         Html.map HospitilizationsMsg (Hospitilizations.view subModel model.addEditDataSource)
--     Allergies subModel ->
--         Html.map AllergiesMsg (Allergies.view subModel model.addEditDataSource)
--     Immunizations subModel ->
--         Html.map ImmunizationsMsg (Immunizations.view subModel model.addEditDataSource)
--     LastKnownVitals subModel ->
--         Html.map LastKnownVitalsMsg (LastKnownVitals.view subModel model.addEditDataSource)
--     -- Other
--     NoPage ->
--         div [] []
--     Error str ->
--         div [] [ Html.text str ]


type Msg
    = --SetRoute (Maybe Route)
      -- Resize WindowSize
      --   BillingMsg Billing.Msg
      -- | ClinicalSummaryMsg ClinicalSummary.Msg
      -- | PastMedicalHistoryMsg PastMedicalHistory.Msg
      -- | HospitilizationsMsg Hospitilizations.Msg
      -- | AllergiesMsg Allergies.Msg
      -- | ImmunizationsMsg Immunizations.Msg
      LastKnownVitalsMsg LastKnownVitals.Msg
      -- | RecordsMsg Records.Msg
    | DemographicsMsg Demographics.Msg
    | AddEditDataSourceLoaded (Result Http.Error AddEditDataSource)



-- setRoute : Maybe Route -> Model -> ( Model, Cmd Msg )
-- setRoute maybeRoute model =
--     let
--         getDropdownsCmd =
--             case model.addEditDataSource of
--                 Just _ ->
--                     Cmd.none
--                 Nothing ->
--                     getDropDowns model.patientId
--         cmds t =
--             [ getDropdownsCmd
--             , Functions.setLoadingStatus False
--             ]
--                 ++ t
--         setModel route page =
--             { model | page = page, route = route }
--     in
--     case maybeRoute of
--         -- Patients\Profile
--         Just Route.Profile ->
--             ( setModel Route.Demographics (Demographics (Demographics.emptyModel model.patientId))
--             , Cmd.batch (cmds [ Cmd.map DemographicsMsg (Demographics.init model.patientId) ])
--             )
--         Just Route.Demographics ->
--             ( setModel Route.Demographics (Demographics (Demographics.emptyModel model.patientId))
--             , Cmd.batch (cmds [ Cmd.map DemographicsMsg (Demographics.init model.patientId) ])
--             )
--         --People/ClinicalSummary
--         Just Route.ClinicalSummaryRoot ->
--             ( setModel Route.ClinicalSummaryRoot (ClinicalSummary ClinicalSummary.emptyModel)
--             , Cmd.batch (cmds [ Cmd.map ClinicalSummaryMsg (ClinicalSummary.init model.patientId) ])
--             )
--         Just Route.ClinicalSummary ->
--             ( setModel Route.ClinicalSummary (ClinicalSummary ClinicalSummary.emptyModel)
--             , Cmd.batch (cmds [ Cmd.map ClinicalSummaryMsg (ClinicalSummary.init model.patientId) ])
--             )
--         Just Route.PastMedicalHistory ->
--             ( setModel Route.PastMedicalHistory (PastMedicalHistory PastMedicalHistory.emptyModel)
--             , Cmd.batch (cmds [ Cmd.map PastMedicalHistoryMsg (PastMedicalHistory.init model.patientId) ])
--             )
--         Just Route.Hospitilizations ->
--             ( setModel Route.Hospitilizations (Hospitilizations Hospitilizations.emptyModel)
--             , Cmd.batch (cmds [ Cmd.map HospitilizationsMsg (Hospitilizations.init model.patientId) ])
--             )
--         Just Route.Immunizations ->
--             ( setModel Route.Immunizations (Immunizations Immunizations.emptyModel)
--             , Cmd.batch (cmds [ Cmd.map ImmunizationsMsg (Immunizations.init model.patientId) ])
--             )
--         Just Route.Allergies ->
--             ( setModel Route.Allergies (Allergies Allergies.emptyModel)
--             , Cmd.batch (cmds [ Cmd.map AllergiesMsg (Allergies.init model.patientId) ])
--             )
-- Just Route.LastKnownVitals ->
--     ( setModel Route.LastKnownVitals (LastKnownVitals LastKnownVitals.emptyModel)
--     , Cmd.batch (cmds [ Cmd.map LastKnownVitalsMsg (LastKnownVitals.init model.patientId) ])
--     )
-- --People/Records
-- Just Route.RecordsRoot ->
--     ( setModel (Route.Records Common.PrimaryCare) (Records (Records.emptyModel Common.PrimaryCare))
--     , Cmd.batch (cmds [ Cmd.map RecordsMsg (Records.init Common.PrimaryCare model.patientId) ])
--     )
-- Just (Route.Records t) ->
--     ( setModel (Route.Records t) (Records (Records.emptyModel t))
--     , Cmd.batch (cmds [ Cmd.map RecordsMsg (Records.init t model.patientId) ])
--     )
--         --Other
--         Just Route.Billing ->
--             ( setModel Route.Billing (Billing Billing.emptyModel)
--             , Cmd.batch (cmds [ Cmd.map BillingMsg (Billing.init model.patientId) ])
--             )
--         Just Route.None ->
--             ( setModel Route.None NoPage
--             , Cmd.none
--             )
--         Just (Route.Error str) ->
--             ( setModel (Route.Error str) (Error str)
--             , Cmd.none
--             )
--         Nothing ->
--             -- setModel (Route.Error "no route provided, map me in Route.routeHash")
--             --     (Error "no route provided, map me in Route.routeHash")
--             --     ! []
--             ( setModel Route.None NoPage
--             , Cmd.none
--             )


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
            ( { model | page = toModel newModel }
            , Cmd.map toMsg newCmd
            )
    in
    case ( msg, page ) of
        -- ( SetRoute route, _ ) ->
        --     setRoute route model
        -- ( Resize windowSize, _ ) ->
        --     let
        --         rootDialog =
        --             model.rootDialog
        --     in
        --     ( { model | rootDialog = { rootDialog | windowSize = windowSize } }
        --     , Cmd.none
        --     )
        ( AddEditDataSourceLoaded response, _ ) ->
            case response of
                Ok t ->
                    ( { model | addEditDataSource = Just t }
                    , Cmd.none
                    )

                Err t ->
                    ( { model | page = Error (Functions.getError t) }
                    , Cmd.none
                    )

        ( DemographicsMsg subMsg, Demographics subModel ) ->
            toPage Demographics DemographicsMsg Demographics.update subMsg subModel

        -- ( PastMedicalHistoryMsg subMsg, PastMedicalHistory subModel ) ->
        --     toPage PastMedicalHistory PastMedicalHistoryMsg PastMedicalHistory.update subMsg subModel
        -- ( BillingMsg subMsg, Billing subModel ) ->
        --     toPage Billing BillingMsg Billing.update subMsg subModel
        -- ( HospitilizationsMsg subMsg, Hospitilizations subModel ) ->
        --     toPage Hospitilizations HospitilizationsMsg Hospitilizations.update subMsg subModel
        -- ( ClinicalSummaryMsg subMsg, ClinicalSummary subModel ) ->
        --     toPage ClinicalSummary ClinicalSummaryMsg ClinicalSummary.update subMsg subModel
        -- ( RecordsMsg subMsg, Records subModel ) ->
        --     toPage Records RecordsMsg Records.update subMsg subModel
        -- ( AllergiesMsg subMsg, Allergies subModel ) ->
        --     toPage Allergies AllergiesMsg Allergies.update subMsg subModel
        -- ( ImmunizationsMsg subMsg, Immunizations subModel ) ->
        --     toPage Immunizations ImmunizationsMsg Immunizations.update subMsg subModel
        ( LastKnownVitalsMsg subMsg, LastKnownVitals subModel ) ->
            toPage LastKnownVitals LastKnownVitalsMsg LastKnownVitals.update subMsg subModel

        _ ->
            --{ model | page = Error <| "Missing Page\\Message " ++ toString page ++ " !!!__-__!!! " ++ toString msg } ! []
            -- above line is useful for debugging, but when releasing, needs to be this
            -- because, what if you save, move away from the page, then receive confirmation previous thing saved, we don't care at this point
            ( model
            , Cmd.none
            )


getDropDowns : Int -> Cmd Msg
getDropDowns patientId =
    Decode.succeed AddEditDataSource
        |> required "facilityId" (Decode.maybe Decode.int)
        |> required "facilityDropdown" (Decode.list Functions.decodeDropdownItem)
        |> required "providersDropdown" (Decode.list Functions.decodeDropdownItem)
        |> required "recordTypeDropdown" (Decode.list Functions.decodeDropdownItem)
        |> required "userDropDown" (Decode.list Functions.decodeDropdownItem)
        |> required "taskDropDown" (Decode.list Functions.decodeDropdownItem)
        |> required "hospitilizationServiceTypeDropdown" (Decode.list Functions.decodeDropdownItem)
        |> required "hospitalizationDischargePhysicianDropdown" (Decode.list Functions.decodeDropdownItem)
        |> required "hospitilizations" (Decode.list Functions.decodeDropdownItem)
        |> Http.get ("/People/PatientRecordsDropdowns?patientId=" ++ String.fromInt patientId)
        |> Http.send AddEditDataSourceLoaded


main : Program Flags Model Msg
main =
    --TODO
    -- (Route.fromLocation >> SetRoute)
    Browser.embed
        { init = init
        , view = view
        , update = update
        , subscriptions = subscriptions
        }

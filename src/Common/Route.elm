module Common.Route
    exposing
        ( Route
            ( None
            , Billing
            , ClinicalSummary
            , Records
            , Hospitilizations
            , PastMedicalHistory
            , Error
            , Allergies
            , Immunizations
            , LastKnownVitals
            )
        , getPatientId
        , fromLocation
        , href
        , modifyUrl
        , back
        , getSideNav
        , RouteDesc
        )

import Navigation
import Common.Types as Common
import Html exposing (Attribute)
import Html.Attributes as Attr
import UrlParser as Url exposing ((</>), (<?>), Parser, oneOf, parseHash, s, parsePath, intParam)


type Tree a
    = Child a
    | Parent a (List (Tree a))


type alias RouteDesc =
    { depth : Float
    , url : String
    , navText : String
    }


nodes : Tree Route
nodes =
    Parent Home
        [ Parent Profile
            [ Child Demographics
            , Child Contacts
            , Child SocialHistory
            , Child Employment
            , Child Insurance
            ]
        , Parent Services
            [ Child CCM
            , Child TCM
            ]
        , Parent Providers
            []
        , Parent ClinicalSummary
            [ Child Tasks
            , Child Appointments
            ]
        ]


flattenWithDepth : Float -> Tree Route -> List ( Route, Float )
flattenWithDepth depth tree =
    case tree of
        Child t ->
            [ ( t, depth ) ]

        Parent t y ->
            ( t, depth ) :: List.concatMap (flattenWithDepth (depth + 1.0)) y


routeToSideNav : ( Route, Float ) -> RouteDesc
routeToSideNav ( route, depth ) =
    { depth = depth
    , url = routeToString route
    , navText = toString route
    }


getSideNav : List RouteDesc
getSideNav =
    flattenWithDepth -1.0 nodes
        |> List.map routeToSideNav


type Route
    = None
    | Profile
    | Demographics
    | Billing
    | ClinicalSummary
    | Records Common.RecordType
    | Hospitilizations
    | PastMedicalHistory
    | Allergies
    | Immunizations
    | LastKnownVitals
    | Error String
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


recordTypeToString : Common.RecordType -> String
recordTypeToString recordType =
    case recordType of
        Common.PrimaryCare ->
            "_primarycarerecords"

        Common.Specialty ->
            "_specialtyrecords"

        Common.Labs ->
            "_labrecords"

        Common.Radiology ->
            "_radiologyrecords"

        Common.Hospitalizations ->
            "_hospitalizationrecords"

        Common.Legal ->
            "_legalrecords"

        Common.CallRecordings ->
            "_callrecordingrecords"

        Common.PreviousHistories ->
            "_previoushistoryrecords"

        Common.Enrollment ->
            "_enrollmentrecords"

        Common.Misc ->
            "_miscrecords"


routeToString : Route -> String
routeToString route =
    case route of
        None ->
            "#"

        Billing ->
            "#/people/_insurance"

        ClinicalSummary ->
            "#/people/_clinicalsummary"

        Records recordType ->
            "#/people/" ++ recordTypeToString recordType

        PastMedicalHistory ->
            "#/people/_pastmedicalhistory"

        Hospitilizations ->
            "#/people/_hospitalizations"

        Allergies ->
            "#/people/_allergies"

        Immunizations ->
            "#/people/_immunizations"

        LastKnownVitals ->
            "#/people/_vitals"

        Error t ->
            "#/Error" ++ t

        _ ->
            ""


route : Parser (Route -> a) a
route =
    oneOf
        [ -- Clinical Summary
          Url.map ClinicalSummary (s "people" </> s "_clinicalsummary")
        , Url.map PastMedicalHistory (s "people" </> s "_pastmedicalhistory")
        , Url.map Hospitilizations (s "people" </> s "_hospitalizations")
        , Url.map Allergies (s "people" </> s "_allergies")
        , Url.map Immunizations (s "people" </> s "_immunizations")
        , Url.map LastKnownVitals (s "people" </> s "_vitals")
        , Url.map Billing (s "people" </> s "_insurance")

        -- Records Grid
        , Url.map (Records Common.PrimaryCare) (s "people" </> s "_primarycarerecords")
        , Url.map (Records Common.Specialty) (s "people" </> s "_specialtyrecords")
        , Url.map (Records Common.Labs) (s "people" </> s "_labrecords")
        , Url.map (Records Common.Radiology) (s "people" </> s "_radiologyrecords")
        , Url.map (Records Common.Hospitalizations) (s "people" </> s "_hospitalizationrecords")
        , Url.map (Records Common.Legal) (s "people" </> s "_legalrecords")
        , Url.map (Records Common.CallRecordings) (s "people" </> s "_callrecordingrecords")
        , Url.map (Records Common.PreviousHistories) (s "people" </> s "_previoushistoryrecords")
        , Url.map (Records Common.Enrollment) (s "people" </> s "_enrollmentrecords")
        , Url.map (Records Common.Misc) (s "people" </> s "_miscrecords")
        ]


getPatientId : Navigation.Location -> Maybe Int
getPatientId location =
    location
        |> parsePath (s "people" <?> intParam "patientId")
        |> Maybe.andThen identity


href : Route -> Attribute msg
href route =
    Attr.href (routeToString route)


modifyUrl : Route -> Cmd msg
modifyUrl =
    routeToString >> Navigation.newUrl


back : Cmd msg
back =
    Navigation.back 1


fromLocation : Navigation.Location -> Maybe Route
fromLocation location =
    if String.isEmpty location.hash then
        Just None
    else
        parseHash route location

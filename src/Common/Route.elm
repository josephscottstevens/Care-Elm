module Common.Route
    exposing
        ( Route(..)
        , getPatientId
        , fromLocation
        , href
        , modifyUrl
        , back
        , routeUrl
        , routeDescription
        , routeId
        )

import Navigation
import Common.Types as Common
import Html exposing (Attribute)
import Html.Attributes as Attr
import UrlParser as Url exposing ((</>), (<?>), Parser, oneOf, parseHash, s, parsePath, intParam)


type Route
    = None
    | Profile
    | Demographics
      -- | Billing
    | ClinicalSummaryRoot
    | ClinicalSummary
    | RecordsRoot
    | Records Common.RecordType
    | Hospitilizations
    | PastMedicalHistory
    | Allergies
    | Immunizations
    | LastKnownVitals
    | Error String


routeByHash : Parser (Route -> a) a
routeByHash =
    oneOf
        [ -- Clinical Summary
          Url.map Demographics (s "people")
        , Url.map ClinicalSummary (s "people" </> s "_clinicalsummary")
        , Url.map PastMedicalHistory (s "people" </> s "_pastmedicalhistory")
        , Url.map Hospitilizations (s "people" </> s "_hospitalizations")
        , Url.map Allergies (s "people" </> s "_allergies")
        , Url.map Immunizations (s "people" </> s "_immunizations")
        , Url.map LastKnownVitals (s "people" </> s "_vitals")

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
        , Url.map Demographics (s "people" </> s "_demographics")

        -- Other
        -- , Url.map Billing (s "people" </> s "_insurance")
        ]


routeByUrl : Parser (Route -> a) a
routeByUrl =
    oneOf
        [ Url.map Demographics (s "people")
        ]


routeUrl : Route -> String
routeUrl route =
    case route of
        -- Patients\Profile
        Profile ->
            "#/people/_demographics"

        Demographics ->
            "#/people/_demographics"

        --People/ClinicalSummary
        ClinicalSummaryRoot ->
            "#/people/_clinicalsummary"

        ClinicalSummary ->
            "#/people/_clinicalsummary"

        PastMedicalHistory ->
            "#/people/_pastmedicalhistory"

        Hospitilizations ->
            "#/people/_hospitalizations"

        Immunizations ->
            "#/people/_immunizations"

        Allergies ->
            "#/people/_allergies"

        LastKnownVitals ->
            "#/people/_vitals"

        --People/Records
        RecordsRoot ->
            "#/people/_primarycarerecords"

        Records Common.PrimaryCare ->
            "#/people/_primarycarerecords"

        Records Common.Specialty ->
            "#/people/_specialtyrecords"

        Records Common.Labs ->
            "#/people/_labrecords"

        Records Common.Radiology ->
            "#/people/_radiologyrecords"

        Records Common.Hospitalizations ->
            "#/people/_hospitalizationrecords"

        Records Common.Legal ->
            "#/people/_legalrecords"

        Records Common.CallRecordings ->
            "#/people/_callrecordingrecords"

        Records Common.PreviousHistories ->
            "#/people/_previoushistoryrecords"

        Records Common.Enrollment ->
            "#/people/_enrollmentrecords"

        Records Common.Misc ->
            "#/people/_miscrecords"

        --People/Notes
        -- Other
        None ->
            ""

        Error t ->
            "#/Error" ++ t



-- Billing ->
--     "#/people/_insurance"


routeId : Route -> Int
routeId route =
    case route of
        -- Patients\Profile
        Profile ->
            15

        Demographics ->
            16

        -- People/Services
        -- People/Providers
        --People/ClinicalSummary
        ClinicalSummaryRoot ->
            23

        ClinicalSummary ->
            25

        PastMedicalHistory ->
            28

        Hospitilizations ->
            29

        Immunizations ->
            30

        Allergies ->
            31

        LastKnownVitals ->
            32

        --People/Records
        RecordsRoot ->
            35

        Records Common.PrimaryCare ->
            55

        Records Common.Specialty ->
            56

        Records Common.Labs ->
            57

        Records Common.Radiology ->
            58

        Records Common.Hospitalizations ->
            59

        Records Common.Legal ->
            60

        Records Common.CallRecordings ->
            62

        Records Common.PreviousHistories ->
            63

        Records Common.Enrollment ->
            64

        Records Common.Misc ->
            65

        --Other
        --TODO, pretty clunky here
        None ->
            0

        Error _ ->
            0



-- Billing ->
--     0


getPatientId : Navigation.Location -> Maybe Int
getPatientId location =
    location
        |> parsePath (s "people" <?> intParam "patientId")
        |> Maybe.andThen identity


href : Route -> Attribute msg
href route =
    Attr.href (routeUrl route)


modifyUrl : Route -> Cmd msg
modifyUrl =
    routeUrl >> Navigation.newUrl


back : Cmd msg
back =
    Navigation.back 1


fromLocation : Navigation.Location -> Maybe Route
fromLocation location =
    if String.isEmpty location.hash then
        parsePath routeByUrl location
    else
        parseHash routeByHash location


routeDescription : Route -> String
routeDescription route =
    case route of
        -- Patients\Profile
        Profile ->
            "Profile"

        Demographics ->
            "Demographic Information"

        --People/ClinicalSummary
        ClinicalSummaryRoot ->
            "Clinical Summary"

        ClinicalSummary ->
            "Clinical Summary"

        PastMedicalHistory ->
            "Past Medical History"

        Hospitilizations ->
            "Hospitilizations"

        Immunizations ->
            "Immunizations & Preventative Screenings"

        Allergies ->
            "Allergies"

        LastKnownVitals ->
            "Last Known Vitals"

        --People/Records
        RecordsRoot ->
            "Records"

        Records Common.PrimaryCare ->
            "Primary Care"

        Records Common.Specialty ->
            "Specialty"

        Records Common.Labs ->
            "Labs"

        Records Common.Radiology ->
            "Radiology / Gen. Study"

        Records Common.Hospitalizations ->
            "Hospitalizations"

        Records Common.Legal ->
            "Legal"

        Records Common.CallRecordings ->
            "Call Recordings"

        Records Common.PreviousHistories ->
            "Previous Histories"

        Records Common.Enrollment ->
            "Enrollment"

        Records Common.Misc ->
            "Miscellaneous"

        --Other
        None ->
            ""

        Error t ->
            t



-- Billing ->
--     "Billing"

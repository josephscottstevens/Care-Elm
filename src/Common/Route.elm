module Common.Route
    exposing
        ( Route(..)
        , getPatientId
        , fromLocation
        , href
        , modifyUrl
        , back
        , getSideNav
        , RouteDesc
        , Breadcrumb
        , getBreadcrumbsFromRoute
        , getParentFromRoute
        , routeUrl
        , routeDescription
        , routeId
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
    , route : Route
    , id : Int
    }


type Route
    = None
    | Profile
    | Demographics
    | Billing
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
        , Parent ClinicalSummaryRoot
            [ Child ClinicalSummary
            , Child ProblemList
            , Child Medications
            , Child PastMedicalHistory
            , Child Hospitilizations
            , Child Immunizations
            , Child Allergies
            , Child LastKnownVitals
            ]
        , Parent RecordsRoot
            [ Child (Records Common.PrimaryCare)
            , Child (Records Common.Specialty)
            , Child (Records Common.Labs)
            , Child (Records Common.Radiology)
            , Child (Records Common.Hospitalizations)
            , Child (Records Common.Legal)
            , Child (Records Common.CallRecordings)
            , Child (Records Common.PreviousHistories)
            , Child (Records Common.Enrollment)
            , Child (Records Common.Misc)
            ]
        , Parent Notes
            []
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
    , url = routeUrl route
    , navText = routeDescription route
    , route = route
    , id = routeId route
    }


getSideNav : List RouteDesc
getSideNav =
    flattenWithDepth -1.0 nodes
        |> List.map routeToSideNav


type alias Breadcrumb =
    { depth : Float
    , route : Route
    , idx : Int
    }


items : List Breadcrumb
items =
    nodes
        |> flattenWithDepth 0
        |> List.reverse
        |> List.indexedMap (\idx ( t, depth ) -> Breadcrumb depth t idx)


getBreadcrumb : Route -> Maybe Breadcrumb
getBreadcrumb route =
    items
        |> List.map
            (\t ->
                if t.route == route then
                    Just t
                else
                    Nothing
            )
        |> List.filterMap identity
        |> List.head


getParent : Breadcrumb -> Maybe Breadcrumb
getParent breadCrumb =
    items
        |> List.drop (breadCrumb.idx + 1)
        |> List.filter (\t -> t.depth < breadCrumb.depth)
        |> List.head


getBreadcrumbs : Maybe Breadcrumb -> List Breadcrumb
getBreadcrumbs maybeBreadCrumb =
    case maybeBreadCrumb of
        Just breadCrumb ->
            breadCrumb :: (getBreadcrumbs (getParent breadCrumb))

        Nothing ->
            []


getParentFromRoute : Route -> Maybe Route
getParentFromRoute route =
    route
        |> getBreadcrumb
        |> Maybe.andThen getParent
        |> Maybe.map .route


getBreadcrumbsFromRoute : Route -> List Breadcrumb
getBreadcrumbsFromRoute route =
    getBreadcrumbs (getBreadcrumb route)
        |> List.reverse


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

        --Non Elm pages
        , Url.map Contacts (s "people" </> s "_contacts")
        , Url.map SocialHistory (s "people" </> s "_socialhistory")
        , Url.map Employment (s "people" </> s "_employment")
        , Url.map Insurance (s "people" </> s "_insurance")
        , Url.map CCM (s "people" </> s "_ccm")
        , Url.map TCM (s "people" </> s "_tcm")
        , Url.map Providers (s "people" </> s "_careteam")
        , Url.map Tasks (s "people" </> s "_tasks")
        , Url.map Appointments (s "people" </> s "_appointments")
        , Url.map ProblemList (s "people" </> s "_problemlist")
        , Url.map Medications (s "people" </> s "_medications")
        , Url.map Notes (s "people" </> s "_notes")

        -- Other
        , Url.map Billing (s "people" </> s "_insurance")
        ]


routeByUrl : Parser (Route -> a) a
routeByUrl =
    oneOf
        [ Url.map Home (s "")
        , Url.map Demographics (s "people")
        ]


routeUrl : Route -> String
routeUrl route =
    case route of
        Home ->
            "/"

        -- Patients\Profile
        Profile ->
            "#/people/_demographics"

        Demographics ->
            "#/people/_demographics"

        Contacts ->
            "#/people/_contacts"

        SocialHistory ->
            "#/people/_socialhistory"

        Employment ->
            "#/people/_employment"

        Insurance ->
            "#/people/_insurance"

        -- People/Services
        Services ->
            "#/people/_ccm"

        CCM ->
            "#/people/_ccm"

        TCM ->
            "#/people/_tcm"

        -- People/Providers
        Providers ->
            "#/people/_careteam"

        --People/ClinicalSummary
        ClinicalSummaryRoot ->
            "#/people/_clinicalsummary"

        ClinicalSummary ->
            "#/people/_clinicalsummary"

        ProblemList ->
            "#/people/_problemlist"

        Medications ->
            "#/people/_medications"

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

        --People/Tasks
        Tasks ->
            "#/people/_tasks"

        --People/Appointments
        Appointments ->
            "#/people/_appointments"

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
        Notes ->
            "#/people/_notes"

        -- Other
        None ->
            ""

        Error t ->
            "#/Error" ++ t

        Billing ->
            "#/people/_insurance"


routeId : Route -> Int
routeId route =
    case route of
        Home ->
            3

        -- Patients\Profile
        Profile ->
            15

        Demographics ->
            16

        Contacts ->
            17

        SocialHistory ->
            18

        Employment ->
            68

        Insurance ->
            69

        -- People/Services
        Services ->
            19

        CCM ->
            20

        TCM ->
            21

        -- People/Providers
        Providers ->
            22

        --People/ClinicalSummary
        ClinicalSummaryRoot ->
            23

        ClinicalSummary ->
            25

        ProblemList ->
            26

        Medications ->
            27

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

        --People/Tasks
        Tasks ->
            33

        --People/Appointments
        Appointments ->
            34

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

        --People/Notes
        Notes ->
            36

        --Other
        --TODO, pretty clunky here
        None ->
            0

        Error t ->
            0

        Billing ->
            0


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
        Home ->
            "Home"

        -- Patients\Profile
        Profile ->
            "Profile"

        Demographics ->
            "Demographic Information"

        Contacts ->
            "Contacts"

        SocialHistory ->
            "Social History"

        Employment ->
            "Employment Information"

        Insurance ->
            "Insurance Information"

        -- People/Services
        Services ->
            "Services"

        CCM ->
            "CCM"

        TCM ->
            "TCM"

        -- People/Providers
        Providers ->
            "Providers"

        --People/ClinicalSummary
        ClinicalSummaryRoot ->
            "Clinical Summary"

        ClinicalSummary ->
            "Clinical Summary"

        ProblemList ->
            "Problem List"

        Medications ->
            "Medications"

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

        --People/Tasks
        Tasks ->
            "Tasks"

        --People/Appointments
        Appointments ->
            "Appointments"

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

        --People/Notes
        Notes ->
            "Notes"

        --Other
        None ->
            ""

        Error t ->
            t

        Billing ->
            "Billing"

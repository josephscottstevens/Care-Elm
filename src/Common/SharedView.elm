module Common.SharedView exposing (view)

import Html exposing (Html)
import Element exposing (column, el, image, row, text, link, empty, below)
import Element.Attributes exposing (center, fill, fillPortion, width, height, class, padding, spacing, px, verticalCenter, spacingXY, paddingLeft, paddingRight, paddingBottom, paddingTop, hidden, alignRight, clipX, id)
import Color
import Style exposing (style, styleSheet)
import Style.Border as Border
import Style.Color as Color
import Style.Font as Font
import Common.Route as Route
import Common.Types as Common


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


view : Html msg -> Route.Route -> Maybe Common.PersonHeaderDetails -> Html msg
view innerView activeRoute activePerson =
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
                        (viewPatientHeader activePerson
                            ++ [ row None
                                    [ paddingLeft 10, paddingTop 10, paddingRight 10 ]
                                    [ el None [ class "body-content" ] <| Element.html innerView
                                    ]
                               ]
                        )
                    ]
                ]


viewPatientHeader : Maybe Common.PersonHeaderDetails -> List (Element.Element MyStyles variation msg)
viewPatientHeader maybeActivePerson =
    case maybeActivePerson of
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
                        [ el None [] (text "hours")
                            |> below
                                [ column HeaderPatient
                                    [ spacing 20 ]
                                    [ el None [] (text "a")
                                    , el None [] (text "b")
                                    , el None [] (text "c")
                                    ]
                                ]
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

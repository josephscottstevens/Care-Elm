module Common.SharedView exposing (view)

import Html exposing (Html)
import Element exposing (column, el, image, row, text, link, empty)
import Element.Attributes exposing (center, fill, fillPortion, width, height, class, padding, spacing, px, verticalCenter, spacingXY, paddingLeft, paddingRight, paddingBottom, paddingTop)
import Color
import Style exposing (style, styleSheet)
import Style.Border as Border
import Style.Color as Color
import Style.Font as Font
import Common.Route as Route
import Navigation


type MyStyles
    = Root
    | HeaderNav
    | HeaderNavActive
    | HeaderBreadQuick
    | SideNav
    | SideNavActive
    | Body
    | None


navBlue : Color.Color
navBlue =
    Color.rgb 51 122 183


navBlueActive : Color.Color
navBlueActive =
    Color.rgb 187 217 238


stylesheet : Style.StyleSheet MyStyles variation
stylesheet =
    styleSheet
        [ style Root
            [ --Font.typeface [ Font.importUrl { url = "https://fonts.googleapis.com/css", name = "eb garamond" } ]
              Font.size 20
            ]
        , style HeaderNav
            [ Color.text navBlue
            , Color.background Color.white
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
            ]
        , style SideNavActive
            [ Color.text navBlue
            , Color.background navBlueActive
            ]
        , style Body
            [ Color.text Color.black
            ]
        , style None []
        ]


view : Html msg -> Navigation.Location -> Html msg
view innerView location =
    let
        activeRoute =
            case Route.fromLocation location of
                Just route ->
                    toString route

                Nothing ->
                    ""

        fr amount =
            width <| fillPortion amount

        toTopUrl navUrl navText =
            let
                activeClass =
                    -- TODO, this needs to bubble up to the parent
                    if navText == "Home" then
                        HeaderNavActive
                    else
                        HeaderNav
            in
                el activeClass [] <| link navUrl <| el None [] (text navText)

        toSideUrl { depth, url, navText } =
            let
                activeClass =
                    if navText == activeRoute then
                        SideNavActive
                    else
                        SideNav
            in
                link url <|
                    el activeClass
                        [ height <| px 40
                        , verticalCenter
                        , paddingLeft (10 + (depth * 15))
                        , paddingTop 10.0
                        , paddingBottom 10.0
                        , paddingRight 0.0
                        , width fill
                        ]
                        (text navText)

        sideNav =
            Route.getSideNav
                |> List.filter (\t -> t.depth /= -1.0)
                |> List.map toSideUrl
    in
        Element.layout stylesheet <|
            column None
                []
                [ row None
                    [ width fill, height <| px 59 ]
                    [ column None
                        [ fr 5 ]
                        [ row None
                            []
                            [ image None [ class "pointer" ] { src = "/Images/Logos/Logo-ncn.png", caption = "" } ]
                        ]
                    , column None
                        [ fr 6 ]
                        [ row None
                            [ spacing 14 ]
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
                            ]
                        ]
                    ]
                , row HeaderBreadQuick
                    []
                    [ column None
                        []
                        [ row None
                            []
                            [ text "Home..Profile..Dem"
                            ]
                        ]
                    , column None
                        []
                        [ row None
                            []
                            [ text "Home..Profile..Dem"
                            ]
                        ]
                    ]
                , row None
                    []
                    [ column None
                        [ fr 2 ]
                        sideNav
                    , column None
                        [ fr 10 ]
                        [ Element.html innerView ]
                    ]
                ]

port module Common.GridDropdown exposing (Msg, update, view)

import Html exposing (Html, Attribute, div, span, text, li, ul, input, button)
import Html.Attributes exposing (style, value, class, readonly, type_, target)
import Html.Events as Events
import Json.Decode
import Common.Types exposing (DropdownItem, MenuMessage)
import Common.Functions as Functions


port sendMenuMessageNew : MenuMessage -> Cmd msg


type Msg
    = GridSetOpenState
    | GridOnBlur
    | NoOp
    | MenuItemSelected MenuMessage


update : List { a | isOpen : Bool } -> { a | isOpen : Bool } -> Msg -> ( List { a | isOpen : Bool }, Cmd msg )
update dropRows row msg =
    let
        newDropRows newOpenStatus =
            dropRows
                |> List.map
                    (\t ->
                        if t == row then
                            newOpenStatus
                        else
                            t
                    )
    in
        case msg of
            GridSetOpenState ->
                newDropRows { row | isOpen = not row.isOpen } ! []

            GridOnBlur ->
                newDropRows { row | isOpen = False } ! []

            MenuItemSelected menuItem ->
                newDropRows { row | isOpen = False } ! [ sendMenuMessageNew menuItem ]

            NoOp ->
                dropRows ! []


view : Bool -> List ( String, String, MenuMessage ) -> Html Msg
view isOpen dropDownItems =
    let
        dropMenu =
            case isOpen of
                True ->
                    [ ul
                        [ class "e-menu e-js e-widget e-box e-separator"
                        ]
                        (List.map dropDownMenuItem dropDownItems)
                    ]

                False ->
                    []

        btnClass =
            class "btn btn-sm btn-default fa fa-angle-down btn-context-menu editDropDown"

        btnStyle =
            style [ ( "position", "relative" ) ]
    in
        div
            [ style [ ( "text-align", "right" ) ]
            , onClick GridSetOpenState
            ]
            [ button
                [ type_ "button"
                , btnClass
                , btnStyle
                , if isOpen then
                    Events.onBlur GridOnBlur
                  else
                    Events.onBlur NoOp
                ]
                [ div [ dropDownMenuStyle ]
                    dropMenu
                ]
            ]


dropDownMenuStyle : Html.Attribute msg
dropDownMenuStyle =
    style
        [ ( "z-index", "5000" )
        , ( "position", "absolute" )
        , ( "display", "block" )
        , ( "left", "-173px" )
        , ( "width", "178.74px" )
        ]


dropDownMenuItem : ( String, String, MenuMessage ) -> Html Msg
dropDownMenuItem ( iconClass, displayText, menuMessage ) =
    li [ class "e-content e-list" ]
        [ Html.a [ class "e-menulink", Events.onClick (MenuItemSelected menuMessage), target "_blank" ]
            [ text displayText
            , span [ class ("e-gridcontext e-icon " ++ iconClass) ] []
            ]
        ]


getId : String -> DropdownItem -> String
getId id item =
    id ++ "-" ++ Functions.defaultIntToString item.id


onClick : msg -> Attribute msg
onClick message =
    Events.onWithOptions "click"
        { stopPropagation = True, preventDefault = False }
        (Json.Decode.succeed message)



-- styles for list container


dropdownList : List ( String, String )
dropdownList =
    [ ( "position", "absolute" )
    , ( "top", "32px" )
    , ( "border-radius", "4px" )
    , ( "box-shadow", "0 1px 2px rgba(0,0,0,.24)" )
    , ( "padding", "0" )
    , ( "margin", "0" )

    -- , ( "width", "150px" )
    , ( "background-color", "white" )
    , ( "max-height", "152px" )
    , ( "overflow-x", "hidden" )
    , ( "overflow-y", "scroll" )
    , ( "z-index", "100" )
    ]


maybeFallback : Maybe a -> Maybe a -> Maybe a
maybeFallback replacement original =
    case original of
        Just _ ->
            original

        Nothing ->
            replacement



-- keyDecoder : Dropdown -> Int -> Json.Decode.Decoder Key
-- keyDecoder dropdown keyCode =
--     let
--         -- This is necessary to ensure that the key is not consumed and can propagate to the parent
--         pass =
--             Json.Decode.fail ""
--         key =
--             Json.Decode.succeed
--     in
--         case keyCode of
--             13 ->
--                 key Enter
--             27 ->
--                 -- Consume Esc only if the Menu is open
--                 if dropdown.isOpen then
--                     pass
--                 else
--                     key Esc
--             -- 32 ->
--             --     key Space
--             33 ->
--                 key PageUp
--             34 ->
--                 key PageDown
--             35 ->
--                 key End
--             36 ->
--                 key Home
--             38 ->
--                 key ArrowUp
--             40 ->
--                 key ArrowDown
--             _ ->
--                 let
--                     char =
--                         Char.fromCode keyCode
--                     -- TODO should the user be able to search non-alphanum chars?
--                     -- TODO add support for non-ascii alphas
--                     isAlpha char =
--                         (char >= 'a' && char <= 'z') || (char >= 'A' && char <= 'Z')
--                 in
--                     -- Backspace is "searchable" because it can be used to modify the search string
--                     if isAlpha char || Char.isDigit char || char == '\x08' then
--                         key (Searchable char)
--                     else
--                         pass


htmlNeverToHtmlMsg : Html Never -> Html Msg
htmlNeverToHtmlMsg =
    Html.map (always NoOp)

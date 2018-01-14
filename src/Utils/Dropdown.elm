port module Utils.Dropdown exposing (Dropdown, Msg, init, update, view)

import Html exposing (Html, Attribute, div, span, text, li, ul, input)
import Html.Attributes exposing (style, value, class, readonly)
import Html.Events as Events
import Json.Decode
import Utils.CommonTypes exposing (DropdownItem)
import Utils.CommonFunctions as Functions
import Char


port dropdownMenuScroll : String -> Cmd msg


scrollToDomId : String -> Maybe Int -> Cmd msg
scrollToDomId str id =
    let
        newId =
            Maybe.withDefault 1 id
    in
        dropdownMenuScroll (str ++ "-" ++ toString newId)


type alias Dropdown =
    { isOpen : Bool
    , mouseSelectedId : Maybe Int
    , keyboardSelectedId : Maybe Int
    , searchString : String
    , domId : String
    }


init : String -> Maybe Int -> Dropdown
init domId selectedId =
    { isOpen = False
    , mouseSelectedId = Nothing
    , keyboardSelectedId = Nothing
    , searchString = ""
    , domId = domId
    }


type Key
    = Esc
    | Enter
    | ArrowUp
    | ArrowDown
    | PageUp
    | PageDown
    | Home
    | End
    | Searchable Char


type Msg
    = ItemPicked DropdownItem
    | ItemEntered DropdownItem
    | ItemLeft
    | SetOpenState Bool
    | OnBlur
    | OnKey Key


type SkipAmount
    = First
    | Last
    | Exact Int


byId : Int -> List DropdownItem -> DropdownItem
byId id items =
    items
        |> List.filter (\t -> t.id == Just id)
        |> List.head
        |> Maybe.withDefault (DropdownItem Nothing "")


getDropdownText : Maybe Int -> List DropdownItem -> String
getDropdownText id dropdownItems =
    dropdownItems
        |> List.filter (\t -> t.id == id)
        |> List.map (\t -> t.name)
        |> List.head
        |> Maybe.withDefault ""


update : Msg -> Dropdown -> Maybe Int -> List DropdownItem -> ( Dropdown, Maybe Int, Cmd msg )
update msg dropdown selectedId dropdownItems =
    case msg of
        ItemPicked item ->
            ( { dropdown | isOpen = False }, dropdown.mouseSelectedId, Cmd.none )

        ItemEntered item ->
            ( { dropdown | mouseSelectedId = item.id }, selectedId, Cmd.none )

        ItemLeft ->
            ( { dropdown | mouseSelectedId = Nothing }, selectedId, Cmd.none )

        SetOpenState newState ->
            ( { dropdown | isOpen = newState, keyboardSelectedId = selectedId }, selectedId, scrollToDomId dropdown.domId selectedId )

        OnBlur ->
            if selectedId /= Nothing && dropdown.mouseSelectedId == Nothing then
                ( { dropdown | isOpen = False }, selectedId, Cmd.none )
            else
                ( { dropdown | isOpen = False }, dropdown.mouseSelectedId, Cmd.none )

        OnKey Esc ->
            ( { dropdown | isOpen = False }, selectedId, Cmd.none )

        OnKey Enter ->
            if dropdown.isOpen then
                ( { dropdown | isOpen = False }, dropdown.keyboardSelectedId, Cmd.none )
            else
                ( dropdown, selectedId, Cmd.none )

        OnKey ArrowUp ->
            pickerSkip dropdown (Exact -1) dropdownItems selectedId

        OnKey ArrowDown ->
            pickerSkip dropdown (Exact 1) dropdownItems selectedId

        OnKey PageUp ->
            pickerSkip dropdown (Exact -9) dropdownItems selectedId

        OnKey PageDown ->
            pickerSkip dropdown (Exact 9) dropdownItems selectedId

        OnKey Home ->
            pickerSkip dropdown First dropdownItems selectedId

        OnKey End ->
            pickerSkip dropdown Last dropdownItems selectedId

        OnKey (Searchable char) ->
            updateSearchString char dropdown dropdownItems selectedId


boundedIndex : List DropdownItem -> Int -> Int
boundedIndex dropdownSource index =
    if index < 0 then
        0
    else if index > List.length dropdownSource then
        List.length dropdownSource - 1
    else
        index


pickerSkip : Dropdown -> SkipAmount -> List DropdownItem -> Maybe Int -> ( Dropdown, Maybe Int, Cmd msg )
pickerSkip dropdown skipAmount dropdownItems selectedId =
    let
        newIndexCalc =
            case skipAmount of
                Exact skipCount ->
                    (Functions.defaultInt dropdown.keyboardSelectedId) + skipCount

                First ->
                    0

                Last ->
                    List.length dropdownItems - 1

        newIndex =
            boundedIndex dropdownItems newIndexCalc

        selectedItem =
            byId newIndex dropdownItems
    in
        if dropdown.isOpen then
            ( { dropdown | keyboardSelectedId = Just newIndex }, selectedId, scrollToDomId dropdown.domId selectedItem.id )
        else
            ( { dropdown | keyboardSelectedId = Just newIndex }, Just newIndex, Cmd.none )


view : Dropdown -> List DropdownItem -> Maybe Int -> Html Msg
view dropdown dropdownItems selectedId =
    let
        displayStyle =
            if dropdown.isOpen then
                ( "display", "block" )
            else
                ( "display", "none" )

        activeClass =
            if dropdown.isOpen then
                "e-focus e-popactive"
            else
                ""

        dropInputWidth =
            style [ ( "width", "100%" ) ]

        keyMsgDecoder =
            Events.keyCode
                |> Json.Decode.andThen (keyDecoder dropdown)
                |> Json.Decode.map OnKey
    in
        div [ Events.onWithOptions "keydown" { stopPropagation = True, preventDefault = True } keyMsgDecoder ]
            [ span
                [ onClick (SetOpenState (not dropdown.isOpen))
                , class ("e-ddl e-widget " ++ activeClass)
                , dropInputWidth
                ]
                [ span
                    [ class "e-in-wrap e-box" ]
                    [ input
                        [ class "e-input"
                        , readonly True
                        , value (getDropdownText selectedId dropdownItems)
                        , if dropdown.isOpen then
                            Events.onBlur OnBlur
                          else
                            style []
                        ]
                        []
                    , span [ class "e-select" ]
                        [ span [ class "e-icon e-arrow-sans-down" ] []
                        ]
                    ]
                ]
            , ul [ style <| displayStyle :: dropdownList, class "dropdown-ul" ] (viewItem dropdown dropdownItems)
            ]


getId : String -> DropdownItem -> String
getId id item =
    id ++ "-" ++ Functions.defaultIntToString item.id


viewItem : Dropdown -> List DropdownItem -> List (Html Msg)
viewItem dropdown dropdownItems =
    let
        numItems =
            dropdownItems
                |> List.map (\t -> String.length t.name)
                |> List.sortBy identity
                |> List.reverse
                |> List.head
                |> Maybe.withDefault 150

        width =
            numItems * 6

        commonWidth =
            [ ( "width", toString width ++ "px" ) ]

        mouseActive =
            [ ( "background-color", "" ), ( "color", "#808080" ) ] ++ commonWidth

        keyActive =
            [ ( "background-color", "#f4f4f4" ), ( "color", "#333" ) ] ++ commonWidth
    in
        dropdownItems
            |> List.map
                (\item ->
                    li
                        [ onClick (ItemPicked item)
                        , Events.onMouseEnter (ItemEntered item)
                        , Events.onMouseLeave ItemLeft
                        , class "dropdown-li"
                        , if dropdown.mouseSelectedId == item.id then
                            style mouseActive
                          else if dropdown.keyboardSelectedId == item.id && dropdown.isOpen then
                            style keyActive
                          else
                            style commonWidth
                        , Html.Attributes.id (getId dropdown.domId item)
                        ]
                        [ text item.name ]
                )


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


updateSearchString : Char -> Dropdown -> List DropdownItem -> Maybe Int -> ( Dropdown, Maybe Int, Cmd msg )
updateSearchString searchChar dropdown dropdownItems selectedId =
    let
        searchString =
            -- Manage backspace character
            if searchChar == '\x08' then
                String.dropRight 1 dropdown.searchString
            else
                dropdown.searchString ++ String.toLower (String.fromChar searchChar)

        maybeSelectedItem =
            dropdownItems
                |> List.filter (\t -> String.startsWith searchString (String.toLower t.name))
                |> List.head
    in
        case maybeSelectedItem of
            Just t ->
                --todo... why was this here? , selectedId = selectedItem.id
                ( { dropdown | searchString = searchString }, selectedId, Cmd.none )

            --TODO
            --! [ scrollToDomId (getId dropdown.domId t.id) ]
            Nothing ->
                ( dropdown, selectedId, Cmd.none )


keyDecoder : Dropdown -> Int -> Json.Decode.Decoder Key
keyDecoder dropdown keyCode =
    let
        -- This is necessary to ensure that the key is not consumed and can propagate to the parent
        pass =
            Json.Decode.fail ""

        key =
            Json.Decode.succeed
    in
        case keyCode of
            13 ->
                key Enter

            27 ->
                -- Consume Esc only if the Menu is open
                if dropdown.isOpen then
                    pass
                else
                    key Esc

            -- 32 ->
            --     key Space
            33 ->
                key PageUp

            34 ->
                key PageDown

            35 ->
                key End

            36 ->
                key Home

            38 ->
                key ArrowUp

            40 ->
                key ArrowDown

            _ ->
                let
                    char =
                        Char.fromCode keyCode

                    isAlpha char =
                        (char >= 'a' && char <= 'z') || (char >= 'A' && char <= 'Z')
                in
                    -- Backspace is "searchable" because it can be used to modify the search string
                    if isAlpha char || Char.isDigit char || char == '\x08' then
                        key (Searchable char)
                    else
                        pass

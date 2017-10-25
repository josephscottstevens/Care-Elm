port module Main exposing (..)

import Load exposing (..)
import Model exposing (..)
import HtmlHelper exposing (..)
import Html exposing (Html, text, div, input, program, button, select, option)
import Html.Attributes exposing (style, class, placeholder, id, type_, value)
import Html.Events exposing (onClick, onInput)
import Table


port sendTestDate : String -> Cmd msg


port getTestDate : (String -> msg) -> Sub msg


subscriptions : Model -> Sub Msg
subscriptions model =
    getTestDate UpdateStartDate


main : Program Never Model Msg
main =
    Html.program
        { init = init
        , view = view
        , update = update
        , subscriptions = subscriptions
        }


init : ( Model, Cmd Msg )
init =
    ( emptyModel, getEmployment )


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        EditStart employer ->
            ( { model | state = Edit employer }, sendTestDate employer.dob )

        EditSave employer ->
            ( { model | state = Grid, employers = (updateEmployers model.employers employer) }, Cmd.none )

        EditCancel ->
            ( { model | state = Grid }, Cmd.none )

        Load (Ok model) ->
            ( { model | state = Grid, employers = (newEmployers model.employers) }, Cmd.none )

        Load (Err t) ->
            ( { model | state = Error t }, Cmd.none )

        UpdateState emp newState ->
            ( { model | state = Edit { emp | state = newState } }, Cmd.none )

        UpdateCity emp newCity ->
            ( { model | state = Edit { emp | city = newCity } }, Cmd.none )

        UpdateStartDate newDob ->
            case model.state of
                Edit emp ->
                    ( { model | state = Edit { emp | dob = newDob } }, Cmd.none )

                _ ->
                    ( model, Cmd.none )

        SetQuery newQuery ->
            ( { model | query = newQuery }, Cmd.none )

        SetTableState newState ->
            ( { model | tableState = newState }, Cmd.none )

        Reset ->
            ( emptyModel, getEmployment )


view : Model -> Html Msg
view model =
    case model.state of
        Initial ->
            div [] [ text "loading" ]

        Grid ->
            div []
                [ button [ class "btn btn-default", controlStyle, onClick Reset ] [ text "reset" ]
                , Table.view config model.tableState model.employers
                ]

        Edit emp ->
            div []
                [ input [ placeholder "Date of birth", type_ "text", class "e-textbox", controlStyle, id "testDate", value emp.dob ] []
                , input [ placeholder "City", class "e-textbox", controlStyle, onInput (UpdateCity emp), value emp.city ] []
                , input [ placeholder "State", class "e-textbox", controlStyle, onInput (UpdateState emp), value emp.state ] []
                , button [ class "btn btn-default", controlStyle, onClick (EditSave emp) ] [ text "save" ]
                , button [ class "btn btn-default", controlStyle, onClick EditCancel ] [ text "cancel" ]
                ]

        Error err ->
            div [] [ text (toString err) ]


config : Table.Config Employer Msg
config =
    Table.config
        { toId = .city
        , toMsg = SetTableState
        , columns =
            [ editColumn
            , Table.stringColumn "Date of birth" .dob
            , Table.stringColumn "Email" .email
            , Table.stringColumn "Address Line 1" .addressLine1
            , Table.stringColumn "Address Line 2" .addressLine2
            , Table.stringColumn "City" .city
            , Table.stringColumn "State" .state
            , Table.stringColumn "Zip Code" .zipCode
            , Table.stringColumn "Phone" .phone
            ]
        }


editColumn : Table.Column Employer Msg
editColumn =
    Table.veryCustomColumn
        { name = ""
        , viewData = editButton
        , sorter = Table.unsortable
        }


editButton : Employer -> Table.HtmlDetails Msg
editButton emp =
    Table.HtmlDetails []
        [ button [ class "btn btn-default", controlStyle, onClick (EditStart emp) ] [ text "Edit" ]
        ]

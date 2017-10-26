port module Main exposing (..)

import Load exposing (..)
import Model exposing (..)
import Styles exposing (..)
import Html exposing (Html, text, div, input, program, button, select, option, span)
import Html.Attributes exposing (style, class, placeholder, id, type_, value)
import Html.Events exposing (onClick, onInput)
import Grid exposing (..)
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
            -- interesting, so ! [] is shorthand for ,( ... Cmd.none )
            { model | tableState = newState } ! []

        Reset ->
            ( emptyModel, getEmployment )


view : Model -> Html Msg
view model =
    let
        lowerQuery =
            String.toLower model.query

        filteredEmployers =
            model.employers
                |> List.filter (String.contains lowerQuery << String.toLower << .addressLine1)

        len =
            (List.length filteredEmployers) // 12

        rng =
            List.range 1 (len + 1)
                |> List.map (\t -> text (toString t ++ " "))

        employersCount =
            toString (List.length filteredEmployers)
    in
        case model.state of
            Initial ->
                div [] [ text "loading" ]

            Grid ->
                div []
                    [ button [ class "btn btn-default", controlStyle, onClick Reset ] [ text "reset" ]
                    , input [ placeholder "Search by Address", onInput SetQuery, value model.query ] []
                    , Table.view config model.tableState (filteredEmployers |> List.take 12)
                    , div [] [ text ("Total items: " ++ employersCount) ]
                    , span [] rng
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

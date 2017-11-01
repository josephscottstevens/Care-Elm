port module Main exposing (..)

import Load exposing (..)
import Model exposing (..)
import Html exposing (Html, text, div, input, program, button, select, option, span)
import Html.Attributes exposing (style, class, placeholder, id, type_, value)
import Html.Events exposing (onClick, onInput)
import Grid exposing (..)
import Table


port sendTestDate : String -> Cmd msg


port getTestDate : (String -> msg) -> Sub msg


subscriptions : Model -> Sub Msg
subscriptions model =
    Sub.none



-- getTestDate UpdateStartDate


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
            ( { model | state = Edit employer }, Cmd.none )

        -- , sendTestDate employer.dOB
        -- EditSave employer ->
        --     ( { model | state = Grid, employment = (updateEmployers model.enrollment employer) }, Cmd.none )
        EditCancel ->
            ( { model | state = Grid }, Cmd.none )

        Load (Ok model) ->
            ( { model | state = Grid, billingCcm = (newEmployers model.billingCcm) }, Cmd.none )

        Load (Err t) ->
            ( { model | state = Error t }, Cmd.none )

        -- UpdateState emp newState ->
        --     ( { model | state = Edit { emp | state = newState } }, Cmd.none )
        -- UpdateCity emp newCity ->
        --     ( { model | state = Edit { emp | city = newCity } }, Cmd.none )
        -- UpdateStartDate newDob ->
        --     case model.state of
        --         Edit emp ->
        --             ( { model | state = Edit { emp | dob = newDob } }, Cmd.none )
        --         _ ->
        --             ( model, Cmd.none )
        SetQuery newQuery ->
            ( { model | query = newQuery }, Cmd.none )

        SetTableState newState ->
            -- interesting, so ! [] is shorthand for ,( ... Cmd.none )
            ( { model | tableState = newState }, Cmd.none )

        Reset ->
            ( emptyModel, getEmployment )


view : Model -> Html Msg
view model =
    let
        lowerQuery =
            String.toLower model.query

        filteredEmployers =
            model.billingCcm
                |> List.filter (String.contains lowerQuery << String.toLower << .patientName)

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
                    [ button [ class "btn btn-default", onClick Reset ] [ text "reset" ]
                    , input [ class "form-control", placeholder "Search by Address", onInput SetQuery, value model.query ] []
                    , Table.view config model.tableState (filteredEmployers |> List.take 12)
                    , div [] [ text ("Total items: " ++ employersCount) ]
                    , span [] rng
                    ]

            Edit emp ->
                div []
                    [ input [ placeholder "Date of birth", type_ "text", class "e-textbox", id "testDate", value emp.doB ] []

                    -- , input [ placeholder "City", class "e-textbox", onInput (UpdateCity emp), value emp.city ] []
                    -- , input [ placeholder "State", class "e-textbox", onInput (UpdateState emp), value emp.state ] []
                    -- , button [ class "btn btn-default", onClick (EditSave emp) ] [ text "save" ]
                    , button [ class "btn btn-default", onClick EditCancel ] [ text "cancel" ]
                    ]

            Error err ->
                div [] [ text (toString err) ]

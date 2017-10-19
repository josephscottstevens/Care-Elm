port module Main exposing (..)

import Load exposing (getEmployment)
import Model exposing (..)
import HtmlHelper exposing (..)
import Html exposing (Html, text, div, input, program, button, select, option)
import Html.Attributes exposing (style, class, placeholder, id, type_, value)
import Html.Events exposing (onClick, onInput)


-- Todo,
-- add the rest of the missing fields
-- Re-add port \ subscription
-- Super heavy right now on boilerplate to update row
-- Test to see if I can update stuff
-- Fix missing column header
-- port sendTestDate : String -> Cmd msg
-- port getTestDate : (String -> msg) -> Sub msg
-- subscriptions model t i =
--     getTestDate (UpdateStartDate t i)


main : Program Never Model Msg
main =
    Html.program
        { init = init
        , view = view
        , update = update
        , subscriptions = always Sub.none --subscriptions
        }


init : ( Model, Cmd Msg )
init =
    ( emptyModel, getEmployment )


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        EditStart idx ->
            ( { model | state = Edit idx }, Cmd.none )

        --sendTestDate "model.testDate"
        EditEnd ->
            ( { model | state = Grid }, Cmd.none )

        Load (Ok newModel) ->
            ( { newModel | state = Grid }, Cmd.none )

        Load (Err t) ->
            ( { model | state = Error t }, Cmd.none )

        UpdateState i t ->
            ( { model | employers = (updateEmployerState model.employers i t) }, Cmd.none )

        UpdateCity i t ->
            ( { model | employers = (updateEmployerCity model.employers i t) }, Cmd.none )

        UpdateStartDate i t ->
            ( { model | employers = (updateEmployerStartDate model.employers i t) }, Cmd.none )


view : Model -> Html Msg
view model =
    case model.state of
        Initial ->
            div [] [ text "loading" ]

        Grid ->
            div []
                [ div [ gridStyle ] (employmentHeaders :: (employmentRows model.employers))
                ]

        Edit idx ->
            case getEmployer model.employers idx of
                Just emp ->
                    div []
                        [ input [ placeholder "City", class "e-textbox", controlStyle, onInput (UpdateCity <| idx), value emp.city ] []
                        , input [ placeholder "State", class "e-textbox", controlStyle, onInput (UpdateState <| idx), value emp.state ] []

                        --, input [ type_ "text", class "e-textbox", controlStyle, id "testDate", value emp.dueAt ] [ text emp.dueAt ]
                        , button [ class "btn btn-default", controlStyle, onClick EditEnd ] [ text "save" ]
                        ]

                Nothing ->
                    div [] [ text "row is gone" ]

        Error err ->
            div [] [ text (toString err) ]

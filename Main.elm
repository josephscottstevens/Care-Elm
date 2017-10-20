port module Main exposing (..)

import Load exposing (getEmployment)
import Model exposing (..)
import HtmlHelper exposing (..)
import UpdateHelper exposing (..)
import Html exposing (Html, text, div, input, program, button, select, option)
import Html.Attributes exposing (style, class, placeholder, id, type_, value)
import Html.Events exposing (onClick, onInput)
import Array


port sendTestDate : String -> Cmd msg


port getTestDate : (String -> msg) -> Sub msg


subscriptions : Model -> Sub Msg
subscriptions model =
    getTestDate (UpdateStartDate 0)


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
        EditStart idx ->
            ( { model | state = Edit idx }, sendTestDate (testDt model.employers idx) )

        EditEnd ->
            ( { model | state = Grid }, Cmd.none )

        Load (Ok newModel) ->
            ( { newModel | state = Grid }, Cmd.none )

        Load (Err t) ->
            ( { model | state = Error t }, Cmd.none )

        UpdateState i t ->
            ( updateEmployeeList model i t (\emp newState -> { emp | state = newState }), Cmd.none )

        UpdateCity i t ->
            ( updateEmployeeList model i t (\emp newCity -> { emp | city = newCity }), Cmd.none )

        UpdateStartDate i t ->
            ( updateEmployeeList model i t (\emp newStartDate -> { emp | startDate = newStartDate }), Cmd.none )


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
            case Array.get idx model.employers of
                Just emp ->
                    div []
                        [ input [ placeholder "City", class "e-textbox", controlStyle, onInput (UpdateCity <| idx), value emp.city ] []
                        , input [ placeholder "State", class "e-textbox", controlStyle, onInput (UpdateState <| idx), value emp.state ] []
                        , input [ placeholder "Start Date", type_ "text", class "e-textbox", controlStyle, id "testDate", value emp.startDate ] []
                        , button [ class "btn btn-default", controlStyle, onClick EditEnd ] [ text "save" ]
                        ]

                Nothing ->
                    div [] [ text "row is gone" ]

        Error err ->
            div [] [ text (toString err) ]

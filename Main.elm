port module Main exposing (..)

import Load exposing (getEmployment)
import Model exposing (..)
import HtmlHelper exposing (..)
import UpdateHelper exposing (..)
import Html exposing (Html, text, div, input, program, button, select, option)
import Html.Attributes exposing (style, class, placeholder, id, type_, value)
import Html.Events exposing (onClick, onInput)


-- Fix me


port sendTestDate : String -> Cmd msg


port getTestDate : (Employer -> msg) -> Sub msg


subscriptions : Model -> Sub Msg
subscriptions model =
    getTestDate EditStart


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
            ( { model | state = Edit employer }, sendTestDate employer.startDate )

        EditEnd ->
            ( { model | state = Grid }, Cmd.none )

        Load (Ok newModel) ->
            ( { newModel | state = Grid }, Cmd.none )

        Load (Err t) ->
            ( { model | state = Error t }, Cmd.none )

        -- Very interesting... so I now have two employee records... and the UI keeps me in check... weird!
        UpdateState emp newState ->
            let
                newEmployers =
                    model.employers
                        |> List.map
                            (\t ->
                                if t == emp then
                                    { emp | state = newState }
                                else
                                    t
                            )
            in
                ( { model | employers = newEmployers }, Cmd.none )

        -- Re add me!
        -- UpdateCity i t ->
        --     ( updateEmployeeList model i t (\emp newCity -> { emp | city = newCity }), Cmd.none )
        -- UpdateStartDate i t ->
        --     ( updateEmployeeList model i t (\emp newStartDate -> { emp | startDate = newStartDate }), Cmd.none )
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
                , div [ gridStyle ] (employmentHeaders :: (employmentRows model.employers))
                ]

        Edit emp ->
            div []
                [ -- input [ placeholder "City", class "e-textbox", controlStyle, onInput (UpdateCity emp), value emp.city ] []
                  input [ placeholder "State", class "e-textbox", controlStyle, onInput (UpdateState emp), value emp.state ] []
                , input [ placeholder "Start Date", type_ "text", class "e-textbox", controlStyle, id "testDate", value emp.startDate ] []
                , button [ class "btn btn-default", controlStyle, onClick EditEnd ] [ text "save" ]
                ]

        Error err ->
            div [] [ text (toString err) ]

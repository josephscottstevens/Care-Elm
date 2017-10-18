port module Main exposing (..)

import Load exposing (getEmployment)
import Model exposing (..)
import HtmlHelper exposing (..)
import Html exposing (Html, text, div, input, program, button, select, option)
import Html.Attributes exposing (style, class, placeholder, id, type_, value)
import Html.Events exposing (onClick, onInput)


port sendTestDate : String -> Cmd msg


port getTestDate : (String -> msg) -> Sub msg


subscriptions : Model -> Sub Msg
subscriptions model =
    getTestDate UpdateTestDate


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
        EditStart ->
            ( { model | state = Edit }, sendTestDate model.testDate )

        EditEnd ->
            ( { model | state = Grid }, Cmd.none )

        Load (Ok newModel) ->
            ( { newModel | state = Grid }, Cmd.none )

        Load (Err t) ->
            ( { model | state = Error t }, Cmd.none )

        UpdateTestDate t ->
            ( { model | testDate = t }, Cmd.none )

        UpdateFirst t ->
            ( { model | testFirst = t }, Cmd.none )

        UpdateLast t ->
            ( { model | testLast = t }, Cmd.none )


view : Model -> Html Msg
view model =
    case model.state of
        Initial ->
            div [] [ text "loading" ]

        Grid ->
            div []
                [ div [] [ text "grid mode" ]
                , button [ onClick EditStart ] [ text "edit" ]
                , div [ gridStyle ] (employmentHeaders :: (employmentRows model.employers))
                ]

        Edit ->
            div []
                [ div [] [ text "edit mode" ]
                , button [ onClick EditEnd ] [ text "edit" ]
                , input [ type_ "text", class "e-textbox", id "testDate", value model.testDate ] [ text model.testDate ]
                , input [ placeholder "First", class "e-textbox", onInput UpdateFirst, value model.testFirst ] []
                , input [ placeholder "Last", class "e-textbox", onInput UpdateLast, value model.testLast ] []
                , div [] [ text model.testDate ]
                ]

        Error err ->
            div [] [ text (toString err) ]

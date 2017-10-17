port module Main exposing (..)

import Load exposing (getEmployment)
import Model exposing (..)
import HtmlHelper exposing (..)
import Html exposing (Html, text, div, input, program, button, select, option)
import Html.Attributes exposing (style, class, placeholder, id, type_, value)
import Html.Events exposing (onClick)


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
    ( emptyModel
    , getEmployment
    )


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        EditStart ->
            ( model, Cmd.none )

        EditEnd ->
            ( model, Cmd.none )

        Load (Ok newModel) ->
            ( newModel, sendTestDate newModel.testDate )

        Load (Err t) ->
            ( { model | state = Error t }, Cmd.none )

        UpdateTestDate t ->
            ( { model | testDate = t }, Cmd.none )


view : Model -> Html Msg
view model =
    case model.state of
        Initial ->
            div [] [ text "loading" ]

        Grid ->
            div []
                [ button [ onClick EditStart ] [ text "edit" ]
                , input [ type_ "text", class "e-textbox", id "testDate" ] []
                , div [] [ text model.testDate ]
                , div [ gridStyle ] (employmentHeaders :: (employmentRows model.employers))
                , priorityList
                ]

        Edit ->
            div []
                [ button [ onClick EditEnd ] [ text "edit" ]
                , div [] [ text "edit mode" ]
                ]

        Error err ->
            div [] [ text (toString err) ]

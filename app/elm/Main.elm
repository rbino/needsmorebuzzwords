module Main exposing (main)


import Html exposing (Html, div, text, program, textarea)
import Html.Events exposing (onInput)


-- MODEL


type alias Model =
    String


init : ( Model, Cmd Msg )
init =
    ( "", Cmd.none )



-- MESSAGES


type Msg
    = NoOp
    | TextChanged String



-- VIEW


view : Model -> Html Msg
view model =
    div
        []
        [ div
            []
            [ textarea
                [ onInput TextChanged ]
                []
            ]
        , div
            []
            [ text model ]
        ]



-- UPDATE


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        NoOp ->
            ( model, Cmd.none )

        TextChanged text ->
            ( text, Cmd.none )



-- SUBSCRIPTIONS


subscriptions : Model -> Sub Msg
subscriptions model =
    Sub.none



-- MAIN


main : Program Never Model Msg
main =
    program
        { init = init
        , view = view
        , update = update
        , subscriptions = subscriptions
        }

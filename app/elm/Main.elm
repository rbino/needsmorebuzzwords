module Main exposing (main)


import Html exposing (Html, div, text, program, textarea, input)
import Html.Attributes as H exposing (min, max, type_)
import Html.Events exposing (onInput)
import Random exposing (Seed, initialSeed, minInt, maxInt)


-- MODEL


type alias Model =
    { inputText : String
    , outputText : String
    , buzzwordRatio : Float
    , randomSeed : Seed
    }


init : ( Model, Cmd Msg )
init =
    ( initModel, Cmd.none )


initModel : Model
initModel =
    { inputText = ""
    , outputText = ""
    , buzzwordRatio = 0.1
    , randomSeed = initialSeed 0
    }


initCmd : Cmd Msg
initCmd =
    Random.int minInt maxInt
        |> Random.generate RandomIntGenerated



-- MESSAGES


type Msg
    = NoOp
    | TextChanged String
    | BuzzwordRatioChanged String
    | RandomIntGenerated Int



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
            [ input
                [ type_ "range"
                , H.min <| toString scaledMinBuzzwordRatio
                , H.max <| toString scaledMaxBuzzwordRatio
                , onInput BuzzwordRatioChanged
                ]
                []
            ]
        , div
            []
            [ text model.outputText ]
        ]



-- UPDATE


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        NoOp ->
            ( model, Cmd.none )

        TextChanged text ->
            ( { model | inputText = text }
            , Cmd.none )

        BuzzwordRatioChanged rangeText ->
            let
                newBuzzwordRatio =
                    String.toFloat rangeText
                    |> Result.withDefault (model.buzzwordRatio * rangeScalingFactor)
                    |> flip (/) rangeScalingFactor
            in
                ( { model | buzzwordRatio = newBuzzwordRatio}, Cmd.none )

        RandomIntGenerated randomInt ->
            ( { model | randomSeed = initialSeed randomInt }, Cmd.none )




-- SUBSCRIPTIONS


subscriptions : Model -> Sub Msg
subscriptions model =
    Sub.none



-- CONSTANTS


rangeScalingFactor : Float
rangeScalingFactor =
    1000


scaledMinBuzzwordRatio : Float
scaledMinBuzzwordRatio =
    0.1 * rangeScalingFactor


scaledMaxBuzzwordRatio : Float
scaledMaxBuzzwordRatio =
    10 * rangeScalingFactor



-- MAIN


main : Program Never Model Msg
main =
    program
        { init = init
        , view = view
        , update = update
        , subscriptions = subscriptions
        }

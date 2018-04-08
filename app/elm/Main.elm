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
            ( updateOutput { model | inputText = text }
            , Cmd.none )

        BuzzwordRatioChanged rangeText ->
            let
                newBuzzwordRatio =
                    String.toFloat rangeText
                    |> Result.withDefault (model.buzzwordRatio * rangeScalingFactor)
                    |> flip (/) rangeScalingFactor

            in
                ( updateOutput { model | buzzwordRatio = newBuzzwordRatio }
                , Cmd.none )

        RandomIntGenerated randomInt ->
            ( { model | randomSeed = initialSeed randomInt }, Cmd.none )


updateOutput : Model -> Model
updateOutput model =
    let
        (newOutputText, newSeed) =
            buzzwordize
                model.inputText
                model.buzzwordRatio
                model.randomSeed
    in
        { model | outputText = newOutputText, randomSeed = newSeed }


-- SUBSCRIPTIONS


subscriptions : Model -> Sub Msg
subscriptions model =
    Sub.none



-- DATA MANIPULATION


buzzwordize : String -> Float -> Seed -> (String, Seed)
buzzwordize input buzzwordRatio seed =
    let
        splitted =
            String.split " " input
                |> List.filter (not << String.isEmpty)
        finalAcc =
            List.foldl
                (buzzwordIntersperse buzzwordRatio)
                { outputList = [], ratioSum = 0, seed = seed }
                splitted
    in
        ( finalAcc.outputList
            |> List.reverse
            |> String.join " "
        , finalAcc.seed
        )


type alias BuzzwordIntersperseAcc =
    { outputList : List String
    , ratioSum : Float
    , seed : Seed
    }


buzzwordIntersperse : Float -> String -> BuzzwordIntersperseAcc -> BuzzwordIntersperseAcc
buzzwordIntersperse buzzwordRatio word acc =
    let
        nBuzzwords = floor acc.ratioSum
        newRatioSum = acc.ratioSum - toFloat nBuzzwords + buzzwordRatio
        (withBuzzwords, newSeed) = prependBuzzwords nBuzzwords acc.seed acc.outputList
        newOutputList = word :: withBuzzwords
    in
        { acc | outputList = newOutputList, seed = newSeed, ratioSum = newRatioSum }


prependBuzzwords : Int -> Seed -> List String -> (List String, Seed)
prependBuzzwords n seed words =
    case n of
        0 ->
            (words, seed)

        _ ->
            let
                (randomIdx, newSeed) = Random.step (Random.int 0 (buzzwordsLength - 1)) seed
                buzzword =
                    buzzwords
                        |> List.drop randomIdx
                        |> List.head
                        |> Maybe.withDefault ""
            in
                prependBuzzwords (n - 1) newSeed (buzzword :: words)


-- CONSTANTS


buzzwords : List String
buzzwords =
    [ "cloud based"
    , "blockchain based"
    , "IoT ready"
    , "webscale"
    , "cyber"
    , "containerized"
    , "reproducible"
    , "agile"
    , "functional"
    , "cryptocurrency enabled"
    ]


buzzwordsLength : Int
buzzwordsLength =
    List.length buzzwords


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

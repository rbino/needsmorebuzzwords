module Main exposing (main)


import Bootstrap.Form.Input as Input
import Bootstrap.Form.Textarea as Textarea
import Bootstrap.Grid as Grid
import Bootstrap.Grid.Col as Col
import Bootstrap.Utilities.Border as Border
import Bootstrap.Utilities.Spacing as Spacing
import Html exposing (Html, a, h1, div, text, p, program)
import Html.Attributes as H exposing (href, placeholder, readonly, style, type_)
import Random exposing (Seed, initialSeed, minInt, maxInt)
import Round


-- MODEL


type alias Model =
    { inputText : String
    , outputText : String
    , buzzwordRatio : Float
    , randomSeed : Seed
    }


init : ( Model, Cmd Msg )
init =
    ( initModel, initCmd )


initModel : Model
initModel =
    { inputText = initialText
    , outputText = ""
    , buzzwordRatio = 0.5
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
    Grid.container []
        [ Grid.row []
            [ Grid.col
                [ Col.md6
                , Col.offsetMd3
                ]
                [ h1
                    [ Spacing.mt2 ]
                    [ text "Needs more buzzwords" ]
                , mainContent model
                , footer
                ]
            ]
        ]


mainContent : Model -> Html Msg
mainContent model =
    div
        []
        [ div
            [ Spacing.mt2
            , Spacing.mb2
            ]
            [ text "Paste your not-enough-buzzwordy text here" ]
        , Textarea.textarea
            [ Textarea.id "inputtext"
            , Textarea.onInput TextChanged
            , Textarea.rows 3
            , Textarea.attrs
                [ placeholder initialText ]
            ]
        , div
            [ Spacing.mt4
            , Spacing.mb2
            ]
            [ text <| "Words/buzzwords ratio: " ++ Round.round 2 model.buzzwordRatio ]
        , Input.number
            [ Input.onInput BuzzwordRatioChanged
            , Input.attrs
                [ type_ "range"
                , H.min <| toString 0
                , H.max <| toString 100
                , Spacing.mt2
                , Spacing.mb4
                , Border.none
                , fromLogScale model.buzzwordRatio
                    |> round
                    |> toString
                    |> H.value
                ]
            ]
        , div
            [ Spacing.mt2
            , Spacing.mb2
            ]
            [ text "Here is your management-approved text" ]
        , Textarea.textarea
            [ Textarea.id "outputtext"
            , Textarea.onInput TextChanged
            , Textarea.rows 4
            , Textarea.value model.outputText
            , Textarea.attrs
                [ readonly True
                , style
                    [ ("background-color", "transparent") ]
                ]
            ]
        ]


footer : Html Msg
footer =
    div
        [ style
            [ ("background-color", "#ededed")
            ]
        , Spacing.mt3
        , Spacing.p3
        ]
        [ text "Made by "
        , a
            [ href "https://github.com/rbino" ]
            [ text "rbino" ]
        , text ", source on "
        , a
            [ href "https://github.com/rbino/needsmorebuzzwords" ]
            [ text "Github" ]
        ]



-- UPDATE


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        NoOp ->
            ( model, Cmd.none )

        TextChanged text ->
            let
                newInputText =
                    if text == "" then
                        initialText
                    else
                        text

            in
                ( updateOutput { model | inputText = newInputText }
                , Cmd.none )

        BuzzwordRatioChanged rangeText ->
            let
                newBuzzwordRatio =
                    String.toFloat rangeText
                    |> Result.withDefault model.buzzwordRatio
                    |> toLogScale

            in
                ( updateOutput { model | buzzwordRatio = newBuzzwordRatio }
                , Cmd.none )

        RandomIntGenerated randomInt ->
            ( updateOutput { model | randomSeed = initialSeed randomInt }
            , Cmd.none )


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



-- UTILS


toLogScale : Float -> Float
toLogScale value =
    let
        minv = logBase e minBuzzwordRatio
        maxv = logBase e maxBuzzwordRatio
        scale = (maxv - minv) / 100
    in
        e ^ (minv + scale * value)


fromLogScale : Float -> Float
fromLogScale value =
    let
        minv = logBase e minBuzzwordRatio
        maxv = logBase e maxBuzzwordRatio
        scale = (maxv - minv) / 100
    in
        ((logBase e value) - minv) / scale



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
                lastWord =
                    List.head words
                    |> Maybe.withDefault ""
                buzzword =
                    buzzwords
                        |> List.drop randomIdx
                        |> List.head
                        |> Maybe.withDefault ""
            in
                if lastWord /= buzzword then
                    prependBuzzwords (n - 1) newSeed (buzzword :: words)
                else
                    prependBuzzwords n newSeed words


-- CONSTANTS


buzzwords : List String
buzzwords =
    [ "2.0"
    , "4.0"
    , "agile"
    , "AI"
    , "bleeding edge"
    , "blockchain based"
    , "cloud"
    , "containerized"
    , "cryptocurrency enabled"
    , "cyber"
    , "data-driven"
    , "deep learning"
    , "devops oriented"
    , "disruptive"
    , "distributed"
    , "end-to-end"
    , "enterprise"
    , "functional"
    , "growth hacking"
    , "immersive"
    , "infinitely scalable"
    , "innovative"
    , "IoT ready"
    , "machine learning"
    , "microservice oriented"
    , "mobile-first"
    , "performance-driven"
    , "production ready"
    , "reactive"
    , "real-time"
    , "reproducible"
    , "single page"
    , "social"
    , "UX-driven"
    , "viral"
    , "virtualized"
    , "web"
    , "webscale"
    , "whitepaper-validated"
    ]


buzzwordsLength : Int
buzzwordsLength =
    List.length buzzwords


minBuzzwordRatio : Float
minBuzzwordRatio =
    0.1


maxBuzzwordRatio : Float
maxBuzzwordRatio =
    10


initialText : String
initialText =
    "Buy our product with features."



-- MAIN


main : Program Never Model Msg
main =
    program
        { init = init
        , view = view
        , update = update
        , subscriptions = subscriptions
        }

import Browser
import Browser.Events
import Html exposing (Html)
import Html.Attributes
import Time exposing (Posix)
import Random
import Http
import Json.Encode as Encode
import Json.Decode as Decode

import Collage exposing (..)
import Collage.Layout as Layout
import Collage.Render exposing (svg)
import Collage.Text as Text
import Collage.Events as Events

import Color
import Animation as A

import Snowflake
import Config exposing (config)


-- INIT

main = Browser.document
    { init = init
    , update = update
    , view = (\m -> { title = "The Hungry Snowman", body = view m })
    , subscriptions = subscriptions
    }

model0 =
    Model 0
        []
        (0, 0)
        0
        0
        Nothing
        Unstarted

init : () -> (Model, Cmd Msg)
init _ =
  (model0, Cmd.none)

subscriptions : Model -> Sub Msg
subscriptions model =
    Sub.batch
        [ Browser.Events.onAnimationFrameDelta Tick
        , Time.every config.snowflakeFrequency NewSnowflake
        ]


-- TYPES

type alias Model =
  { clock : A.Clock
  , snowflakePositions : List Snowflake.Position
  , randomness : ( Float, Float )
  , playerX : Float
  , score : Int
  , totalFlakes : Maybe Int
  , state : GameState
  }

type Msg
    = Tick Float
    | NewRandom (Float, Float)
    | NewSnowflake Posix
    | MouseMove Point
    | StartGame
    | LoadedFlakes (Result Http.Error Int)
    | SavedFlakes (Result Http.Error ())

type GameState
    = Unstarted
    | Playing Float
    | Won Int

-- UPDATE

update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        Tick time ->
            case model.state of
                Playing startTime ->
                    let
                        (caught, uncaught) = List.partition (isFlakeCaught model) model.snowflakePositions
                        newScore = model.score + List.length caught
                        clock = model.clock + time
                        won = compare (clock - startTime) (config.gameLengthSeconds * 1000) == GT
                    in
                        ( { model
                            | clock = clock
                            , score = newScore
                            , snowflakePositions = uncaught
                            , state = if won then Won newScore else model.state
                        }, if won then finishGame newScore model.totalFlakes else Cmd.none )

                _ ->
                    ({ model | clock = model.clock + time }, Cmd.none )

        NewRandom (x, y) ->
            ( { model | randomness = (x, y) }, Cmd.none )

        NewSnowflake clock ->
            let
                position = Snowflake.initPosition model.clock model.randomness
            in
               ({ model | snowflakePositions = position :: (List.take config.maxSnowflakes model.snowflakePositions) }
               , Random.generate NewRandom (Random.pair (Random.float 0 1) (Random.float 0 1))
               )

        MouseMove point ->
            case model.state of
                Playing startTime ->
                    ( { model | playerX = Tuple.first point }, Cmd.none )

                _ ->
                    ( model, Cmd.none )

        StartGame ->
            ( { model | state = Playing model.clock, score = 0 }, loadFlakes)

        LoadedFlakes result ->
            case result of
                Ok count ->
                    ( { model | totalFlakes = Just count }, Cmd.none )

                Err _ ->
                    ( model, Cmd.none )

        SavedFlakes _ ->
            ( model, Cmd.none )


finishGame : Int -> Maybe Int -> Cmd Msg
finishGame newScore totalFlakes =
    case (newScore, totalFlakes) of
        (count, Just flakes) ->
            saveFlakes (flakes + count)

        _ ->
            Cmd.none


loadFlakes : Cmd Msg
loadFlakes =
  Http.get
    { url = "https://api.jsonbin.io/b/5c16a811dbf79646d0fca78b/latest"
    , expect = Http.expectJson LoadedFlakes (Decode.field "flakes" Decode.int)
    }

saveFlakes : Int -> Cmd Msg
saveFlakes count =
    Http.request
        { method = "PUT"
        , headers = []
        , url = "https://api.jsonbin.io/b/5c16a811dbf79646d0fca78b"
        , body = Http.jsonBody (Encode.object [("flakes", Encode.int count)])
        , expect = Http.expectWhatever SavedFlakes
        , timeout = Nothing
        , tracker = Nothing
        }


isFlakeCaught : Model -> Snowflake.Position -> Bool
isFlakeCaught model flakePosition =
    let
        tongueX = model.playerX
        tongueY = -850
        position = Snowflake.animatePosition model.clock flakePosition
        withinDistance a b = compare (abs (a - b)) (headRadius model.score) == LT
    in
        withinDistance tongueX position.x && withinDistance tongueY position.y


-- VIEW

view : Model -> List (Html Msg)
view model =
    [ Html.node "link" [ Html.Attributes.rel "stylesheet", Html.Attributes.href ("https://fonts.googleapis.com/css?family=" ++ config.font)] []
    , Layout.impose (snowflakes model) (Layout.align Layout.topLeft background)
        |> Layout.align Layout.base
        |> Layout.at Layout.base (message model)
        |> Layout.at Layout.topLeft (score model |> shift (50, -50))
        |> Layout.align Layout.bottomLeft |> Layout.impose (snowman model.score |> shiftY 60 |> shiftX model.playerX |> scale 0.5)
        |> Layout.align Layout.bottomLeft |> Layout.impose snowdrifts
        |> Layout.align Layout.bottomRight |> Layout.impose (html (820, 40) info)
        |> Events.onMouseMove MouseMove
        |> svg
    ]

background =
    rectangle config.sceneWidth config.sceneHeight
        |> filled (uniform Color.lightBlue)

snowflakes : Model -> Collage msg
snowflakes model =
    model.snowflakePositions
        |> List.map (Snowflake.animatePosition model.clock)
        |> List.map (\p -> Snowflake.snowflake |> shiftX p.x |> shiftY p.y |> rotate p.rotation)
        |> group

snowdrifts : Collage msg
snowdrifts =
    [ ellipse 150 30
    , ellipse 140 50
    , ellipse 200 40
    , ellipse 150 50
    , ellipse 150 40
    , ellipse 150 30
    ]
    |> List.map (filled (uniform Color.lightGrey))
    |> List.indexedMap (\i e -> shiftX ((toFloat i) * 200) e)
    |> group


customStyle =
    Text.style
        { color = Color.white
        , typeface = (Text.Font config.font)
        , size = 100
        , shape = Text.Upright
        , weight = Text.Regular
        , line = Text.None
        }


message : Model -> Collage Msg
message model =
    case model.state of
        Unstarted ->
            happyHolidays

        Playing startTime ->
            Text.empty |> rendered

        Won points ->
            youWon points model.totalFlakes


happyHolidays : Collage Msg
happyHolidays =
    [ Text.fromString "Happy Holidays"
        |> customStyle
        |> rendered
    , Layout.spacer 0 30
    , Text.fromString "Click to catch some flakes"
        |> customStyle
        |> Text.size 30
        |> rendered
    ] |> Layout.vertical
    |> Events.onClick StartGame


youWon : Int -> Maybe Int -> Collage Msg
youWon points totalFlakes =
    [ Text.fromString "Nice!"
        |> customStyle
        |> rendered
    , Text.fromString ("You caught " ++ (String.fromInt points) ++ " flakes")
        |> customStyle
        |> Text.size 30
        |> rendered
    , Layout.spacer 0 10
    , case totalFlakes of
        Just flakes ->
            Text.fromString ("The Hungry Snowman has caught " ++ (String.fromInt (flakes + points)) ++ " flakes")
                |> customStyle
                |> Text.size 30
                |> rendered

        Nothing ->
            Layout.spacer 0 0

    , Layout.spacer 0 10
    , Text.fromString "Click to play again"
        |> customStyle
        |> Text.size 30
        |> rendered
    ] |> Layout.vertical
    |> Events.onClick StartGame


score : Model -> Collage Msg
score model =
    case model.state of
        Playing startTime ->
            [Text.fromString "⌛︎"
                |> customStyle
                |> Text.size 50
                |> rendered
            ,Text.fromString ((String.fromInt <| round <| (config.gameLengthSeconds - (model.clock - startTime) / 1000) ))
                |> customStyle
                |> Text.size 50
                |> rendered
            , Layout.spacer 40 0
            , Text.fromString "❄"
                |> customStyle
                |> Text.size 50
                |> rendered
            , Layout.spacer 15 0
            , Text.fromString (String.fromInt model.score)
                |> customStyle
                |> Text.size 50
                |> rendered
            ] |> Layout.horizontal

        _ ->
            Text.empty |> rendered


headRadius : Int -> Float
headRadius currentScore =
    30 -- + (toFloat currentScore) / 2


snowman : Int -> Collage msg
snowman currentScore =
    let
        whiteCircle size = circle size |> (filled (uniform Color.white))
        nose = triangle 20
            |> (filled (uniform Color.orange))
            |> rotate (degrees 30)
            |> scaleX 3
        eye = circle 5
            |> filled (uniform Color.white)
    in
    [ whiteCircle 90
    , whiteCircle 75
    , whiteCircle (headRadius currentScore * 2)
        |> Layout.at Layout.right (nose |> shiftX 10)
        |> Layout.at Layout.base (eye |> shiftY 25)
    ]
    |> List.indexedMap (\i e -> shiftY ((toFloat i) * 90) e)
    |> group


info : Html Msg
info =
    Html.a
        [ Html.Attributes.href "https://www.nature.org/"
        , Html.Attributes.target "_blank"
        , Html.Attributes.style "color" "grey"
        , Html.Attributes.style "text-decoration" "none"
        , Html.Attributes.style "font-family" "sans-serif"
        , Html.Attributes.style "font-size" "12px"
        ]  [ Html.text "Like The Hungry Snowman? Consider donating to The Nature Conservancy." ]
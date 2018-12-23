import Browser
import Browser.Events
import Browser.Dom exposing (getViewport, Viewport)
import Html exposing (Html)
import Html.Attributes
import Time exposing (Posix)
import Task
import Random

import Collage exposing (..)
import Collage.Layout as Layout
import Collage.Render exposing (svg)
import Collage.Text as Text
import Collage.Events as Events

import Color
import Animation as A

import Snowflake
import Config exposing (config)
import Pointer exposing (Pointer, onPointerMove)


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
        1000
        1000

init : () -> (Model, Cmd Msg)
init _ =
  (model0, Task.perform GotViewport getViewport)

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
  , sceneHeight : Float
  , sceneWidth : Float
  }

type Msg
    = Tick Float
    | NewRandom (Float, Float)
    | NewSnowflake Posix
    | PointerMove Pointer
    | GotViewport Viewport
    | StartGame

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
                        }, Cmd.none )

                _ ->
                    ({ model | clock = model.clock + time }, Cmd.none )

        NewRandom (x, y) ->
            ( { model | randomness = (x, y) }, Cmd.none )

        NewSnowflake clock ->
            let
                position = Snowflake.initPosition model.clock model.randomness model.sceneWidth model.sceneHeight
            in
               ({ model | snowflakePositions = position :: (List.take config.maxSnowflakes model.snowflakePositions) }
               , Random.generate NewRandom (Random.pair (Random.float 0 1) (Random.float 0 1))
               )

        PointerMove pointer ->
            case model.state of
                Playing startTime ->
                    ( { model | playerX = pointer.x }, Cmd.none )

                _ ->
                    ( model, Cmd.none )

        GotViewport viewport ->
            let
                width = max 700 viewport.scene.width
                height = max 700 viewport.scene.height
            in
                ( { model | sceneHeight = height, sceneWidth = width }, Cmd.none )

        StartGame ->
            case model.state of
                Unstarted ->
                    ( { model | state = Playing model.clock, score = 0 }, Cmd.none )

                Won _ ->
                    ( { model | state = Playing model.clock, score = 0 }, Cmd.none )

                Playing _ ->
                    ( model, Cmd.none )


isFlakeCaught : Model -> Snowflake.Position -> Bool
isFlakeCaught model flakePosition =
    let
        tongueX = model.playerX
        tongueY = 0 - model.sceneHeight + snowmanHeight
        position = Snowflake.animatePosition model.clock flakePosition
        withinDistance a b = compare (abs (a - b)) headRadius == LT
    in
        withinDistance tongueX position.x && withinDistance tongueY position.y


-- VIEW

view : Model -> List (Html Msg)
view model =
    [ Html.div
        [ onPointerMove PointerMove
        , Html.Attributes.style "touch-action" "none"
        , Html.Attributes.style "position" "fixed"
        ]
        [ Layout.impose (snowflakes model) (Layout.align Layout.topLeft (background model.sceneWidth model.sceneHeight))
            |> Layout.align Layout.base
            |> Layout.at Layout.base (message model)
            |> Layout.at Layout.topLeft (score model |> shift (50, -50))
            |> Layout.align Layout.bottomLeft |> Layout.impose (snowman model.score |> shiftY 160 |> shiftX model.playerX |> scale 0.5)
            |> Layout.align Layout.bottomLeft |> Layout.impose (snowdrifts model.sceneWidth |> shiftY 100)
            |> Events.onClick StartGame
            |> svg
        ]
    ]

background width height =
    rectangle width height
        |> filled (uniform Color.lightBlue)

snowflakes : Model -> Collage msg
snowflakes model =
    model.snowflakePositions
        |> List.map (Snowflake.animatePosition model.clock)
        |> List.map (\p -> Snowflake.snowflake |> shiftX p.x |> shiftY p.y |> rotate p.rotation)
        |> group

snowdrifts : Float -> Collage msg
snowdrifts width =
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
    |> scaleX (width / 1000)
    |> Layout.at Layout.bottom
        ( rectangle width 100
            |> (filled (uniform Color.lightGrey))
            |> (Layout.align Layout.left)
        )


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


headRadius : Float
headRadius =
    30

snowmanHeight : Float
snowmanHeight =
    250

snowman : Int -> Collage msg
snowman currentScore =
    let
        whiteCircle size = circle size |> (filled (uniform Color.white))
        nose = triangle 20
            |> (filled (uniform Color.orange))
            |> rotate (degrees 30)
            |> scaleX 3
    in
    [ whiteCircle 90
    , whiteCircle 75
    , whiteCircle (headRadius * 2)
        |> Layout.at Layout.right (nose |> shiftX 10)
    ]
    |> List.indexedMap (\i e -> shiftY ((toFloat i) * 90) e)
    |> group
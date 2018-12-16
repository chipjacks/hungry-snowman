import Browser
import Browser.Events
import Html exposing (Html)
import Time exposing (Posix)
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


-- INIT

main = Browser.element
    { init = init
    , update = update
    , view = view
    , subscriptions = subscriptions
    } 

model0 =
    Model 0
        []
        (0, 0)
        0
        0

init : () -> (Model, Cmd Msg)
init _ =
  (model0 , Cmd.none)

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
  }

type Msg
    = Tick Float
    | NewRandom (Float, Float)
    | NewSnowflake Posix
    | MouseMove Point


-- UPDATE

update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        Tick time ->
            let
                (caught, uncaught) = List.partition (isFlakeCaught model) model.snowflakePositions
            in
                ( { model | clock = model.clock + time, score = model.score + List.length caught, snowflakePositions = uncaught }, Cmd.none)
        
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
            ( { model | playerX = Tuple.first point }, Cmd.none )


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

view : Model -> Html Msg
view model =
    rectangle config.sceneWidth config.sceneHeight
        |> filled (uniform Color.lightBlue)
--        |> Layout.align Layout.base |> Layout.impose happyHolidays
        |> Layout.align Layout.topRight |> Layout.impose (score model.score |> shift (-50, -50))
        |> Layout.align Layout.bottomLeft |> Layout.impose (snowman model.score |> shiftY 60 |> shiftX model.playerX |> scale 0.5)
        |> Layout.align Layout.bottomRight |> Layout.impose (tree |> shiftX -100)
        --|> Layout.align Layout.bottomLeft |> Layout.impose (snowman |> shiftX 100 |> shiftY 50 |> scale 0.8)
        |> Layout.align Layout.topLeft |> Layout.impose (snowflakes model)
        |> Layout.align Layout.bottomLeft |> Layout.impose snowdrifts
        |> Events.onMouseMove MouseMove
        |> svg


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
    , ellipse 200 70
    , ellipse 150 50
    , ellipse 150 60
    , ellipse 150 30
    ]
    |> List.map (filled (uniform Color.white))
    |> List.indexedMap (\i e -> shiftX ((toFloat i) * 200) e)
    |> group


happyHolidays : Collage msg
happyHolidays =
    Text.fromString "Happy Holidays"
        |> Text.size 100
        |> Text.typeface (Text.Font "Hobo Std")
        |> rendered


score : Int -> Collage msg
score points =
    Text.fromString (String.fromInt points)
        |> Text.size 50
        |> Text.typeface (Text.Font "Hobo Std")
        |> Text.color Color.white
        |> rendered


tree : Collage msg
tree =
    rectangle 30 300
        |> filled (uniform Color.darkBrown)
        |> Layout.at Layout.top (triangle 150 |> filled (uniform Color.darkGreen) |> scaleY 1.5 |> shiftY 20)
        |> Layout.at Layout.bottom (present |> shiftY 190 |> scale 0.6 |> shiftX 40)


headRadius : Int -> Float
headRadius currentScore =
    30 + (toFloat currentScore) / 2


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


present : Collage msg
present =
    let
        bow = [45, 90, 135] |> List.map (\d -> ellipse 20 10 |> filled (uniform Color.darkYellow) |> rotate (degrees d)) |> group
    in
        roundedRectangle 70 50 5
            |> filled (uniform Color.darkRed)
            |> Layout.impose (rectangle 10 50 |> filled (uniform Color.darkYellow))
            |> Layout.at Layout.top bow
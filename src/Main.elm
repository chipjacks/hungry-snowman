import Browser
import Browser.Events
import Html exposing (Html)
import Time exposing (Posix)
import Random

import Collage exposing (..)
import Collage.Layout as Layout
import Collage.Render exposing (svg)
import Collage.Text as Text

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
  }

type Msg
    = Tick Float
    | NewRandom (Float, Float)
    | NewSnowflake Posix


-- UPDATE

update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        Tick time ->
            ( { model | clock = model.clock + time }, Cmd.none)
        
        NewRandom (x, y) ->
            ( { model | randomness = (x, y) }, Cmd.none )
        
        NewSnowflake clock ->
            let
                position = Snowflake.initPosition model.clock model.randomness
            in
               ({ model | snowflakePositions = position :: (List.take config.maxSnowflakes model.snowflakePositions) }
               , Random.generate NewRandom (Random.pair (Random.float 0 1) (Random.float 0 1))
               )


-- VIEW

view : Model -> Html msg
view model =
    rectangle config.sceneWidth config.sceneHeight
        |> filled (uniform Color.lightBlue)
        |> Layout.align Layout.base |> Layout.impose merryChristmas
        |> Layout.align Layout.bottomRight |> Layout.impose (tree |> shiftX -100)
        |> Layout.align Layout.bottomLeft |> Layout.impose (snowman |> shiftX 100 |> shiftY 50 |> scale 0.8)
        |> Layout.at Layout.topLeft (snowflakes model)
        |> Layout.align Layout.bottomLeft |> Layout.impose snowdrifts
        |> svg


snowflakes : Model -> Collage msg
snowflakes model =
    model.snowflakePositions
        |> List.map (\p -> Snowflake.snowflake |> Snowflake.animatePosition p model.clock)
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


merryChristmas : Collage msg
merryChristmas =
    Text.fromString "Merry Christmas"
        |> Text.size 100
        |> Text.typeface (Text.Font "Hobo Std")
        |> rendered


tree : Collage msg
tree =
    rectangle 30 300
        |> filled (uniform Color.darkBrown)
        |> Layout.at Layout.top (triangle 150 |> filled (uniform Color.darkGreen) |> scaleY 1.5 |> shiftY 20)


snowman : Collage msg
snowman =
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
    , whiteCircle 60 |> Layout.at Layout.right (nose |> shiftX 10) |> Layout.at Layout.base (eye |> shiftY 25)
    ]
    |> List.indexedMap (\i e -> shiftY ((toFloat i) * 90) e)
    |> group
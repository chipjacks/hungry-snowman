import Browser
import Browser.Events
import Html exposing (Html)
import Time exposing (Posix)
import Random

import Collage exposing (..)
import Collage.Layout as Layout
import Collage.Render exposing (svg)

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
        |> Layout.at Layout.topLeft (snowflakes model)
        |> svg


snowflakes : Model -> Collage msg
snowflakes model =
    model.snowflakePositions
        |> List.map (\p -> Snowflake.snowflake |> Snowflake.animatePosition p model.clock)
        |> group 
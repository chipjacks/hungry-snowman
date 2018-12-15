
import Browser
import Browser.Events
import Html exposing (Html)
import Time exposing (Posix)

import Collage exposing (..)
import Collage.Layout as Layout
import Collage.Render exposing (svg)

import Color
import Animation as A

main = Browser.element
    { init = init
    , update = \msg model -> ( update msg model, Cmd.none )
    , view = view
    , subscriptions = (\m -> Browser.Events.onAnimationFrameDelta Tick)
    } 

type alias Model =
  { clock : A.Clock
  , snowflakeYs : List A.Animation
  }

second : Float
second =
    1000

model0 =
    Model 0
        (List.range 0 10 |> List.map (\t -> t * 500 |> toFloat) |> List.map snowflakeRowAnimation)

type Msg
    = Tick Float

init : () -> (Model, Cmd Msg)
init _ =
  (model0 , Cmd.none)


update : Msg -> Model -> Model
update msg model =
    case msg of
        Tick time ->
            let
                clock = model.clock + time
                animations = model.snowflakeYs |> List.map (loop clock)
            in
                { model | clock = clock, snowflakeYs = animations }


loop : A.Clock -> A.Animation -> A.Animation
loop t a = if A.isDone t a then snowflakeRowAnimation t else a


snowflakeRowAnimation : A.Clock -> A.Animation
snowflakeRowAnimation startTime =
    A.animation startTime |> A.duration 5000 |> A.from 0 |> A.to -1000


snowflakeRows : List A.Animation -> A.Clock -> Collage msg
snowflakeRows animations time =
    let
        snowflake =
            [ 45, 90, 135, 180 ]
            |> List.map (\d -> line 10 |> traced (solid thin (uniform Color.white)) |> rotate d)
            |> group

        row =
            List.repeat 10 snowflake
                |> List.intersperse (Layout.spacer 100 0)
                |> Layout.horizontal
                |> Layout.align Layout.base

        rows = List.map (\animation -> row |> shiftY (A.animate time animation)) animations
    in
        group rows
    


view : Model -> Html msg
view model =
    let
        ground =
            Collage.ellipse 700 100
                |> filled (uniform Color.darkGreen)
                |> Layout.align Layout.base

        sky =
            rectangle 1000 1000
                |> filled (uniform Color.lightBlue)
                |> Layout.at Layout.top (snowflakeRows model.snowflakeYs model.clock)
                |> Layout.align Layout.bottom
        
    in
        Layout.impose ground sky
        |> svg
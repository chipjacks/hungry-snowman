module Snowflake exposing (..)

import Collage exposing (..)
import Animation as A
import Color

import Config exposing (config)

type alias Position =
    { x : A.Animation
    , y : A.Animation
    , rotation : A.Animation
    }

snowflake : Collage msg
snowflake =
    [ 45, 90, 135, 180 ]
    |> List.map (\d -> line 10 |> traced (solid thin (uniform Color.white)) |> rotate d)
    |> group

initPosition : A.Clock -> (Float, Float) -> Position
initPosition startTime (xRand, yRand) =
    let
        duration = yRand * 5000 + 10000
    in
        { y = A.animation startTime |> A.duration duration |> A.from 5 |> A.to (0 - config.sceneHeight + 5)
        , x = A.static (5 + (config.sceneWidth - 10) * xRand)
        , rotation = A.animation startTime |> A.duration duration |> A.from 0 |> A.to 10
        }

animatePosition : Position -> A.Clock -> Collage msg -> Collage msg
animatePosition position clock flake =
    flake |> shiftX (position.x |> A.animate clock) |> shiftY (position.y |> A.animate clock) |> rotate (position.rotation |> A.animate clock)
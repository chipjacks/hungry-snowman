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

initPosition : A.Clock -> (Float, Float) -> Float -> Position
initPosition startTime (xRand, yRand) sceneHeight =
    let
        duration = yRand * 5000 + 10000
    in
        { y = A.animation startTime |> A.duration duration |> A.from 5 |> A.to (0 - sceneHeight + 5)
        , x = A.static (5 + (config.sceneWidth - 10) * xRand)
        , rotation = A.animation startTime |> A.duration duration |> A.from 0 |> A.to 10
        }

animatePosition : A.Clock -> Position -> { x : Float, y : Float, rotation : Float }
animatePosition clock position =
    { x = (position.x |> A.animate clock)
    , y = (position.y |> A.animate clock)
    , rotation = (position.rotation |> A.animate clock)
    }
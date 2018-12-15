module Snowflake exposing (..)

import Collage exposing (..)
import Animation as A
import Color

type alias Position =
    { x : A.Animation
    , y : A.Animation
    }

snowflake : Collage msg
snowflake =
    [ 45, 90, 135, 180 ]
    |> List.map (\d -> line 10 |> traced (solid thin (uniform Color.white)) |> rotate d)
    |> group

initPosition : A.Clock -> (Float, Float) -> Position
initPosition startTime (xRand, yRand) =
    { y = A.animation startTime |> A.duration (yRand * 5000 + 3000) |> A.from 5 |> A.to -995
    , x = A.static (990 * xRand + 5) 
    }

animatePosition : Position -> A.Clock -> Collage msg -> Collage msg
animatePosition position clock flake =
    flake |> shiftX (position.x |> A.animate clock) |> shiftY (position.y |> A.animate clock)
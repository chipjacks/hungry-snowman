module Pointer exposing (Pointer, onPointerMove)

import Html exposing (Attribute)
import Html.Events exposing (on)
import Json.Decode as Json exposing (Decoder, field, float)


type alias Pointer =
    { x: Float
    , y: Float
    }

onPointerMove : (Pointer -> msg) -> Html.Attribute msg
onPointerMove msg =
    on "pointermove" (pointerDecoder |> Json.andThen (Json.succeed << msg))

pointerDecoder : Decoder Pointer
pointerDecoder =
    Json.map2 Pointer
        (field "x" float)
        (field "y" float)
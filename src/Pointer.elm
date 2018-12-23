module Pointer exposing (Pointer, onPointerMove)

import Html exposing (Attribute)
import Html.Events exposing (preventDefaultOn)
import Json.Decode as Json exposing (Decoder, field, float)


type alias Pointer =
    { x: Float
    , y: Float
    }


onPointerMove : (Pointer -> msg) -> Html.Attribute msg
onPointerMove msg =
    preventDefaultOn "pointermove" (Json.map alwaysPreventDefault (pointerDecoder |> Json.andThen (Json.succeed << msg)))


pointerDecoder : Decoder Pointer
pointerDecoder =
    Json.map2 Pointer
        (field "x" float)
        (field "y" float)


alwaysPreventDefault : msg -> ( msg, Bool )
alwaysPreventDefault msg =
  ( msg, True )
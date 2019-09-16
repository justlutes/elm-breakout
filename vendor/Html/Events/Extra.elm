module Html.Events.Extra exposing (keyboard, onKeyDown)

import Html exposing (Attribute)
import Html.Events as Events
import Json.Decode as Decode exposing (Decoder)


onKeyDown : List (Decoder msg) -> Attribute msg
onKeyDown decoders =
    Events.on "keydown" (Decode.oneOf decoders)


keyboard :
    { left : msg -> Decoder msg
    , right : msg -> Decoder msg
    , escape : msg -> Decoder msg
    , space : msg -> Decoder msg
    , enter : msg -> Decoder msg
    }
keyboard =
    { left = onKeyCodePressed 37
    , right = onKeyCodePressed 39
    , escape = onKeyCodePressed 27
    , space = onKeyCodePressed 32
    , enter = onKeyCodePressed 13
    }


onKeyCodePressed : Int -> msg -> Decoder msg
onKeyCodePressed key msg =
    Decode.andThen (isKey key msg) Events.keyCode


isKey : Int -> msg -> Int -> Decoder msg
isKey testKey msg keyCode =
    if keyCode == testKey then
        Decode.succeed msg

    else
        Decode.fail ""

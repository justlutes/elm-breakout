module Paddle exposing (Model(..), normal, short, traction, view, width)

import Svg exposing (Svg)
import Svg.Attributes as Attributes


type Model
    = Normal PaddleSize
    | Short PaddleSize


type alias PaddleSize =
    { left : Int
    , leftMiddle : Int
    , middle : Int
    , rightMiddle : Int
    , right : Int
    , traction : Float
    }


traction : Model -> Float
traction paddle =
    (case paddle of
        Normal size ->
            size

        Short size ->
            size
    )
        |> (\s -> s.traction)


width : Model -> Int
width paddle =
    case paddle of
        Normal size ->
            size.right + 4

        Short size ->
            size.right + 2


normal : Model
normal =
    Normal <| PaddleSize 0 24 32 64 128 0.55


short : Model
short =
    Short <| PaddleSize 0 2 4 6 8 0.55


view : Int -> Float -> Model -> Svg msg
view positionY positionX paddle =
    let
        height =
            15
    in
    Svg.rect
        [ case paddle of
            Normal size ->
                Attributes.width (String.fromInt (size.right + 4))

            Short size ->
                Attributes.width (String.fromInt (size.right + 2))
        , Attributes.height <| String.fromInt height
        , Attributes.fill "#C64947"
        , Attributes.x <| String.fromFloat positionX
        , Attributes.y <| String.fromInt (positionY - height)
        ]
        []

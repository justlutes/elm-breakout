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
    Normal <| PaddleSize 0 4 8 12 16 0.55


short : Model
short =
    Short <| PaddleSize 0 2 4 6 8 0.55


view : Float -> Model -> Svg msg
view positionX paddle =
    Svg.rect
        [ case paddle of
            Normal size ->
                Attributes.width (String.fromInt (size.right + 4))

            Short size ->
                Attributes.width (String.fromInt (size.right + 2))
        , Attributes.height "1.5"
        , Attributes.fill "#C64947"
        , Attributes.x <| String.fromFloat positionX
        , Attributes.y "75.5"
        ]
        []

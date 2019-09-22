module Game.Life exposing (view)

import Svg exposing (Svg)
import Svg.Attributes as Attributes


view : Int -> Int -> Int -> List (Svg msg)
view lives height containerWidth =
    let
        width =
            (containerWidth // 5) // lives
    in
    List.range 1 lives
        |> List.map
            (\i ->
                Svg.rect
                    [ Attributes.height <| String.fromInt (height // 2)
                    , Attributes.width <| String.fromInt width
                    , Attributes.y <| String.fromInt (height // 2)
                    , Attributes.x <|
                        String.fromInt
                            (containerWidth - (width * i + (width // 2 * i)))
                    , Attributes.rx "0.5"
                    , Attributes.fill "#C64947"
                    ]
                    []
            )

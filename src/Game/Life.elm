module Game.Life exposing (view)

import Svg exposing (Svg)
import Svg.Attributes as Attributes


view : Int -> Float -> Float -> List (Svg msg)
view lives containerHeight containerWidth =
    let
        height =
            containerHeight * 0.16

        width =
            (containerWidth / 5) / toFloat lives
    in
    List.range 1 lives
        |> List.map
            (\i ->
                Svg.rect
                    [ Attributes.height <| String.fromFloat height
                    , Attributes.width <| String.fromFloat width
                    , Attributes.y <| String.fromFloat (containerHeight / 2 - (height / 2))
                    , Attributes.x <|
                        String.fromFloat
                            (containerWidth - (width * toFloat i + (width / 2 * toFloat i)))
                    , Attributes.rx "1.25"
                    , Attributes.fill "#C64947"
                    ]
                    []
            )

module Data.Cell exposing (Cell(..), color, size, stroke)

import Game.Ball
import Game.Brick
import Game.Paddle


type Cell
    = Empty
    | Ball Game.Ball.Model
    | Paddle Game.Paddle.Model
    | Brick Game.Brick.Model


stroke : Cell -> String
stroke cell =
    case cell of
        Brick _ ->
            "rgb(237, 232, 225)"

        Paddle model ->
            model.fill

        Ball model ->
            model.fill

        Empty ->
            "#1760b9"


color : Cell -> String
color cell =
    case cell of
        Ball model ->
            model.fill

        Paddle model ->
            model.fill

        Brick model ->
            model.fill

        Empty ->
            "none"


size : Int
size =
    1

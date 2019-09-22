module Data.Level exposing (Level, bricks, empty, lives, paddleWidth, random, speed)

import Random exposing (Generator)


type alias Level =
    { lives : Int
    , paddleWidth : Int
    , speed : Float
    , bricks : List (List Int)
    }


lives : Level -> Int
lives level =
    level.lives


paddleWidth : Level -> Int
paddleWidth level =
    level.paddleWidth


speed : Level -> Float
speed level =
    level.speed


bricks : Level -> List (List Int)
bricks level =
    level.bricks


empty : Level
empty =
    { lives = 0
    , paddleWidth = 0
    , speed = 0
    , bricks = []
    }


random : Int -> Generator Level
random columns =
    Random.map4
        Level
        randLiveGen
        randPaddleGen
        randSpeedGen
        (randBrickGen columns)


randLiveGen : Generator Int
randLiveGen =
    Random.int 2 5


randPaddleGen : Generator Int
randPaddleGen =
    Random.int 3 7
        |> Random.andThen
            (\i ->
                if modBy 2 i == 0 then
                    Random.constant (i + 1)

                else
                    Random.constant i
            )


randSpeedGen : Generator Float
randSpeedGen =
    Random.float 0 2


randBrickGen : Int -> Generator (List (List Int))
randBrickGen columns =
    Random.int 2 5
        |> Random.andThen
            (\rows ->
                let
                    rowGenerator =
                        Random.list (columns // 2) (Random.int 1 3)
                in
                Random.list rows rowGenerator
            )

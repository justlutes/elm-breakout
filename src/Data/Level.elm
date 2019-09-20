module Data.Level exposing (Level, bricks, create, empty, lives, paddleWidth, speed)

import Random exposing (Generator)


type alias Level =
    { lives : Int
    , paddleWidth : Float
    , speed : Int
    , bricks : List (List Int)
    }


lives : Level -> Int
lives level =
    level.lives


paddleWidth : Level -> Float
paddleWidth level =
    level.paddleWidth


speed : Level -> Int
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


create : Generator Level
create =
    Random.map4
        Level
        randLiveGen
        randPaddleGen
        randSpeedGen
        randBrickGen


randLiveGen : Generator Int
randLiveGen =
    Random.int 2 5


randPaddleGen : Generator Float
randPaddleGen =
    Random.float 0.25 1


randSpeedGen : Generator Int
randSpeedGen =
    Random.int 1 3


randBrickGen : Generator (List (List Int))
randBrickGen =
    Random.int 4 7
        |> Random.andThen
            (\rows ->
                Random.int 4 6
                    |> Random.andThen
                        (\columns ->
                            let
                                rowGenerator =
                                    Random.list rows (Random.int 1 3)
                            in
                            Random.list columns rowGenerator
                        )
            )

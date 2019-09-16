module Main exposing (init, update)

import Ball
import Brick
import Browser
import Browser.Events
import Data.Vector2 exposing (Vector2)
import Html exposing (Html)
import Html.Attributes as Attributes
import Html.Events.Extra as Events
import Json.Decode as Decode exposing (Decoder)
import Paddle
import Random
import Svg
import Svg.Attributes as SvgAttributes
import Time exposing (Posix)



---- MODEL ----


type alias Model =
    { player : Player
    , ballPosition : Vector2
    , ballVelocity : Vector2
    , ballRadius : Int
    , state : GameState
    , paddlePosition : Float
    , paddleVelocity : Float
    , paddle : Paddle.Model
    , bricks : List Brick.Model
    , controls : Controls
    }


type GameState
    = StartScreen
    | Playing
    | Won
    | Lost


type alias Player =
    { score : Int
    , lives : Int
    }


type BallMovement
    = Moving
    | Stopped


type alias Controls =
    { left : Bool
    , right : Bool
    , movement : BallMovement
    }


init : ( Model, Cmd Msg )
init =
    ( initModel
    , Random.generate RowResult (Random.int 4 9)
    )


initModel : Model
initModel =
    { player = Player 0 5
    , ballPosition = { x = 40, y = initBallPosition }
    , ballVelocity = { x = initVelocity, y = initVelocity }
    , ballRadius = 2
    , state = StartScreen
    , paddle = Paddle.normal
    , paddlePosition = 40
    , paddleVelocity = 110
    , bricks = Brick.init 0
    , controls = initControls
    }


initBallPosition : Float
initBallPosition =
    73.5


initVelocity : Float
initVelocity =
    -0.04


initControls : Controls
initControls =
    { left = False
    , right = False
    , movement = Stopped
    }



---- UPDATE ----


type Msg
    = Tick Float
    | GameInput ControlState
    | RowResult Int
    | NoOp


type ControlState
    = PaddleLeft
    | PaddleRight
    | LaunchAndPause


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        Tick delta ->
            case model.state of
                Playing ->
                    ( model, Cmd.none )

                StartScreen ->
                    let
                        newPaddlePosition =
                            movePaddle model delta

                        newBallPosition =
                            newPaddlePosition + (toFloat (Paddle.width model.paddle) / 2)
                    in
                    ( { model
                        | paddlePosition = newPaddlePosition
                        , ballPosition =
                            { x = newBallPosition
                            , y = model.ballPosition.y
                            }
                      }
                    , Cmd.none
                    )

                _ ->
                    ( model, Cmd.none )

        RowResult rows ->
            ( { model | bricks = Brick.init rows }, Cmd.none )

        GameInput input ->
            case model.state of
                Playing ->
                    case input of
                        PaddleLeft ->
                            ( { model
                                | controls =
                                    { left = True
                                    , right = False
                                    , movement = Stopped
                                    }
                              }
                            , Cmd.none
                            )

                        PaddleRight ->
                            ( { model
                                | controls =
                                    { left = True
                                    , right = False
                                    , movement = Stopped
                                    }
                              }
                            , Cmd.none
                            )

                        LaunchAndPause ->
                            ( { model
                                | controls = { left = False, right = False, movement = Stopped }
                              }
                            , Cmd.none
                            )

                StartScreen ->
                    case input of
                        PaddleLeft ->
                            ( { model
                                | controls =
                                    { left = not model.controls.left
                                    , right = False
                                    , movement = Stopped
                                    }
                              }
                            , Cmd.none
                            )

                        PaddleRight ->
                            ( { model
                                | controls =
                                    { left = False
                                    , right = not model.controls.right
                                    , movement = Stopped
                                    }
                              }
                            , Cmd.none
                            )

                        LaunchAndPause ->
                            ( serve model
                            , Cmd.none
                            )

                _ ->
                    ( model, Cmd.none )

        NoOp ->
            ( model, Cmd.none )


movePaddle : Model -> Float -> Float
movePaddle { paddlePosition, paddleVelocity, controls } delta =
    let
        newPos =
            if controls.left then
                paddlePosition - 1 * paddleVelocity * delta

            else if controls.right then
                paddlePosition + 1 * paddleVelocity * delta

            else
                paddlePosition
    in
    if 0 < newPos && newPos <= 80 then
        newPos

    else
        paddlePosition


serve : Model -> Model
serve model =
    let
        serveSpeed =
            200

        traction =
            model.paddle
                |> Paddle.traction
    in
    { model
        | controls =
            { left = False
            , right = False
            , movement = Moving
            }
        , state = Playing
        , ballVelocity = { x = traction * model.paddlePosition, y = serveSpeed }
    }



---- VIEW ----


view : Model -> Html Msg
view model =
    Html.div
        [ Attributes.class "container" ]
        [ header model
        , Html.div
            [ Attributes.class "game-container"
            ]
            [ board model
            , case model.state of
                StartScreen ->
                    Html.div [] []

                Playing ->
                    Html.div [] []

                Won ->
                    Html.div [ Attributes.class "game-content" ]
                        [ Html.p [] [ Html.text "Winner" ] ]

                Lost ->
                    Html.div [ Attributes.class "game-content" ]
                        [ Html.p [] [ Html.text "Enter to reset" ] ]
            ]
        , Html.footer [] []
        ]


header : Model -> Html Msg
header model =
    Html.header [] [ Html.text "header" ]


board : Model -> Html Msg
board model =
    case model.state of
        StartScreen ->
            Html.div [ Attributes.class "game-overlay" ]
                [ Html.div [ Attributes.class "start-title" ]
                    [ Html.text "Space to launch the ball" ]
                , displayGameBoard model
                ]

        Playing ->
            displayGameBoard model

        _ ->
            Html.div [] []


displayGameBoard : Model -> Html Msg
displayGameBoard model =
    Svg.svg
        [ SvgAttributes.width "100%"
        , SvgAttributes.height "100%"
        , SvgAttributes.viewBox "0 0 100 77"
        , SvgAttributes.fill "#000000"
        ]
    <|
        (Brick.view model.bricks
            ++ [ Ball.view model.ballPosition model.ballRadius
               , Paddle.view model.paddlePosition model.paddle
               ]
        )


subscriptions : Model -> Sub Msg
subscriptions _ =
    Sub.batch
        [ Browser.Events.onKeyDown keyDecoder
        , Browser.Events.onKeyUp keyDecoder
        , Browser.Events.onAnimationFrameDelta (\dt -> Tick (dt / 1000))
        ]


keyDecoder : Decoder Msg
keyDecoder =
    Decode.map toInput (Decode.field "key" Decode.string)


toInput : String -> Msg
toInput string =
    case string of
        "ArrowLeft" ->
            GameInput PaddleLeft

        "ArrowRight" ->
            GameInput PaddleRight

        " " ->
            GameInput LaunchAndPause

        _ ->
            NoOp



---- PROGRAM ----


main : Program () Model Msg
main =
    Browser.element
        { view = view
        , init = \_ -> init
        , update = update
        , subscriptions = subscriptions
        }

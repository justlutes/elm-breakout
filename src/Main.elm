module Main exposing (init, main, update)

import Browser exposing (Document)
import Browser.Events
import Data.Level as Level exposing (Level)
import Data.Movement as Movement
import Data.Vector2 as Vector2 exposing (Vector2)
import Game.Ball as Ball
import Game.Brick as Brick
import Game.Life as Life
import Game.Paddle as Paddle
import Html exposing (Html)
import Html.Attributes as Attributes
import Html.Events as Events
import Json.Decode as Decode exposing (Decoder)
import Random
import Svg
import Svg.Attributes



---- MODEL ----


type alias Model =
    { score : Int
    , level : Level
    , ballPosition : Vector2
    , ballVelocity : Vector2
    , ballRadius : Float
    , state : GameState
    , paddlePosition : Vector2
    , paddle : Paddle.Model
    , bricks : List Brick.Model
    , controls : Controls
    , window : Window
    , unitsOnScreen : Float
    }


type alias Window =
    { width : Float
    , height : Float
    }


type GameState
    = StartScreen
    | Playing
    | Won
    | Lost


type BallMovement
    = Moving
    | Stopped


type alias Controls =
    { left : Bool
    , right : Bool
    , movement : BallMovement
    }


init : Flags -> ( Model, Cmd Msg )
init flags =
    ( initModel flags
    , Random.generate LevelResult Level.random
    )


initModel : Flags -> Model
initModel { window } =
    let
        width =
            window.width

        height =
            window.height

        unitsOnScreen =
            min width height
    in
    { score = 0
    , level = Level.empty
    , ballPosition = { x = 0, y = 0 }
    , ballVelocity = Movement.none
    , ballRadius = 0.025 * unitsOnScreen
    , state = StartScreen
    , paddle = { height = 0.033 * unitsOnScreen, width = 0.25 * unitsOnScreen }
    , paddlePosition = { x = 0, y = 0 }
    , bricks = Brick.init []
    , controls = initControls
    , window = { width = width, height = height }
    , unitsOnScreen = unitsOnScreen
    }


initialBallAndPaddle :
    Model
    ->
        { ballPosition : Vector2
        , paddlePosition : Vector2
        , paddle : Paddle.Model
        }
initialBallAndPaddle { level, paddle, window, unitsOnScreen } =
    let
        gameHeight =
            window.height * 0.66

        ballRadius =
            0.025 * unitsOnScreen

        paddleWidth =
            level.paddleWidth * unitsOnScreen * 0.45

        paddlePosX =
            (window.width - paddleWidth) / 2

        ballPosX =
            paddlePosX + (paddleWidth / 2)

        ballVector =
            { x = ballPosX
            , y = gameHeight - paddle.height - ballRadius
            }

        paddleVector =
            { x = paddlePosX
            , y = gameHeight - paddle.height
            }
    in
    { ballPosition = ballVector
    , paddlePosition = paddleVector
    , paddle = { height = paddle.height, width = paddleWidth }
    }


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
    | LevelResult Level
    | ResizeWindow Int Int
    | Reset
    | NoOp


type ControlState
    = PaddleLeft
    | PaddleLeftUp
    | PaddleRight
    | PaddleRightUp
    | Serve
    | Pause


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        ResizeWindow width height ->
            case model.state of
                StartScreen ->
                    ( { model
                        | window = { width = toFloat width / 2, height = toFloat height }
                        , ballPosition = { x = model.ballPosition.x, y = toFloat height - 15 - model.ballRadius }
                      }
                    , Cmd.none
                    )

                _ ->
                    ( { model
                        | window = { width = toFloat width / 2, height = toFloat height }
                      }
                    , Cmd.none
                    )

        Reset ->
            ( initModel { window = model.window }
            , Random.generate LevelResult Level.random
            )

        Tick delta ->
            case model.state of
                Playing ->
                    let
                        newModel =
                            updateGameState model delta
                    in
                    if newModel.level.lives < 1 then
                        ( { newModel | state = Lost }, Cmd.none )

                    else
                        ( newModel
                        , Cmd.none
                        )

                StartScreen ->
                    ( updateGameState model delta
                    , Cmd.none
                    )

                _ ->
                    ( model, Cmd.none )

        LevelResult level ->
            let
                modelWithLevel =
                    { model
                        | level = level
                        , bricks = Brick.init level.bricks
                    }

                { ballPosition, paddlePosition, paddle } =
                    initialBallAndPaddle modelWithLevel
            in
            ( { modelWithLevel
                | ballPosition = ballPosition
                , paddlePosition = paddlePosition
                , paddle = paddle
              }
            , Cmd.none
            )

        GameInput input ->
            case input of
                PaddleLeft ->
                    ( { model
                        | controls =
                            { left = True
                            , right = False
                            , movement = model.controls.movement
                            }
                      }
                    , Cmd.none
                    )

                PaddleLeftUp ->
                    ( { model
                        | controls =
                            { left = False
                            , right = False
                            , movement = model.controls.movement
                            }
                      }
                    , Cmd.none
                    )

                PaddleRight ->
                    ( { model
                        | controls =
                            { left = False
                            , right = True
                            , movement = model.controls.movement
                            }
                      }
                    , Cmd.none
                    )

                PaddleRightUp ->
                    ( { model
                        | controls =
                            { left = False
                            , right = False
                            , movement = model.controls.movement
                            }
                      }
                    , Cmd.none
                    )

                Pause ->
                    case model.state of
                        Playing ->
                            ( { model
                                | controls =
                                    { left = False
                                    , right = False
                                    , movement = Stopped
                                    }
                              }
                            , Cmd.none
                            )

                        StartScreen ->
                            ( model, Cmd.none )

                        _ ->
                            ( initModel { window = model.window }
                            , Random.generate LevelResult Level.random
                            )

                Serve ->
                    case model.state of
                        StartScreen ->
                            ( serve model, Cmd.none )

                        _ ->
                            ( model, Cmd.none )

        NoOp ->
            ( model, Cmd.none )


updateGameState : Model -> Float -> Model
updateGameState model delta =
    let
        distance =
            delta * 0.5 * toFloat (Level.speed model.level)

        newBallCenter =
            Vector2.scaleBy model.ballVelocity distance
                |> Vector2.add model.ballPosition

        ballBottom =
            newBallCenter.y - model.ballRadius
    in
    if ballBottom > model.window.height then
        let
            { ballPosition, paddlePosition, paddle } =
                initialBallAndPaddle model

            level =
                model.level

            updatedLevel =
                { level | lives = level.lives - 1 }
        in
        { model
            | ballPosition = ballPosition
            , paddlePosition = paddlePosition
            , paddle = paddle
            , level = updatedLevel
        }

    else
        let
            newPaddlePosition =
                movePaddle model distance

            newBallPosition =
                case model.state of
                    Playing ->
                        updateBallPosition model distance

                    _ ->
                        { x = newPaddlePosition.x + (model.paddle.width / 2)
                        , y = model.ballPosition.y
                        }
        in
        { model
            | paddlePosition = newPaddlePosition
            , ballPosition = newBallPosition
        }


updateBallPosition : Model -> Float -> Vector2
updateBallPosition model distance =
    Vector2.scaleBy model.ballVelocity distance
        |> Vector2.add model.ballPosition


movePaddle : Model -> Float -> Vector2
movePaddle model distance =
    let
        movementVector =
            if model.controls.left then
                Movement.left

            else if model.controls.right then
                Movement.right

            else
                Movement.none

        newPos =
            Vector2.scaleBy movementVector distance
                |> Vector2.add model.paddlePosition
    in
    if newPos.x < 0 then
        { newPos | x = 0 }

    else if newPos.x > (model.window.width - model.paddle.width) then
        { newPos | x = model.window.width - model.paddle.width }

    else
        newPos


serve : Model -> Model
serve model =
    let
        controls =
            model.controls

        initialVelocity =
            if controls.right then
                Vector2.normalize Movement.rightUp

            else
                Vector2.normalize Movement.leftUp
    in
    { model
        | controls =
            { left = False
            , right = False
            , movement = Moving
            }
        , state = Playing
        , ballVelocity = initialVelocity
    }



---- VIEW ----


view : Model -> Document Msg
view model =
    { title = "Elm Breakout"
    , body = [ viewBody model ]
    }


viewBody : Model -> Html Msg
viewBody model =
    Html.div
        [ Attributes.class "container" ]
        [ Html.div
            [ Attributes.class "game-container"
            ]
            [ viewHeader model
            , viewOverlay model
            , case model.state of
                StartScreen ->
                    displayGameBoard model

                Playing ->
                    displayGameBoard model

                _ ->
                    Html.text ""
            , viewFooter model
            ]
        ]


viewHeader : Model -> Html Msg
viewHeader model =
    let
        window =
            model.window

        headerHeight =
            window.height * 0.16
    in
    Svg.svg
        [ Svg.Attributes.width <| String.fromFloat window.width
        , Svg.Attributes.height <| String.fromFloat headerHeight
        ]
    <|
        [ Svg.text_
            [ Svg.Attributes.x "0"
            , Svg.Attributes.y <| String.fromFloat (headerHeight / 2 + 4)
            , Svg.Attributes.fontSize "12"
            ]
            [ Svg.text "LEVEL 1" ]
        ]
            ++ Life.view (Level.lives model.level) headerHeight window.width


viewFooter : Model -> Html Msg
viewFooter model =
    Html.footer []
        [ Html.button
            [ Events.onMouseDown (GameInput PaddleLeft) ]
            [ Html.span [] [ Html.text "⬅️" ]
            ]
        , case model.state of
            Playing ->
                Html.button
                    [ Events.onMouseDown (GameInput Pause) ]
                    [ Html.span [] [ Html.text "⏸" ]
                    ]

            _ ->
                Html.button
                    [ Events.onMouseDown (GameInput Serve) ]
                    [ Html.span [] [ Html.text "🚀" ]
                    ]
        , Html.button
            [ Events.onMouseDown Reset ]
            [ Html.span [] [ Html.text "↺" ]
            ]
        , Html.button
            [ Events.onMouseDown (GameInput PaddleRight) ]
            [ Html.span [] [ Html.text "➡️" ]
            ]
        ]


viewOverlay : Model -> Html Msg
viewOverlay model =
    case model.state of
        StartScreen ->
            Html.div
                [ Attributes.class "game-overlay"
                , Attributes.tabindex -1
                ]
                [ Html.div [ Attributes.class "start-title" ]
                    [ Html.text "Space to launch the ball" ]
                ]

        Playing ->
            Html.text ""

        Won ->
            Html.div [ Attributes.class "game-content" ]
                [ Html.p [] [ Html.text "Winner" ] ]

        Lost ->
            Html.div [ Attributes.class "game-content" ]
                [ Html.p [] [ Html.text "Escape to reset" ] ]


displayGameBoard : Model -> Html Msg
displayGameBoard model =
    let
        window =
            model.window

        numColumns =
            model.level.bricks
                |> List.map
                    (\column ->
                        List.length column
                    )
                |> List.head
                |> Maybe.withDefault 0

        height =
            window.height * 0.66
    in
    Svg.svg
        [ Svg.Attributes.width <| String.fromFloat window.width
        , Svg.Attributes.height <| String.fromFloat height
        , Svg.Attributes.class "game-board"
        ]
    <|
        Brick.view numColumns model.bricks
            ++ [ Ball.view model.ballPosition model.ballRadius
               , Paddle.view model.paddlePosition model.paddle
               ]


subscriptions : Model -> Sub Msg
subscriptions _ =
    Sub.batch
        [ Browser.Events.onKeyDown keyDecoder
        , Browser.Events.onKeyUp keyUpDecoder
        , Browser.Events.onAnimationFrameDelta Tick
        , Browser.Events.onResize ResizeWindow
        ]


keyDecoder : Decoder Msg
keyDecoder =
    Decode.field "key" Decode.string
        |> Decode.andThen
            (\key ->
                case key of
                    "ArrowLeft" ->
                        Decode.succeed (GameInput PaddleLeft)

                    "ArrowRight" ->
                        Decode.succeed (GameInput PaddleRight)

                    "ArrowUp" ->
                        Decode.succeed (GameInput Serve)

                    " " ->
                        Decode.succeed (GameInput Serve)

                    "Enter" ->
                        Decode.succeed (GameInput Pause)

                    "Escape" ->
                        Decode.succeed Reset

                    _ ->
                        Decode.fail ""
            )


keyUpDecoder : Decoder Msg
keyUpDecoder =
    Decode.field "key" Decode.string
        |> Decode.andThen
            (\key ->
                case key of
                    "ArrowLeft" ->
                        Decode.succeed (GameInput PaddleLeftUp)

                    "ArrowRight" ->
                        Decode.succeed (GameInput PaddleRightUp)

                    _ ->
                        Decode.fail ""
            )



---- PROGRAM ----


type alias Flags =
    { window : Window }


main : Program Flags Model Msg
main =
    Browser.document
        { view = view
        , init = init
        , update = update
        , subscriptions = subscriptions
        }

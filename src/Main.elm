module Main exposing (init, main, update)

import Browser
import Browser.Events
import Data.Level as Level exposing (Level)
import Data.Vector2 as Vector2 exposing (Vector2)
import Game.Ball as Ball
import Game.Brick as Brick
import Game.Life as Life
import Game.Paddle as Paddle
import Html exposing (Html)
import Html.Attributes as Attributes
import Json.Decode as Decode exposing (Decoder)
import Random
import Svg
import Svg.Attributes as SvgAttributes



---- MODEL ----


type alias Model =
    { score : Int
    , level : Level
    , ballPosition : Vector2
    , ballVelocity : Vector2
    , ballRadius : Float
    , state : GameState
    , paddlePosition : Vector2
    , paddleVelocity : Vector2
    , paddle : Paddle.Model
    , bricks : List Brick.Model
    , controls : Controls
    , window : Window
    , unitsOnScreen : Float
    }


type alias Window =
    { width : Int
    , height : Int
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
    , Random.generate LevelResult Level.create
    )


initModel : Flags -> Model
initModel { window } =
    let
        width =
            window.width

        height =
            window.height

        unitsOnScreen =
            toFloat (min width height)
    in
    { score = 0
    , level = Level.empty
    , ballPosition = { x = 0, y = 0 }
    , ballVelocity = { x = 0, y = 0 }
    , ballRadius = 0.025 * unitsOnScreen
    , state = StartScreen
    , paddle = { height = 0.033 * unitsOnScreen, width = 0.25 * unitsOnScreen }
    , paddlePosition = { x = 0, y = 0 }
    , paddleVelocity = { x = 100, y = 0 }
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
            toFloat window.height * 0.66

        ballRadius =
            0.025 * unitsOnScreen

        paddleWidth =
            level.paddleWidth * unitsOnScreen * 0.45

        paddlePosX =
            (toFloat window.width - paddleWidth) / 2

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
                        | window = { width = width // 2, height = height }
                        , ballPosition = { x = model.ballPosition.x, y = toFloat height - 15 - model.ballRadius }
                      }
                    , Cmd.none
                    )

                _ ->
                    ( { model
                        | window = { width = width // 3, height = height // 2 }
                      }
                    , Cmd.none
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
            case model.state of
                Playing ->
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

                        Pause ->
                            ( { model
                                | controls = { left = False, right = False, movement = Stopped }
                              }
                            , Cmd.none
                            )

                        _ ->
                            ( model, Cmd.none )

                StartScreen ->
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

                        PaddleLeftUp ->
                            ( { model
                                | controls =
                                    { left = False
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
                                    , right = True
                                    , movement = Stopped
                                    }
                              }
                            , Cmd.none
                            )

                        PaddleRightUp ->
                            ( { model
                                | controls =
                                    { left = False
                                    , right = False
                                    , movement = Stopped
                                    }
                              }
                            , Cmd.none
                            )

                        Serve ->
                            ( serve model
                            , Cmd.none
                            )

                        _ ->
                            ( model, Cmd.none )

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
    if ballBottom > toFloat model.window.height then
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
    { x = 0, y = 0 }


movePaddle : Model -> Float -> Vector2
movePaddle model distance =
    let
        movementVector =
            if model.controls.left then
                { x = -1, y = 0 }

            else if model.controls.right then
                { x = 1, y = 0 }

            else
                { x = 0, y = 0 }

        newPos =
            Vector2.scaleBy movementVector distance
                |> Vector2.add model.paddlePosition
    in
    if newPos.x < 0 then
        { newPos | x = 0 }

    else if newPos.x > (toFloat model.window.width - model.paddle.width) then
        -- Vector2.scaleBy newPos unitsOnScreen
        { newPos | x = toFloat model.window.width - model.paddle.width }

    else
        newPos


serve : Model -> Model
serve model =
    let
        controls =
            model.controls

        initialVelocity =
            if controls.right then
                Vector2.normalize { x = 1, y = -1 }

            else
                Vector2.normalize { x = -1, y = -1 }
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


view : Model -> Html Msg
view model =
    Html.div
        [ Attributes.class "container" ]
        [ Html.div
            [ Attributes.class "game-container"
            ]
            [ header model
            , case model.state of
                StartScreen ->
                    Html.div
                        [ Attributes.class "game-overlay" ]
                        [ Html.div [ Attributes.class "start-title" ]
                            [ Html.text "Space to launch the ball" ]
                        ]

                _ ->
                    Html.text ""
            , board model
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
    let
        window =
            model.window

        headerHeight =
            toFloat window.height * 0.16
    in
    Svg.svg
        [ SvgAttributes.width <| String.fromInt window.width
        , SvgAttributes.height <| String.fromFloat headerHeight
        ]
    <|
        [ Svg.text_
            [ SvgAttributes.x "0"
            , SvgAttributes.y <| String.fromFloat (headerHeight / 2 + 4)
            , SvgAttributes.fontSize "12"
            ]
            [ Svg.text "LEVEL 1" ]
        ]
            ++ Life.view (Level.lives model.level) headerHeight window.width


board : Model -> Html Msg
board model =
    case model.state of
        StartScreen ->
            displayGameBoard model

        Playing ->
            displayGameBoard model

        _ ->
            Html.div [] []


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
            toFloat window.height * 0.66
    in
    Svg.svg
        [ SvgAttributes.width <| String.fromInt window.width
        , SvgAttributes.height <| String.fromFloat height
        , SvgAttributes.class "game-board"
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
    Decode.map toInput (Decode.field "key" Decode.string)


keyUpDecoder : Decoder Msg
keyUpDecoder =
    Decode.map toUpInput (Decode.field "key" Decode.string)


toUpInput : String -> Msg
toUpInput string =
    case string of
        "ArrowLeft" ->
            GameInput PaddleLeftUp

        "ArrowRight" ->
            GameInput PaddleRightUp

        _ ->
            NoOp


toInput : String -> Msg
toInput string =
    case string of
        "ArrowLeft" ->
            GameInput PaddleLeft

        "ArrowRight" ->
            GameInput PaddleRight

        " " ->
            GameInput Serve

        "Escape" ->
            GameInput Pause

        _ ->
            NoOp



---- PROGRAM ----


type alias Flags =
    { window : Window }


main : Program Flags Model Msg
main =
    Browser.element
        { view = view
        , init = init
        , update = update
        , subscriptions = subscriptions
        }

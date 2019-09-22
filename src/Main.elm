module Main exposing (init, main, update)

import Browser exposing (Document)
import Browser.Events
import Data.Cell as Cell exposing (Cell)
import Data.Grid as Grid exposing (Coordinate, Grid)
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
import Svg exposing (Svg)
import Svg.Attributes
import Time



---- MODEL ----


type alias Model =
    { score : Int
    , level : Level
    , ballVelocity : Vector2
    , state : GameState
    , controls : Controls
    , paused : Bool
    , grid : Grid Cell
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
init _ =
    ( initModel
    , Random.generate LevelResult (Level.random 20)
    )


initModel : Model
initModel =
    { score = 0
    , level = Level.empty
    , ballVelocity = Movement.none
    , state = StartScreen
    , controls = initControls
    , paused = False
    , grid = Grid.empty { rows = 10, columns = 20 }
    }


generateLayout : Model -> Model
generateLayout model =
    let
        newPaddle =
            Paddle.init model.level.paddleWidth

        ( baseX, baseY ) =
            Grid.dimensions model.grid
                |> (\{ rows, columns } ->
                        ( (columns - newPaddle.width) // 2
                        , rows
                        )
                   )

        newGrid bricks paddle ball =
            List.foldl (\( coords, cell ) -> Grid.insert coords cell) model.grid bricks
                |> (\g ->
                        List.foldl (\( coords, cell ) -> Grid.insert coords cell) g paddle
                            |> (\g_ ->
                                    List.foldl (\( coords, cell ) -> Grid.insert coords cell) g_ ball
                               )
                   )

        brickCells =
            Brick.generate model.level.bricks
                |> List.indexedMap
                    (\index columns ->
                        columns
                            |> List.indexedMap
                                (\j brick ->
                                    [ ( { x = toFloat (j + j)
                                        , y = toFloat (index + 1)
                                        }
                                      , Cell.Brick brick
                                      )
                                    , ( { x = toFloat (j + j + 1)
                                        , y = toFloat (index + 1)
                                        }
                                      , Cell.Brick brick
                                      )
                                    ]
                                )
                            |> List.concat
                    )
                |> List.concat

        paddleCells =
            List.repeat newPaddle.width newPaddle
                |> List.indexedMap
                    (\index p ->
                        ( { x = toFloat (baseX + index)
                          , y = toFloat (baseY - 1)
                          }
                        , Cell.Paddle p
                        )
                    )

        ballCells =
            Ball.init (toFloat Cell.size * 0.5)
                |> (\b -> [ ( { x = toFloat (baseX + 1), y = toFloat (baseY - 2) }, Cell.Ball b ) ])
    in
    { model | grid = newGrid brickCells paddleCells ballCells }


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
        Reset ->
            ( initModel
            , Random.generate LevelResult
                (Level.random
                    (Grid.dimensions model.grid
                        |> (\{ columns } -> columns)
                    )
                )
            )

        Tick _ ->
            if model.paused then
                ( model, Cmd.none )

            else
                case model.state of
                    Playing ->
                        let
                            newModel =
                                updateGameState model
                        in
                        if newModel.level.lives < 1 then
                            ( { newModel | state = Lost }, Cmd.none )

                        else
                            ( newModel
                            , Cmd.none
                            )

                    StartScreen ->
                        ( updateGameState model
                        , Cmd.none
                        )

                    _ ->
                        ( model, Cmd.none )

        LevelResult level ->
            ( generateLayout { model | level = level }
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
                                    , movement =
                                        case model.controls.movement of
                                            Moving ->
                                                Stopped

                                            Stopped ->
                                                Moving
                                    }
                                , paused = not model.paused
                              }
                            , Cmd.none
                            )

                        StartScreen ->
                            ( model, Cmd.none )

                        _ ->
                            ( initModel
                            , Random.generate LevelResult
                                (Level.random
                                    (Grid.dimensions model.grid
                                        |> (\{ columns } -> columns)
                                    )
                                )
                            )

                Serve ->
                    case model.state of
                        StartScreen ->
                            ( serve model, Cmd.none )

                        _ ->
                            ( model, Cmd.none )

        NoOp ->
            ( model, Cmd.none )


updateGameState : Model -> Model
updateGameState model =
    let
        positionCells =
            cellsToUpdate
                (\c ->
                    case c of
                        Cell.Paddle _ ->
                            True

                        Cell.Ball _ ->
                            True

                        _ ->
                            False
                )
                model.grid

        updatedGrid =
            getPositionsMap model positionCells
                |> updateGrid model.grid
    in
    { model | grid = updatedGrid }


cellsToUpdate : (Cell -> Bool) -> Grid Cell -> List ( Coordinate, Cell )
cellsToUpdate f grid =
    grid
        |> Grid.filter (\c -> f c)
        |> Grid.toList


getPositionsMap : Model -> List ( Coordinate, Cell ) -> List ( Vector2, Vector2 )
getPositionsMap model cellsToAdjust =
    List.map
        (\( coords, cell ) ->
            let
                movementVector =
                    case cell of
                        Cell.Paddle _ ->
                            if model.controls.left then
                                Movement.left

                            else if model.controls.right then
                                Movement.right

                            else
                                Movement.none

                        Cell.Ball _ ->
                            case model.controls.movement of
                                Moving ->
                                    model.ballVelocity

                                Stopped ->
                                    if model.controls.left then
                                        Movement.left

                                    else if model.controls.right then
                                        Movement.right

                                    else
                                        Movement.none

                        _ ->
                            Movement.none
            in
            ( coords, Vector2.add coords movementVector )
        )
        cellsToAdjust


boundsCheck : List Coordinate -> Grid Cell -> Bool
boundsCheck coords grid =
    let
        { rows, columns } =
            Grid.dimensions grid

        outOfBounds { x, y } =
            x < 0 || x >= toFloat columns || y < 0 || y >= toFloat rows
    in
    List.any outOfBounds coords


updateGrid : Grid Cell -> List ( Vector2, Vector2 ) -> Grid Cell
updateGrid grid positionsMap =
    let
        ( _, newPositions ) =
            List.unzip positionsMap
    in
    if boundsCheck newPositions grid then
        grid

    else
        positionsMap
            |> List.foldl
                (\( pos, nPos ) newGrid ->
                    case ( Grid.get pos grid, List.member pos newPositions ) of
                        ( Just cell, True ) ->
                            Grid.insert nPos cell newGrid

                        ( Just cell, False ) ->
                            Grid.remove pos newGrid
                                |> Grid.insert nPos cell

                        ( Nothing, _ ) ->
                            newGrid
                )
                grid


serve : Model -> Model
serve model =
    let
        controls =
            model.controls

        initialVelocity =
            if controls.right then
                Movement.rightUp

            else
                Movement.leftUp
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
            , displayGameBoard model
            , viewFooter model
            ]
        ]


viewHeader : Model -> Html Msg
viewHeader model =
    let
        { columns } =
            Grid.dimensions model.grid

        headerHeight =
            10 * Cell.size // 2

        headerWidth =
            columns * Cell.size * 2
    in
    Svg.svg
        [ Svg.Attributes.width "100%"
        , Svg.Attributes.height "25%"
        , [ 0, 0, headerWidth, headerHeight ]
            |> List.map String.fromInt
            |> String.join " "
            |> Svg.Attributes.viewBox
        ]
    <|
        [ Svg.text_
            [ Svg.Attributes.x "0"
            , Svg.Attributes.y (String.fromInt (headerHeight // 2 + 1))
            , Svg.Attributes.fontSize "1"
            ]
            [ Svg.text "LEVEL 1" ]
        ]
            ++ Life.view (Level.lives model.level) headerHeight headerWidth


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
                    [ Html.span []
                        [ if model.paused then
                            Html.text "⏯"

                          else
                            Html.text "⏸"
                        ]
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
            if model.paused then
                Html.div
                    [ Attributes.class "game-overlay"
                    , Attributes.tabindex -1
                    ]
                    [ Html.div [ Attributes.class "start-title" ]
                        [ Html.text "Game Paused" ]
                    ]

            else
                Html.text ""

        Won ->
            Html.div [ Attributes.class "game-content" ]
                [ Html.p [] [ Html.text "Winner" ] ]

        Lost ->
            Html.div [ Attributes.class "game-content" ]
                [ Html.p [] [ Html.text "Escape to reset" ] ]


displayGameBoard : Model -> Html Msg
displayGameBoard model =
    Svg.svg
        [ Grid.dimensions model.grid
            |> (\{ rows, columns } -> [ 0, 0, columns * Cell.size, rows * Cell.size ])
            |> List.map String.fromInt
            |> String.join " "
            |> Svg.Attributes.viewBox
        , Svg.Attributes.width "100%"
        , Svg.Attributes.height "100%"
        , Svg.Attributes.preserveAspectRatio "xMidYMid meet"
        ]
        (List.map (viewCell model.grid) (Grid.coordinates model.grid))


viewCell : Grid Cell -> Coordinate -> Svg Msg
viewCell grid { x, y } =
    Grid.get { x = x, y = y } grid
        |> Maybe.map
            (\cell ->
                case cell of
                    Cell.Ball ball ->
                        Svg.g
                            []
                            [ Svg.rect
                                [ Svg.Attributes.fill "#1760b9"
                                , Svg.Attributes.width (String.fromInt Cell.size)
                                , Svg.Attributes.height (String.fromInt Cell.size)
                                , Svg.Attributes.x (String.fromFloat (x * toFloat Cell.size))
                                , Svg.Attributes.y (String.fromFloat (y * toFloat Cell.size))
                                ]
                                []
                            , Svg.circle
                                [ Svg.Attributes.r (String.fromFloat ball.radius)
                                , Svg.Attributes.fill ball.fill
                                , Svg.Attributes.cx (String.fromFloat ((x * toFloat Cell.size) + ball.radius))
                                , Svg.Attributes.cy (String.fromFloat ((y * toFloat Cell.size) + ball.radius))
                                ]
                                []
                            ]

                    _ ->
                        Svg.rect
                            [ Svg.Attributes.fill (Cell.color cell)
                            , Svg.Attributes.width (String.fromInt Cell.size)
                            , Svg.Attributes.height (String.fromInt Cell.size)
                            , Svg.Attributes.strokeWidth (String.fromFloat (0.05 * toFloat Cell.size))
                            , Svg.Attributes.stroke (Cell.stroke cell)
                            , Svg.Attributes.x (String.fromFloat (x * toFloat Cell.size))
                            , Svg.Attributes.y (String.fromFloat (y * toFloat Cell.size))
                            , additionalCellAttrs cell { x = x, y = x }
                            ]
                            []
            )
        |> Maybe.withDefault (viewEmptyCell { x = x, y = y })


additionalCellAttrs : Cell -> Coordinate -> Svg.Attribute msg
additionalCellAttrs cell { x, y } =
    case cell of
        Cell.Brick _ ->
            if modBy 2 (round x) == 0 then
                Svg.Attributes.strokeDasharray "1, 1, 2"

            else
                Svg.Attributes.strokeDasharray "3, 1"

        _ ->
            Svg.Attributes.strokeDasharray ""


viewEmptyCell : Coordinate -> Svg Msg
viewEmptyCell { x, y } =
    Svg.rect
        [ Svg.Attributes.fill "#1760b9"
        , Svg.Attributes.width (String.fromInt Cell.size)
        , Svg.Attributes.height (String.fromInt Cell.size)
        , Svg.Attributes.x (String.fromFloat (x * toFloat Cell.size))
        , Svg.Attributes.y (String.fromFloat (y * toFloat Cell.size))
        ]
        []


subscriptions : Model -> Sub Msg
subscriptions _ =
    Sub.batch
        [ Browser.Events.onKeyDown keyDecoder
        , Browser.Events.onKeyUp keyUpDecoder
        , Browser.Events.onAnimationFrameDelta Tick
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

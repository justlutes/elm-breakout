module Data.Grid exposing (Coordinate, Grid, buildKey, coordinateFromKey, coordinates, dimensions, empty, filter, get, insert, map, member, positions, remove, toList)

import Dict exposing (Dict)


type Grid a
    = Grid (Internal a)


type alias Internal a =
    { rows : Int
    , columns : Int
    , cells : Dict String a
    }


type alias Coordinate =
    { x : Float
    , y : Float
    }


empty : { rows : Int, columns : Int } -> Grid a
empty { rows, columns } =
    Grid { rows = rows, columns = columns, cells = Dict.empty }


dimensions : Grid a -> { rows : Int, columns : Int }
dimensions (Grid { rows, columns }) =
    { rows = rows, columns = columns }


coordinates : Grid a -> List Coordinate
coordinates (Grid { rows, columns }) =
    List.range 0 ((rows * columns) - 1)
        |> List.map (\cellNumber -> { x = toFloat (cellNumber // rows), y = toFloat (modBy rows cellNumber) })


buildKey : Coordinate -> String
buildKey { x, y } =
    [ x, y ]
        |> List.map String.fromFloat
        |> String.join ","


coordinateFromKey : String -> Coordinate
coordinateFromKey k =
    case List.map (Maybe.withDefault 0 << String.toFloat) (String.split "," k) of
        [ x, y ] ->
            { x = x, y = y }

        _ ->
            { x = 999, y = 999 }


get : Coordinate -> Grid a -> Maybe a
get coordinate (Grid grid) =
    Dict.get (buildKey coordinate) grid.cells


insert : Coordinate -> a -> Grid a -> Grid a
insert coordinate value (Grid grid) =
    Grid { grid | cells = Dict.insert (buildKey coordinate) value grid.cells }


filter : (a -> Bool) -> Grid a -> Grid a
filter f (Grid grid) =
    Grid { grid | cells = Dict.filter (\_ v -> f v) grid.cells }


remove : Coordinate -> Grid a -> Grid a
remove coordinate (Grid grid) =
    Grid { grid | cells = Dict.remove (buildKey coordinate) grid.cells }


member : Coordinate -> Grid a -> Bool
member coordinate (Grid grid) =
    Dict.member (buildKey coordinate) grid.cells


map : (Coordinate -> a -> b) -> Grid a -> Grid b
map f (Grid grid) =
    Grid
        { rows = grid.rows
        , columns = grid.columns
        , cells = Dict.map (\k v -> f (coordinateFromKey k) v) grid.cells
        }


positions : Grid a -> List Coordinate
positions (Grid grid) =
    grid.cells
        |> Dict.keys
        |> List.map coordinateFromKey


toList : Grid a -> List ( Coordinate, a )
toList (Grid grid) =
    grid.cells
        |> Dict.toList
        |> List.map (Tuple.mapFirst coordinateFromKey)

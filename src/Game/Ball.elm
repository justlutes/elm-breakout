module Game.Ball exposing (Model, init)


type alias Model =
    { radius : Float
    , fill : String
    }


init : Float -> Model
init radius =
    { radius = radius
    , fill = "#C64947"
    }

module Game.Paddle exposing (Model, init)


type alias Model =
    { height : Int
    , width : Int
    , fill : String
    }


init : Int -> Model
init width =
    { height = 1
    , width = width
    , fill = "#C64947"
    }

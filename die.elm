module Die1 exposing (Model, Msg, init, update, view)

import Html exposing (..)
import Html.App as Html
import Html.Events exposing (..)
import Html.Attributes exposing (..)
import Random

type alias Model =
  { dieFace : Int
  }

type Msg
  = Roll
  | NewFace Int

diePath = "http://anjinhyu.is2.byuh.edu/cis101.2112/100/"

main =
  Html.program { init = init, view = view, update = update, subscriptions = (\_ -> Sub.none) }

init : (Model, Cmd Msg)
init =
  (Model 1, Cmd.none)

view : Model -> Html Msg
view model =
  div []
    [ img [style [("width", "100px"), ("height", "100px"), ("display", "block")], src <| diePath ++ toString model.dieFace ++ ".png"] [],
      button [ onClick Roll ] [ text "Roll" ]
    ]

update : Msg -> Model -> (Model, Cmd Msg)
update msg model =
  case msg of
    Roll ->
      (model, Random.generate NewFace (Random.int 1 6))

    NewFace newFace ->
      (Model newFace, Cmd.none)

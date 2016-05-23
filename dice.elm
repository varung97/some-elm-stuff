import Html exposing (..)
import Html.App as Html
import Html.Events exposing (..)
import Html.Attributes exposing (..)
import Random

numDice = 5

type alias Die = Int
type alias Model = List Die

type Msg
  = Roll
  | NewFaceRoll Int

diePath = "http://anjinhyu.is2.byuh.edu/cis101.2112/100/"

main = Html.program { init = init, view = view, update = update, subscriptions = (\_ -> Sub.none) }

init : (Model, Cmd Msg)
init = (List.repeat numDice 1, Cmd.none)

view : Model -> Html Msg
view model =
  div [] <| List.map displayDie model ++ [button [ onClick Roll ] [ text "Roll" ]]

displayDie : Die -> Html Msg
displayDie val = img [style [("width", "100px"), ("height", "100px"), ("display", "block")], src <| diePath ++ toString val ++ ".png"] []

-- update : Msg -> Model -> (Model, Cmd Msg)
-- update msg model =
--   case msg of
--     Roll diceIdx ->
--       (model, Random.generate (NewFaceRoll diceIdx) (Random.int 1 6))
--
--     NewFaceRoll diceIdx newFace ->
--       if diceIdx == numDice - 1 then
--         (List.take (numDice - 1) model ++ [newFace], Cmd.none)
--       else
--         (List.take diceIdx model ++ [newFace] ++ List.drop (diceIdx + 1) model, Random.generate (NewFaceRoll (diceIdx + 1)) (Random.int 1 6))

update : Msg -> Model -> (Model, Cmd Msg)
update msg model =
  case msg of
    Roll ->
      ([], Cmd.batch <| List.repeat numDice <| Random.generate NewFaceRoll (Random.int 1 6))

    NewFaceRoll newFace ->
        (newFace :: model, Cmd.none)

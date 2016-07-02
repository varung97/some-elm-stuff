import Html exposing (..)
import Html.App as Html
import Html.Events exposing (..)
import Html.Attributes exposing (..)
import Time exposing (..)

type alias Model =
  {
    time: Int,
    frozenTime: Int,
    running: Bool,
    frozen: Bool
  }

type Msg = Tick Time | StartStop | LapReset

main =
  Html.program { init = init, view = view, update = update, subscriptions = subscriptions }

init : (Model, Cmd Msg)
init =
  ({
    time = 0,
    frozenTime = 0,
    running = True,
    frozen = False
  }, Cmd.none)

subscriptions : Model -> Sub Msg
subscriptions model = Time.every (millisecond * 10) Tick

update : Msg -> Model -> (Model, Cmd Msg)
update msg model =
  let model' =
    case msg of
      Tick _ ->
        let (time, frozenTime) =
          if (model.running && not model.frozen) then
            (model.time + 1, model.time + 1)
          else if (model.running && model.frozen) then
            (model.time + 1, model.frozenTime)
          else if (not model.running && model.frozen) then
            (0, 0)
          else
            (model.time, model.time)
        in
          {model | time = time, frozenTime = frozenTime}
      StartStop ->
        {model | running = not model.running}
      LapReset ->
        {model | frozen = not model.frozen}
  in
    (model', Cmd.none)

view : Model -> Html Msg
view {time, frozenTime, frozen} =
  let
    display =
      if frozen then
        frozenTime
      else
        time
  in
    div []
    [
      button [ onClick StartStop ] [ text "Start/Stop" ],
      button [ onClick LapReset ] [ text "Lap/Reset" ],
      text <| toString (display // 100) ++ "." ++ toString (display % 100)]

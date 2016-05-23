import Html exposing (..)
import Html.App as Html
import Html.Attributes exposing (..)
import Html.Events exposing (..)
import WebSocket
import Time


main =
  Html.program
    { init = init
    , view = view
    , update = update
    , subscriptions = subscriptions
    }


-- MODEL

type alias Model =
  { input : String
  , messages : List String
  }


init : (Model, Cmd Msg)
init =
  (Model "" [], Cmd.none)


-- UPDATE

type Msg
  = Input String
  | Send
  | NewMessage String
  | Tick Time.Time


update : Msg -> Model -> (Model, Cmd Msg)
update msg {input, messages} =
  case msg of
    Input newInput ->
      (Model newInput messages, Cmd.none)

    Send ->
      (Model "" messages, WebSocket.send "ws://localhost:8765/" input)

    NewMessage str ->
      (Model input (str :: messages), Cmd.none)

    Tick _ ->
      (Model input messages, WebSocket.send "ws://localhost:8765/" input)


-- SUBSCRIPTIONS

subscriptions : Model -> Sub Msg
subscriptions model =
  Sub.batch
  [ Time.every (Time.millisecond * 20) Tick,
    WebSocket.listen "ws://localhost:8765/" NewMessage
  ]


-- VIEW

view : Model -> Html Msg
view model =
  div []
    [ input [onInput Input] []
    , button [onClick Send] [text "Send"]
    , div [] (List.map viewMessage <| List.reverse model.messages)
    ]


viewMessage : String -> Html msg
viewMessage msg =
  div [] [ text msg ]

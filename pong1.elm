import Element exposing (..)
import Time exposing (..)
import Keyboard exposing (..)
import Color exposing (..)
import Collage exposing (circle, filled, collage)
import WebSocket
import Html exposing (..)
import Html.App as App
import Html.Events exposing (..)
import Random exposing (..)
import Json.Encode as Encode
import Json.Decode as Decode

paddleWidth = 20
paddleHeight = 100
boardHeight = 300
boardWidth = 500
leftMargin = 100
playerSpeed = 5
halfHeight = boardHeight // 2
halfWidth = boardWidth // 2
halfPaddleHeight = paddleHeight // 2
ballRadius = 20
ballDiameter = ballRadius * 2
init_state =
  ({
    players = ((halfHeight, 0), (halfHeight, 0)),
    ball = ((halfWidth - ballRadius, halfHeight - ballRadius), (0, 0))
   }, Cmd.none)
websocket = "ws://127.0.0.1:5678/"

type Msg = KeyDown KeyCode | KeyUp KeyCode | Tick Time | NewMessage String
type Action = PLeft Dir | PRight Dir

type alias Dir = Int
type alias Coord = (Int, Int)
type alias Vel = (Int, Int)
type alias Player = (Int, Int)
type alias Players = (Player, Player)
type alias Ball = (Coord, Vel)
type alias Model = {players : Players, ball : Ball}

main =
  App.program
    { init = init
    , subscriptions = subscriptions
    , update = update
    , view = view
    }

init : (Model, Cmd Msg)
init = init_state

subscriptions : Model -> Sub Msg
subscriptions model =
  Sub.batch
  [ Time.every (Time.millisecond * 20) Tick,
    Keyboard.downs KeyDown,
    Keyboard.ups KeyUp,
    WebSocket.listen websocket NewMessage
  ]

update : Msg -> Model -> (Model, Cmd Msg)
update msg model =
  case msg of
    KeyDown key -> (updatePlayersDown model key, Cmd.none)
    KeyUp key -> (updatePlayersUp model key, Cmd.none)
    Tick _ -> updateState model
    NewMessage json -> (jsonToState json model, Cmd.none)

view : Model -> Html Msg
view {players, ball} =
  let
    ((pLeft, pLeftDir), (pRight, pRightDir)) = players
    ((ballX, ballY), (ballXVel, ballYVel)) = ball
    ballElem = collage ballDiameter ballDiameter [filled black <| circle ballRadius]

    ballPos = bottomLeftAt (absolute (ballX - ballRadius)) (absolute <| ballY - ballRadius)
    pLeftPos = bottomLeftAt (absolute 0) (absolute <| pLeft - halfPaddleHeight)
    pRightPos = bottomLeftAt (absolute 0) (absolute <| pRight - halfPaddleHeight)

    leftSpacer = spacer leftMargin boardHeight

    pLeftContainer = container paddleWidth boardHeight pLeftPos <| color blue <| container paddleWidth paddleHeight middle empty
    pRightContainer = container paddleWidth boardHeight pRightPos <| color blue <| container paddleWidth paddleHeight middle empty
    playingAreaContainer = container boardWidth boardHeight ballPos ballElem
    board = container (boardWidth + 2 * paddleWidth) boardHeight topLeft <| flow right [pLeftContainer, playingAreaContainer, pRightContainer]

    wholeArea = container (boardWidth + leftMargin + 2 * paddleWidth) boardHeight topLeft <| flow right [leftSpacer, board]

    border = flow right [spacer leftMargin 2, color red <| container (boardWidth + 2 * paddleWidth) 2 topLeft empty]
    new = container (boardWidth + leftMargin + 2 * paddleWidth) (boardHeight + 2) topLeft <| flow down [wholeArea, border]
  in
    toHtml new

updatePlayersDown : Model -> KeyCode -> Model
updatePlayersDown model key =
  let
    (pLeft, pRight) = model.players
    (dirL, dirR) =
      case key of
        38 -> (snd pLeft, playerSpeed)
        40 -> (snd pLeft, negate playerSpeed)
        _ -> (snd pLeft, snd pRight)
  in
    {model | players = (pLeft, (fst pRight, dirR))}

updatePlayersUp : Model -> KeyCode -> Model
updatePlayersUp model key =
  let
    (pLeft, pRight) = model.players
    (dirL, dirR) =
      case key of
        38 -> (snd pLeft, 0)
        40 -> (snd pLeft, 0)
        _ -> (snd pLeft, snd pRight)
  in
    {model | players = (pLeft, (fst pRight, dirR))}

updatePlayers : Players -> Players
updatePlayers (pLeft, pRight) =
  let
    updatedPRight = fst pRight + snd pRight
    newPRight =
      if updatedPRight + halfPaddleHeight > boardHeight || updatedPRight - halfPaddleHeight < 0 then
        fst pRight
      else
        updatedPRight
    in
      (pLeft, (newPRight, snd pRight))

updateState : Model -> (Model, Cmd Msg)
updateState model =
  let
    players = model.players
    updatedPlayers = updatePlayers players
    state = applySendCmd {model | players = updatedPlayers}
  in
    state

applySendCmd : Model -> (Model, Cmd Msg)
applySendCmd model = (model, WebSocket.send websocket <| stateToJSON model)

stateToJSON : Model -> String
stateToJSON {players, ball} =
  let
    pRight = fst <| snd players
  in
    Encode.encode 0 <| Encode.list <| List.map Encode.int [pRight]

jsonToState : String -> Model -> Model
jsonToState json oldModel =
  case Decode.decodeString (Decode.list Decode.int) json of
    Ok [pLeft, ballX, ballY] -> {oldModel | players = ((pLeft, snd <| fst oldModel.players), snd oldModel.players), ball = ((ballX, ballY), snd oldModel.ball)}
    _ -> oldModel

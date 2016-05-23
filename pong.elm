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
   }, randomVel)
websocket = "ws://localhost:5678/"

type Msg = KeyDown KeyCode | KeyUp KeyCode | Tick Time | NewVel Vel | NewMessage String
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
    NewVel newVel -> applySendCmd <| setVel model newVel
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
  in
    toHtml wholeArea

updatePlayersDown : Model -> KeyCode -> Model
updatePlayersDown model key =
  let
    (pLeft, pRight) = model.players
    (dirL, dirR) =
      case key of
        83 -> (negate playerSpeed, snd pRight)
        87 -> (playerSpeed, snd pRight)
        _ -> (snd pLeft, snd pRight)
  in
    {model | players = ((fst pLeft, dirL), pRight)}

updatePlayersUp : Model -> KeyCode -> Model
updatePlayersUp model key =
  let
    (pLeft, pRight) = model.players
    (dirL, dirR) =
      case key of
        83 -> (0, snd pRight)
        87 -> (0, snd pRight)
        _ -> (snd pLeft, snd pRight)
  in
    {model | players = ((fst pLeft, dirL), pRight)}

updatePlayers : Players -> Players
updatePlayers (pLeft, pRight) =
  let
    updatedPLeft = fst pLeft + snd pLeft
    newPLeft =
      if updatedPLeft + halfPaddleHeight > boardHeight || updatedPLeft - halfPaddleHeight < 0 then
        fst pLeft
      else
        updatedPLeft
    in
      ((newPLeft, snd pLeft), pRight)

updateState : Model -> (Model, Cmd Msg)
updateState model =
  let
    players = model.players
    ((xPos, yPos), (xVel, yVel)) = model.ball
    updateXPos = xPos + xVel
    updateYPos = yPos + yVel
    (newYPos, newYVel) =
      if updateYPos + ballRadius > boardHeight || updateYPos - ballRadius < 0 then
        (yPos, negate yVel)
      else
        (updateYPos, yVel)
    state =
      let
        updatedPlayers = updatePlayers players
        bounceBall = ((xPos, newYPos), (negate xVel, newYVel))
        noBounceBall = ((updateXPos, newYPos), (xVel, newYVel))
        pLeftPos = fst <| fst updatedPlayers
        pRightPos = fst <| snd updatedPlayers
      in
        if updateXPos + ballRadius > boardWidth then
          if newYPos >= pRightPos - halfPaddleHeight && newYPos <= pRightPos + halfPaddleHeight then
            applySendCmd {model | players = updatedPlayers, ball = bounceBall}
          else
            init_state
        else if updateXPos - ballRadius < 0 then
          if newYPos >= pLeftPos - halfPaddleHeight && newYPos <= pLeftPos + halfPaddleHeight then
            applySendCmd {model | players = updatedPlayers, ball = bounceBall}
          else
            init_state
        else
          applySendCmd {model | players = updatedPlayers, ball = noBounceBall}
  in
    state

randomVel : Cmd Msg
randomVel = generate NewVel <| pair (int 2 4) (int 2 4)

setVel : Model -> Vel -> Model
setVel model newVel =
  {model | ball = (fst model.ball, newVel)}

applySendCmd : Model -> (Model, Cmd Msg)
applySendCmd model = (model, WebSocket.send websocket <| stateToJSON model)

stateToJSON : Model -> String
stateToJSON {players, ball} =
  let
    pLeft = fst <| fst players
    (ballX, ballY) = fst ball
  in
    Encode.encode 0 <| Encode.list <| List.map Encode.int [pLeft, ballX, ballY]

jsonToState : String -> Model -> Model
jsonToState json oldModel =
  case Decode.decodeString (Decode.list Decode.int) json of
    Ok [pRight] -> {oldModel | players = (fst oldModel.players, (pRight, snd <| snd oldModel.players))}
    _ -> oldModel

import Html exposing (..)
import Html.Events exposing (..)
import Html.App as Html
import Html.Attributes exposing (..)
import Random exposing (int, step, Seed, initialSeed)
import Array exposing (..)
import Time exposing (..)
import Keyboard exposing (..)

main =
  Html.program { init = init, view = view, update = update, subscriptions = subscriptions }

right = 0
down = 1
left = 2
up = 3
turnRight = 1
goStraight = 0
turnLeft = -1
leftArrow = 37
rightArrow = 39
sizeBoard = 20
sizeCell = "15px"
foodInSnakeColour = "yellow"
foodColour = "green"
snakeColour = "white"
emptyCellColour = "black"
lostColour = "red"
snakeStart1 = (sizeBoard // 2, sizeBoard // 2)
snakeStart2 = (sizeBoard // 2, sizeBoard // 2 - 1)
period = 200
foodStart = (2, 3)
seedStart = 100

type alias Msg = Int
type alias Dir = Int
type alias Row = Array String
type alias Board = Array Row
type alias Coord = (Int, Int)
type alias Snake = Array (Coord, Bool)

type alias Model =
  {
    board: Board,
    dir: Dir,
    snake: Snake,
    snakeLength: Int,
    seed: Seed
  }

init : (Model, Cmd Msg)
init = (
    {
      board = setCell foodStart foodColour <| setCell snakeStart2 snakeColour <| setCell snakeStart1 snakeColour <| repeat sizeBoard <| repeat sizeBoard emptyCellColour,
      dir = right,
      snake = fromList [(snakeStart1, False), (snakeStart2, False)],
      snakeLength = 2,
      seed = initialSeed seedStart
    },
    Cmd.none
  )

subscriptions : Model -> Sub Msg
subscriptions model =
  Sub.batch
        [
          Time.every (period * millisecond) (\_ -> goStraight),
          Keyboard.downs readPress
        ]

readPress : Int -> Int
readPress key =
  if key == leftArrow then -1
  else if key == rightArrow then 1
  else 0

view : Model -> Html Msg
view {board} = table [] <| toList <| map viewRow board

viewRow : Row -> Html Msg
viewRow xs = tr [] <| toList <| map viewSquare xs

viewSquare : String -> Html Msg
viewSquare colour =
    td [style [("width", sizeCell), ("height", sizeCell), ("background-color", colour)]] []

update : Msg -> Model -> (Model, Cmd Msg)
update msg model =
  let
    newDir = if msg == turnRight then
               (model.dir + 1) % 4
             else if msg == turnLeft then
               (model.dir - 1) % 4
             else
               model.dir
    newHead = getNewHead model.snake newDir
    expectedLength = model.snakeLength
    actualLength = length model.snake
    (snakeTailCoord, tailHasFood) = case get (actualLength - 1) model.snake of
                                      Just val -> val
                                      Nothing -> ((0, 0), False)
    (newExpectedLength, newBoard, newSnake, newSeed) =
      case cellHasFood model.board newHead of
        Just headHasFood -> updateSnakeTail expectedLength snakeTailCoord tailHasFood <|
                            updateSnakeHead newHead headHasFood (model.board, model.snake, model.seed)
        Nothing -> (expectedLength, repeat sizeBoard <| repeat sizeBoard lostColour, model.snake, model.seed)
  in
    ({
        board = newBoard,
        dir = newDir,
        snake = newSnake,
        snakeLength = newExpectedLength,
        seed = newSeed
      }, Cmd.none)

cellHasFood : Board -> Coord -> Maybe Bool
cellHasFood board (rowIdx, colIdx) =
  case get rowIdx board of
    Just row -> case get colIdx row of
                  Just val -> if val == foodInSnakeColour || val == snakeColour then
                                Nothing
                              else if val == foodColour then
                                Just True
                              else
                                Just False
                  Nothing -> Nothing
    Nothing -> Nothing

getNewHead : Snake -> Dir -> Coord
getNewHead snake dir =
  let
    ((oldHeadX, oldHeadY), hasFood) = case get 0 snake of
                             Just val -> val
                             Nothing -> ((0, 0), False)
    newHead = if dir == right then
                (oldHeadX, oldHeadY + 1)
              else if dir == left then
                (oldHeadX, oldHeadY - 1)
              else if dir == up then
                (oldHeadX - 1, oldHeadY)
              else
                (oldHeadX + 1, oldHeadY)
  in
    newHead

updateSnakeHead : Coord -> Bool -> (Board, Snake, Seed) -> (Board, Snake, Seed)
updateSnakeHead newHead hasFood (board, snake, seed) =
  let
    newSnake = append (fromList [(newHead, hasFood)]) snake
    (newBoard, newSeed) =
      if hasFood then
        let
          (cellNum, seed1) = Random.step (int 0 (sizeBoard * sizeBoard - 1)) seed
        in
          (addFood cellNum <| setCell newHead foodInSnakeColour board, seed1)
      else
        (setCell newHead snakeColour board, seed)
  in
    (newBoard, newSnake, newSeed)

updateSnakeTail : Int -> Coord -> Bool -> (Board, Snake, Seed) -> (Int, Board, Snake, Seed)
updateSnakeTail oldExpectedLength tailCoord hasFood (board, snake, seed) =
  if hasFood then
    let
      newTail = case get ((length snake) - 1) snake of
                  Just (coord, _) -> (coord, False)
                  Nothing -> ((0, 0), False)
    in
      (oldExpectedLength + 1, setCell tailCoord snakeColour board, set ((length snake) - 1) newTail snake, seed)
  else
    (oldExpectedLength, setCell tailCoord emptyCellColour board, slice 0 -1 snake, seed)

addFood : Int -> Board -> Board
addFood cellnum board = setCell (cellnum // sizeBoard, cellnum % sizeBoard) foodColour board

setCell : Coord -> String -> Board -> Board
setCell (rowIdx, colIdx) colour board =
  let
    newRow = case get rowIdx board of
               Just row -> set colIdx colour row
               Nothing -> repeat sizeBoard emptyCellColour
  in
    set rowIdx newRow board

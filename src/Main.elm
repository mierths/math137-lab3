import Lib.Color as Color
import Lib.Graphics as Graphics
import Lib.Game as Game
import Lib.Keyboard as Keyboard
import Lib.Memoize as Memoize
import Lib.List as List

import Debug exposing (todo)

import Life exposing (Board, Cell, CellStatus(..), nextBoard)


-- To define a program, we need
--   1. A type for the possible states         type State
--   2. A type for the possible events         type Event
--   3. A transition function                  update : event -> state -> state
--   4. An initial state                       init : state
--   5. A view function                        view : state -> Canvas event


-- A type for your game state. As your game gets more features, you will
-- probably add more fields to this type.
type alias State = { board : Board, paused : Bool }


-- A type for your game events. As your game gets more features, you will
-- probably add more variants to this type.
type Event = NoOp | CellClick Cell


main : Game.Game State Event
main =
  Game.game
    { init = initialState
    , update = updateGame
    , view = drawGame
    }


-- This is the board our game will start with.
initialState : State
initialState =
  let
    startingBoard : Board
    startingBoard cell = 
      case (cell.x, cell.y) of
        (1, 0) -> Alive
        (1, 1) -> Alive
        (2, 0) -> Alive
        (3, 0) -> Alive
        (1, 2) -> Alive
        (3, 2) -> Alive
        (3, 3) -> Alive
        (3, 4) -> Alive
        (4, 4) -> Alive
        (5, 4) -> Alive
        (5, 5) -> Alive
        (5, 6) -> Alive
        (7, 7) -> Alive
        (12, 2) -> Alive
        (12, 3) -> Alive
        (12, 4) -> Alive
        (12, 6) -> Alive
        (12, 7) -> Alive
        (12, 8) -> Alive
        (15, 5) -> Alive
        (15, 4) -> Alive
        (15, 6) -> Alive
        (15, 8) -> Alive
        (15, 9) -> Alive
        (15, 10) -> Alive
        (15, 11) -> Alive
        (_, _) -> Dead
  in
  { board = startingBoard, paused = False }

memoStrat : Memoize.MemoizeStrategy (Int, Int) Cell CellStatus
memoStrat =
  let
    cellToPair cell = ( cell.x, cell.y )
    pairToCell (x0, y0) = { x = x0, y = y0 }
    allPairs = List.map cellToPair allCells
    defaultStatus = Dead
  in
  { toKey = cellToPair, fromKey = pairToCell, domain = allPairs, default = defaultStatus }


invertStatus : CellStatus -> CellStatus
invertStatus x = case x of
    Alive -> Dead
    Dead -> Alive


flipCell : Cell -> Board -> Board
flipCell c b = 
  let
    newBoard : Cell -> CellStatus
    newBoard someCell = if someCell == c
      then invertStatus(b(c))
      else b(someCell)
  in
  newBoard


-- This function uses the incoming event and the current game state to
-- decide what the next game state should be.
updateGame : Game.GameEvent Event -> State -> State
updateGame event currentState =
  case event of
    -- What to do when we get a `ClockTick`
    Game.ClockTick timestamp ->
      if currentState.paused
        then currentState
        else
          let
            updatedBoard = nextBoard (currentState.board)
            memoizedBoard = Memoize.memoize memoStrat updatedBoard
          in
          { board = memoizedBoard, paused = currentState.paused }

    -- What to do when the player presses or releases a key
    Game.Keyboard keyEvent ->
      case keyEvent of
        Keyboard.KeyEventDown Keyboard.KeySpace ->
          case currentState.paused of
            True ->
              { board = currentState.board, paused = False }
            False ->
              { board = currentState.board, paused = True }
        _ ->
          currentState

    -- What to do when we get a `NoOp`
    Game.Custom NoOp->
      currentState

    Game.Custom (CellClick c) ->
      { board = flipCell c currentState.board, paused = currentState.paused }


-- Pick a size for the game board.
-- Hint: Use this when you go to write `drawCell` and `drawGame`
boardSize : Int
boardSize = 50


-- The list of all cells based on your `boardSize`.
allCells : List Cell
allCells =
  let
    range = List.range 0 boardSize
    toCell (x_coord, y_coord) = { x = x_coord, y = y_coord }
  in
  List.map toCell (List.cross range range)


-- This function will use the game state to decide what to draw on the screen.
drawGame : State -> Graphics.Canvas Event
drawGame state =
  let
    drawCell : Cell -> Graphics.Svg Event
    drawCell cell =
      let
        cellStatus : CellStatus
        cellStatus = state.board(cell)

        cellColor : Color.Color
        cellColor = 
          case cellStatus of
            Alive -> Color.purple
            Dead -> Color.teal

      in
      Graphics.drawRect
        { x0 = toFloat cell.x
        , y0 = toFloat cell.y
        , width = (1)
        , height = (1)
        , fill = cellColor
        , onClick = Just (CellClick cell)
        }


    background = Graphics.drawRect 
        { x0 = 0 -- : Float - top-left corner x-coord
        , y0 = 0 -- : Float - top-left corner y-coord
        , width = 100 -- : Float - choose rectangle width (in coordinates, not in pixels)
        , height = 100 -- : Float - choose rectangle height (in coordinates, not in pixels)
        , fill = Color.teal -- : Color - choose fill color
        , onClick = Nothing -- : Maybe e - optionally, an event when clicked
        }

    cells : List (Graphics.Svg Event)
    cells = List.map drawCell allCells

  in
  Graphics.canvas
      { title = "Seleena and Aaron Get a Life - Working Title" -- : String - title of web page
      , widthPx = 500 -- : Int - width of canvas in pixels
      , heightPx = 500 -- : Int - height of canvas in pixels
      , xMax = toFloat boardSize -- : Float - choose the x-coord of the bottom-right corner
      , yMax = toFloat boardSize -- : Float - choose the y-coord of the bottom-right corner
      , children = cells -- : List (Svg e) - list of svg elements to display
      }

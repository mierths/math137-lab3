module Lib.Game exposing (Timestamp, GameEvent(..), Game, game)

import Browser exposing (document)
import Browser.Events exposing (onAnimationFrame)
import Html
import Time exposing (posixToMillis)

import Lib.Graphics exposing (Canvas)
import Lib.Keyboard exposing (KeyEvent, keyEvents)


{-| The number of milliseconds since Midnight UTC Jan 1 1970.

(_Hint:_ You can tell how much time has elapsed in your program by
saving the start time in your game state and the subtracting later.)
-}
type alias Timestamp = Int


{-| Events that your game can respond to.

The `ClockTick` event occurs about 60 times per second and carries the
current timestamp.

The `Keyboard` event occurs whenever a key is pushed down or released.

The `Custom` event can be triggered by the `onClick` property of an
`SVG` element (see `Lib.Graphics`).
-}
type GameEvent e
  = ClockTick Timestamp
  | Keyboard KeyEvent
  | Custom e


{-| A simple game, with state type 's' and custom event type 'e'.
-}
type alias Game s e = Program () s (GameEvent e)


{-| Create a game.

This follows the "State Machine" program architecture. We must provide:

  1. A type `s` of possible program states,
  2. A type `GameEvent e` of possible events,
  3. A starting state `init : s`,
  4. A transition function `update : GameEvent e -> s -> s`, and
  5. A view function `view : s -> Canvas e`
-}
game :
  { init : s
  , update : GameEvent e -> s -> s
  , view : s -> Canvas e
  } ->
  Game s e
game props =
  document
    { init = \_ -> (props.init, Cmd.none)
    , update = \e s -> (props.update e s, Cmd.none)
    , subscriptions = \_ ->
        let
          clockTick = onAnimationFrame (ClockTick << posixToMillis)
          keyboard = Sub.map Keyboard keyEvents
        in
        Sub.batch [clockTick, keyboard]
    , view = \s ->
        let
          { title, body } = props.view s
          newBody = List.map (Html.map Custom) body
        in
        { title = title, body = newBody }
    }

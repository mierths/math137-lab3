module Lib.Keyboard exposing
  ( KeyEvent(..)
  , Key(..)
  , keyEvents
  )

import Browser.Events
import Json.Decode exposing (andThen, fail, field, string, succeed)


{-| An event that triggers whenever a key is pressed down or released.
Bear in mind, these events correspond to keys, not to characters.
-}
type KeyEvent
  = KeyEventDown Key
  | KeyEventUp Key


{-| A `Key` represents a physical key on the keyboard, not a character.
In particular, the `Key` type does not distinguish between lower case
and upper case.
-}
type Key
  = KeyEscape
  | KeyF1
  | KeyF2
  | KeyF3
  | KeyF4
  | KeyF5
  | KeyF6
  | KeyF7
  | KeyF8
  | KeyF9
  | KeyF10
  | KeyF11
  | KeyF12
  | KeyInsert
  | KeyDelete
  | KeyHome
  | KeyEnd
  | KeyPageUp
  | KeyPageDown
  | KeyUp
  | KeyLeft
  | KeyDown
  | KeyRight
  | KeyTab
  | KeyCapsLock
  | KeyShift
  | KeyControl
  | KeyAlt
  | KeyBackspace
  | KeyEnter
  | KeySpace
  | KeyBacktick
  | KeyHyphen
  | KeyEquals
  | KeyLeftBracket
  | KeyRightBracket
  | KeyBackslash
  | KeySemicolon
  | KeyApostrophe
  | KeyComma
  | KeyPeriod
  | KeySlash
  | Key1
  | Key2
  | Key3
  | Key4
  | Key5
  | Key6
  | Key7
  | Key8
  | Key9
  | Key0
  | KeyQ
  | KeyW
  | KeyE
  | KeyR
  | KeyT
  | KeyY
  | KeyU
  | KeyI
  | KeyO
  | KeyP
  | KeyA
  | KeyS
  | KeyD
  | KeyF
  | KeyG
  | KeyH
  | KeyJ
  | KeyK
  | KeyL
  | KeyZ
  | KeyX
  | KeyC
  | KeyV
  | KeyB
  | KeyN
  | KeyM


{-| Subscribe to key events.
-}
keyEvents : Sub KeyEvent
keyEvents =
  let
    keyDecoder = field "key" string |> andThen parseKey

    parseKey str =
      case str of
        "Escape" -> succeed KeyEscape
        "F1" -> succeed KeyF1
        "F2" -> succeed KeyF2
        "F3" -> succeed KeyF3
        "F4" -> succeed KeyF4
        "F5" -> succeed KeyF5
        "F6" -> succeed KeyF6
        "F7" -> succeed KeyF7
        "F8" -> succeed KeyF8
        "F9" -> succeed KeyF9
        "F10" -> succeed KeyF10
        "F11" -> succeed KeyF11
        "F12" -> succeed KeyF12
        "Insert" -> succeed KeyInsert
        "Delete" -> succeed KeyDelete
        "Home" -> succeed KeyHome
        "End" -> succeed KeyEnd
        "PageUp" -> succeed KeyPageUp
        "PageDown" -> succeed KeyPageDown
        "ArrowUp" -> succeed KeyUp
        "ArrowLeft" -> succeed KeyLeft
        "ArrowDown" -> succeed KeyDown
        "ArrowRight" -> succeed KeyRight
        "Tab" -> succeed KeyTab
        "CapsLock" -> succeed KeyCapsLock
        "Shift" -> succeed KeyShift
        "Control" -> succeed KeyControl
        "Alt" -> succeed KeyAlt
        "Backspace" -> succeed KeyBackspace
        "Enter" -> succeed KeyEnter
        " " -> succeed KeySpace
        "`" -> succeed KeyBacktick
        "~" -> succeed KeyBacktick
        "-" -> succeed KeyHyphen
        "_" -> succeed KeyHyphen
        " =" -> succeed KeyEquals
        "+" -> succeed KeyEquals
        "[" -> succeed KeyLeftBracket
        "{" -> succeed KeyLeftBracket
        "]" -> succeed KeyRightBracket
        "}" -> succeed KeyRightBracket
        "\\" -> succeed KeyBackslash
        "|" -> succeed KeyBackslash
        ";" -> succeed KeySemicolon
        ":" -> succeed KeySemicolon
        "'" -> succeed KeyApostrophe
        "\"" -> succeed KeyApostrophe
        "," -> succeed KeyComma
        "<" -> succeed KeyComma
        "." -> succeed KeyPeriod
        ">" -> succeed KeyPeriod
        "/" -> succeed KeySlash
        "?" -> succeed KeySlash
        "1" -> succeed Key1
        "!" -> succeed Key1
        "2" -> succeed Key2
        "@" -> succeed Key2
        "3" -> succeed Key3
        "#" -> succeed Key3
        "4" -> succeed Key4
        "$" -> succeed Key4
        "5" -> succeed Key5
        "%" -> succeed Key5
        "6" -> succeed Key6
        "^" -> succeed Key6
        "7" -> succeed Key7
        "&" -> succeed Key7
        "8" -> succeed Key8
        "*" -> succeed Key8
        "9" -> succeed Key9
        "(" -> succeed Key9
        "0" -> succeed Key0
        ")" -> succeed Key0
        "q" -> succeed KeyQ
        "Q" -> succeed KeyQ
        "w" -> succeed KeyW
        "W" -> succeed KeyW
        "e" -> succeed KeyE
        "E" -> succeed KeyE
        "r" -> succeed KeyR
        "R" -> succeed KeyR
        "t" -> succeed KeyT
        "T" -> succeed KeyT
        "y" -> succeed KeyY
        "Y" -> succeed KeyY
        "u" -> succeed KeyU
        "U" -> succeed KeyU
        "i" -> succeed KeyI
        "I" -> succeed KeyI
        "o" -> succeed KeyO
        "O" -> succeed KeyO
        "p" -> succeed KeyP
        "P" -> succeed KeyP
        "a" -> succeed KeyA
        "A" -> succeed KeyA
        "s" -> succeed KeyS
        "S" -> succeed KeyS
        "d" -> succeed KeyD
        "D" -> succeed KeyD
        "f" -> succeed KeyF
        "F" -> succeed KeyF
        "g" -> succeed KeyG
        "G" -> succeed KeyG
        "h" -> succeed KeyH
        "H" -> succeed KeyH
        "j" -> succeed KeyJ
        "J" -> succeed KeyJ
        "k" -> succeed KeyK
        "K" -> succeed KeyK
        "l" -> succeed KeyL
        "L" -> succeed KeyL
        "z" -> succeed KeyZ
        "Z" -> succeed KeyZ
        "x" -> succeed KeyX
        "X" -> succeed KeyX
        "c" -> succeed KeyC
        "C" -> succeed KeyC
        "v" -> succeed KeyV
        "V" -> succeed KeyV
        "b" -> succeed KeyB
        "B" -> succeed KeyB
        "n" -> succeed KeyN
        "N" -> succeed KeyN
        "m" -> succeed KeyM
        "M" -> succeed KeyM
        _ -> fail ("unrecognized keycode: " ++ str)

    keyDowns =
      Sub.map KeyEventDown (Browser.Events.onKeyDown keyDecoder)

    keyUps =
      Sub.map KeyEventUp (Browser.Events.onKeyUp keyDecoder)

  in
  Sub.batch [keyDowns, keyUps]

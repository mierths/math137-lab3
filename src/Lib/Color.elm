module Lib.Color exposing (
  Color, hexValue,
  white, silver, gray, black, red, maroon, yellow, olive,
  lime, green, aqua, teal, blue, navy, fuchsia, purple
  )

{-| A color in RGB hex notation.
-}
type Color = Color String

hexValue : Color -> String
hexValue (Color s) = s

white   = Color "#ffffff"
silver  = Color "#c0c0c0"
gray    = Color "#808080"
black   = Color "#000000"
red     = Color "#ff0000"
maroon  = Color "#800000"
yellow  = Color "#ffff00"
olive   = Color "#808000"
lime    = Color "#00ff00"
green   = Color "#008000"
aqua    = Color "#00ffff"
teal    = Color "#008080"
blue    = Color "#0000ff"
navy    = Color "#000080"
fuchsia = Color "#ff00ff"
purple  = Color "#800080"

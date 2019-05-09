module Lib.Graphics exposing (Svg, drawRect, Canvas, canvas)

import Browser
import Svg
import Svg.Attributes
import Svg.Events

import Lib.Color exposing (Color, hexValue)
import Lib.List exposing (filterMaybes)


{-| A "scaled vector graphic" element that can be drawn on a canvas.
-}
type alias Svg e = Svg.Svg e


{-| Create an 'Svg e' rectangle.
-}
drawRect :
  { x0 : Float -- top-left corner x-coord
  , y0 : Float -- top-left corner y-coord
  , width : Float -- choose rectangle width (in coordinates, not in pixels)
  , height : Float -- choose rectangle height (in coordinates, not in pixels)
  , fill : Color -- choose fill color
  , onClick : Maybe e -- optionally, an event when clicked
  } ->
  Svg e
drawRect props =
  let
    attrb = filterMaybes
      [ Just <| Svg.Attributes.x (String.fromFloat props.x0)
      , Just <| Svg.Attributes.y (String.fromFloat props.y0)
      , Just <| Svg.Attributes.width (String.fromFloat props.width)
      , Just <| Svg.Attributes.height (String.fromFloat props.height)
      , Just <| Svg.Attributes.fill (hexValue props.fill)
      , Maybe.map Svg.Events.onClick props.onClick
      ]
  in
  Svg.rect attrb []


{-| A web page consisting of a single container for graphical elements.
-}
type alias Canvas e = Browser.Document e


{-| Create a canvas.
-}
canvas :
  { title : String -- title of web page
  , widthPx : Int -- width of canvas in pixels
  , heightPx : Int -- height of canvas in pixels
  , xMax : Float -- choose the x-coord of the bottom-right corner
  , yMax : Float -- choose the y-coord of the bottom-right corner
  , children : List (Svg e) -- list of svg elements to display
  } ->
  Canvas e
canvas props =
  { title = props.title
  , body =
      [ Svg.svg
          [ Svg.Attributes.width (String.fromInt props.widthPx)
          , Svg.Attributes.height (String.fromInt props.heightPx)
          , Svg.Attributes.viewBox (
              let
                x = String.fromFloat props.xMax
                y = String.fromFloat props.yMax
              in
              "0 0 " ++ x ++ " " ++ y
              )
          ]
          props.children
      ]
  }

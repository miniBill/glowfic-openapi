module TypedSvg.Attributes.InPixels exposing (..)

import Pixels exposing (Pixels)
import Quantity exposing (Quantity)
import TypedSvg.Attributes.InPx
import TypedSvg.Core exposing (Attribute)


x : Quantity Float Pixels -> Attribute msg
x v =
    TypedSvg.Attributes.InPx.x (Pixels.inPixels v)


y : Quantity Float Pixels -> Attribute msg
y v =
    TypedSvg.Attributes.InPx.y (Pixels.inPixels v)


width : Quantity Float Pixels -> Attribute msg
width v =
    TypedSvg.Attributes.InPx.width (Pixels.inPixels v)


height : Quantity Float Pixels -> Attribute msg
height v =
    TypedSvg.Attributes.InPx.height (Pixels.inPixels v)


textLength : Quantity Float Pixels -> Attribute msg
textLength v =
    TypedSvg.Attributes.InPx.textLength (Pixels.inPixels v)

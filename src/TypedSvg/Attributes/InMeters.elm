module TypedSvg.Attributes.InMeters exposing (..)

import Length exposing (Length)
import TypedSvg.Attributes
import TypedSvg.Core exposing (Attribute)
import TypedSvg.Types


viewBox : Length -> Length -> Length -> Length -> Attribute msg
viewBox minX minY vWidth vHeight =
    TypedSvg.Attributes.viewBox
        (Length.inCentimeters minX)
        (Length.inCentimeters minY)
        (Length.inCentimeters vWidth)
        (Length.inCentimeters vHeight)


x : Length -> Attribute msg
x v =
    TypedSvg.Attributes.x (TypedSvg.Types.Num (Length.inCentimeters v))


y : Length -> Attribute msg
y v =
    TypedSvg.Attributes.y (TypedSvg.Types.Num (Length.inCentimeters v))


width : Length -> Attribute msg
width v =
    TypedSvg.Attributes.width (TypedSvg.Types.Num (Length.inCentimeters v))


height : Length -> Attribute msg
height v =
    TypedSvg.Attributes.height (TypedSvg.Types.Num (Length.inCentimeters v))


textLength : Length -> Attribute msg
textLength v =
    TypedSvg.Attributes.textLength (TypedSvg.Types.Num (Length.inCentimeters v))

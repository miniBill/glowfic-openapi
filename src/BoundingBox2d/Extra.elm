module BoundingBox2d.Extra exposing (height, width)

import BoundingBox2d exposing (BoundingBox2d)
import Quantity exposing (Quantity)


width : BoundingBox2d units coordinates -> Quantity Float units
width boundingBox =
    BoundingBox2d.maxX boundingBox |> Quantity.minus (BoundingBox2d.minX boundingBox)


height : BoundingBox2d units coordinates -> Quantity Float units
height boundingBox =
    BoundingBox2d.maxY boundingBox |> Quantity.minus (BoundingBox2d.minY boundingBox)

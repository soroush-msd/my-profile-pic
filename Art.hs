module Art where

import ShapeGraphics
import Codec.Picture


art = tree 15 (Point 400 400) (Vector 7 (-35)) (Colour 32 178 170 255)
 ++ tree 15 (Point 400 400) (Vector 6 (-30)) (Colour  32 178 170 255)
 ++ tree 15 (Point 400 400) (Vector 5 (-25)) (Colour  32 178 170 255)
 ++ tree 15 (Point 400 400) (Vector 4 (-20)) (Colour 32 178 170 255)
 ++ tree 15 (Point 400 400) (Vector 3 (-15)) (Colour 32 178 170 255)
 ++tree 15 (Point 400 400) (Vector 2 (-10)) (Colour 32 178 170 255)
 ++ tree 15 (Point 400 400) (Vector 1 (-5)) (Colour 32 178 170 255)
 ++ tree 15 (Point 400 400) (Vector 1 (5)) (Colour 32 178 170 255)
 ++ tree 15 (Point 400 400) (Vector 2 (10)) (Colour 32 178 170 255)
 ++ tree 15 (Point 400 400) (Vector 3 (15)) (Colour 32 178 170 255)
 ++ tree 15 (Point 400 400) (Vector 4 (20)) (Colour  32 178 170 255)
 ++ tree 15 (Point 400 400) (Vector 5 (25)) (Colour  32 178 170 255)
 ++ tree 15 (Point 400 400) (Vector 6 (30)) (Colour  32 178 170 255)
 ++ tree 15 (Point 400 400) (Vector 7 (35)) (Colour  32 178 170 255)
 ++ tree 7 (Point 175 550) (Vector 8 (40)) (Colour  3 252 252 255)


tree :: Int -> Point -> Vector -> Colour -> Picture
tree depth treeBase treeDirection startColour =
  let
    -- Scale of branches
    branchScale = 1.0
    -- Angle of left branch (radians)
    leftAngle = -0.4
    -- Angle of right branch (radians)
    rightAngle = 0.4

    -- middleAngle = 0.05
    -- Change in color for each iteration
    colourChange = Colour 15 3 15 1

    recursiveFractal :: Int -> Point -> Vector -> Colour -> [PictureObject]
    recursiveFractal 0 _ _ _ = []
    recursiveFractal depth base direction colour =
      [lineToPath (vectorLine base direction) colour Solid]
      ++ recursiveFractal (depth - 1) con leftDirection branchColour
      -- ++ recursiveFractal (depth - 1) con rightDirection branchColour
      ++ recursiveFractal (depth - 1) con rightDirection branchColour
      -- ++ recursiveFractal (depth - 1) bbb rightDirection branchColour

      where
        topOfRoot = movePoint base direction
        con = movePoint topOfRoot direction
        leftDirection =
          scaleVector branchScale $ rotateVector leftAngle direction
        rightDirection =
          scaleVector branchScale $ rotateVector leftAngle direction
        branchColour = addColour colour colourChange
  in
    recursiveFractal depth treeBase treeDirection startColour


-- Produce a line by drawing a vector from a point
vectorLine :: Point -> Vector -> Line
vectorLine base vector = Line base $ movePoint base vector

-- Produce a picture object from a line
lineToPath :: Line -> Colour -> LineStyle -> PictureObject
lineToPath (Line start end) = Path [start, end]

-- Scale a vector by a given factor
scaleVector :: Float -> Vector -> Vector
scaleVector factor (Vector x y) = Vector (factor * x) (factor * y)

-- Rotate a vector by a given angle (in radians)
rotateVector :: Float -> Vector -> Vector
rotateVector angle (Vector x y) = Vector x' y'
  where
    x' = x * (cos angle) - y * (sin angle)
    y' = y * (cos angle) + x * (sin angle)

-- Offset a point using a vector for difference between points
movePoint :: Point -> Vector -> Point
movePoint (Point x y) (Vector dx dy)
  = Point (x + dx) (y + dy)

addColour :: Colour -> Colour -> Colour
addColour (Colour lr lg lb lo) (Colour rr rg rb ro) =
  Colour (mix lr rr) (mix lg rg) (mix lb rb) (mix lo ro)
  where
    mix a b = min 255 (a + b)

writeToFile pic
  = writePng "art.png" (drawPicture 2 art)

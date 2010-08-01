module Physics.Vector where

import Math.Normalize
import Math.Point (Point(..))
import Math.Utils


data Vector = Vector !GLfloat !GLfloat
  deriving (Eq,Show,Ord)

instance HasZero Vector where
  zero                = Vector 0 0
  isZero (Vector 0 0) = True
  isZero _            = False

instance Normalize Vector where
  normalize v@(Vector x y) = Vector (x / len) (y / len)
    where len = vectorLength v

zeroVector :: Vector
zeroVector  = Vector 0 0

mapVector :: (GLfloat -> GLfloat) -> Vector -> Vector
mapVector f (Vector x y) = Vector (f x) (f y)

dotProduct :: Vector -> Vector -> GLfloat
dotProduct (Vector x1 y1) (Vector x2 y2) = x1*x2 + y1*y2

addVector :: Vector -> Vector -> Vector
addVector (Vector x1 y1) (Vector x2 y2) = Vector (x1+x2) (y1+y2)

subtractVector :: Vector -> Vector -> Vector
subtractVector (Vector x1 y1) (Vector x2 y2) = Vector (x1-x2) (y1-y2)

vectorLength :: Vector -> GLfloat
vectorLength (Vector x y) = sqrt (x*x + y*y)

invertVector :: Vector -> Vector
invertVector (Vector x y) = Vector (-x) (-y)

normalVector :: Vector -> Vector
normalVector (Vector x y) = Vector (-y) x

projAlong :: Vector -> Vector -> Vector
projAlong u a = scaleVector ((u `dotProduct` a) / (vectorLength a) ^ 2) a

-- | Turn a point into a vector.
pointToVector :: Point -> Vector
pointToVector (Point x y) = Vector x y

scaleVector :: GLfloat -> Vector -> Vector
scaleVector d (Vector x y) = Vector (d*x) (d*y)

module Math.Point where

import Graphics
import Math.Normalize
import Math.Utils


data Point = Point
  { pointX :: !GLfloat
  , pointY :: !GLfloat
  } deriving (Eq,Show,Ord)

instance HasZero Point where
  zero               = Point 0 0
  isZero (Point 0 0) = True
  isZero _           = False

instance Render Point where
  render (Point x y) = vertex2d x y

instance Normalize Point where
  normalize p@(Point x y)
    | len == 0  = p
    | otherwise = Point (x/len) (y/len)
    where
    len = sqrt (x*x + y*y)

instance Num Point where
  (*)           = pointBinary (*)
  (+)           = pointBinary (+)
  (-)           = pointBinary (-)
  abs           = pointUnary abs
  signum        = pointUnary abs
  fromInteger i = Point (fromInteger i) (fromInteger i)

instance Fractional Point where
  (/)               = pointBinary (/)
  recip (Point x y) = Point (1/x) (1/y)
  fromRational r    = Point (fromRational r) (fromRational r)

scalePoint :: GLfloat -> Point -> Point
scalePoint d (Point x y) = Point (d*x) (d*y)

pointBinary :: (GLfloat -> GLfloat -> GLfloat) -> (Point -> Point -> Point)
pointBinary f (Point x1 y1) (Point x2 y2) = Point (f x1 x2) (f y1 y2)

pointUnary :: (GLfloat -> GLfloat) -> (Point -> Point)
pointUnary f (Point x y) = Point (f x) (f y)

dot :: Point -> Point -> GLfloat
dot (Point x1 y1) (Point x2 y2) = x1 * x2 + y1 * y2
{-# INLINE dot #-}

normal :: Point -> Point
normal (Point x y) = Point (-y) x

distance :: Point -> Point -> GLfloat
distance p1 p2 = sqrt (x*x + y*y)
  where
  Point x y = p1 - p2

distance0 :: Point -> GLfloat
distance0 (Point x y) = sqrt (x*x + y*y)

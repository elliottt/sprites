module Math.Matrix where

import Math.Point

import Data.Monoid (Monoid(..))
import Graphics.Rendering.OpenGL.GL (GLfloat)

data Matrix = Matrix
  { mat00 :: !GLfloat, mat01 :: !GLfloat
  , mat10 :: !GLfloat, mat11 :: !GLfloat
  } deriving (Eq,Show)

instance Num Matrix where
  (*)           = mulMatrix
  (+)           = addMatrix
  abs           = mapMatrix abs
  signum        = mapMatrix signum
  fromInteger i = Matrix
    { mat00 = fromInteger i, mat01 = 0
    , mat10 = 0, mat11 = fromInteger i
    }

instance Monoid Matrix where
  mempty  = 1
  mappend = (*)

identity :: Matrix
identity  = 1

mulScalar :: GLfloat -> Matrix -> Matrix
mulScalar r = mapMatrix (r *)

mulPoint :: Matrix -> Point -> Point
mulPoint m p = Point (p1 `dot` p) (p2 `dot` p)
  where
  p1 = Point (mat00 m) (mat01 m)
  p2 = Point (mat10 m) (mat11 m)

mulMatrix :: Matrix -> Matrix -> Matrix
mulMatrix m1 m2 = Matrix
  { mat00 = r1 `dot` c1, mat01 = r1 `dot` c2
  , mat10 = r2 `dot` c1, mat11 = r2 `dot` c2
  }
  where
  r1 = Point (mat00 m1) (mat01 m1)
  r2 = Point (mat10 m1) (mat11 m1)
  c1 = Point (mat00 m2) (mat01 m2)
  c2 = Point (mat10 m2) (mat11 m2)

addMatrix :: Matrix -> Matrix -> Matrix
addMatrix m1 m2 = Matrix
  { mat00 = mat00 m1 + mat00 m2, mat01 = mat01 m1 + mat01 m2
  , mat10 = mat10 m1 + mat10 m2, mat11 = mat11 m1 + mat11 m2
  }

mapMatrix :: (GLfloat -> GLfloat) -> Matrix -> Matrix
mapMatrix f m = Matrix
  { mat00 = f (mat00 m), mat01 = f (mat01 m)
  , mat10 = f (mat10 m), mat11 = f (mat11 m)
  }

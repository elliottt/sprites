module Math.Matrix where

import Math.Point
import Math.Utils

import Data.Monoid (Monoid(..))

data Matrix = Matrix
  { mat00 :: !GLfloat, mat01 :: !GLfloat, mat02 :: !GLfloat
  , mat10 :: !GLfloat, mat11 :: !GLfloat, mat12 :: !GLfloat
  , mat20 :: !GLfloat, mat21 :: !GLfloat, mat22 :: !GLfloat
  } deriving (Eq,Show)

instance Num Matrix where
  (*)           = mulMatrix
  (+)           = addMatrix
  abs           = mapMatrix abs
  signum        = mapMatrix signum
  fromInteger i = Matrix
    { mat00 = fromInteger i, mat01 = 0, mat02 = 0
    , mat10 = 0, mat11 = fromInteger i, mat12 = 0
    , mat20 = 0, mat21 = 0            , mat22 = 1
    }

instance Monoid Matrix where
  mempty  = 1
  mappend = (*)

identity :: Matrix
identity  = 1

dot3 :: (GLfloat,GLfloat,GLfloat) -> (GLfloat,GLfloat,GLfloat) -> GLfloat
dot3 (x1,y1,z1) (x2,y2,z2) = x1*x2 + y1*y2 + z1*z2

mulScalar :: GLfloat -> Matrix -> Matrix
mulScalar r = mapMatrix (r *)

transformPoint :: Matrix -> Point -> Point
transformPoint m p = Point (p1 `dot` p + mat02 m) (p2 `dot` p + mat12 m)
  where
  p1 = Point (mat00 m) (mat01 m)
  p2 = Point (mat10 m) (mat11 m)

mulMatrix :: Matrix -> Matrix -> Matrix
mulMatrix m1 m2 = Matrix
  { mat00 = r1 `dot3` c1, mat01 = r1 `dot3` c2, mat02 = r1 `dot3` c3
  , mat10 = r2 `dot3` c1, mat11 = r2 `dot3` c2, mat12 = r2 `dot3` c3
  , mat20 = r3 `dot3` c1, mat21 = r3 `dot3` c2, mat22 = r3 `dot3` c3
  }
  where
  r1 = (mat00 m1, mat01 m1, mat02 m1)
  r2 = (mat10 m1, mat11 m1, mat12 m1)
  r3 = (mat20 m1, mat21 m1, mat22 m1)
  c1 = (mat00 m2, mat10 m2, mat20 m2)
  c2 = (mat10 m2, mat11 m2, mat12 m2)
  c3 = (mat20 m2, mat21 m2, mat22 m2)

addMatrix :: Matrix -> Matrix -> Matrix
addMatrix m1 m2 = Matrix
  { mat00 = mat00 m1 + mat00 m2, mat01 = mat01 m1 + mat01 m2
  , mat02 = mat02 m1 + mat02 m2

  , mat10 = mat10 m1 + mat10 m2, mat11 = mat11 m1 + mat11 m2
  , mat12 = mat12 m1 + mat12 m2

  , mat20 = mat20 m1 + mat20 m2, mat21 = mat21 m1 + mat21 m2
  , mat22 = mat22 m1 + mat22 m2
  }

mapMatrix :: (GLfloat -> GLfloat) -> Matrix -> Matrix
mapMatrix f m = Matrix
  { mat00 = f (mat00 m), mat01 = f (mat01 m), mat02 = f (mat02 m)
  , mat10 = f (mat10 m), mat11 = f (mat11 m), mat12 = f (mat12 m)
  , mat20 = f (mat20 m), mat21 = f (mat21 m), mat22 = f (mat22 m)
  }

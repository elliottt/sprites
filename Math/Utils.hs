module Math.Utils (
    module Math.Utils
  , GLfloat
  ) where

import Graphics.Rendering.OpenGL.GL (GLfloat)

rangeOverlap :: (GLfloat,GLfloat) -> (GLfloat,GLfloat) -> GLfloat
rangeOverlap (a1,b1) (a2,b2)
  | a2 > a1   = b1 - a2
  | otherwise = b2 - a1

range :: Ord a => [a] -> Maybe (a,a)
range []     = Nothing
range (a:as) = loop a a as
  where
  loop l h (x:xs) | x > h     = loop l x xs
                  | x < l     = loop x h xs
                  | otherwise = loop l h xs
  loop l h []                 = Just (l,h)

class HasZero a where
  zero   :: a
  isZero :: a -> Bool

instance HasZero GLfloat where
  zero     = 0
  isZero x = x == 0

module Math.Normalize where

class Normalize a where
  normalize :: a -> a

instance Normalize a => Normalize (Maybe a) where
  normalize = fmap normalize

instance Normalize a => Normalize [a] where
  normalize = map normalize

instance (Normalize a, Normalize b) => Normalize (a,b) where
  normalize (a,b) = (normalize a,normalize b)

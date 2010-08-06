module Math.AffinePlane where


-- Points ----------------------------------------------------------------------

data Point a = Point !a !a
  deriving (Show,Eq)

distance a b = magnitude (a -. b)

-- | The origin.
zeroP :: Num a => Point a
zeroP  = Point 0 0

-- | Point subtraction.
(-.) :: Num a => Point a -> Point a -> Vector a
Point a b -. Point c d = Vector (a-c) (b-d)
infix 6 -.

-- | Point-vector addition.
(.+^) :: Num a => Point a -> Vector a -> Point a
Point a b .+^ Vector c d = Point (a+c) (b+d)
infix 6 .+^

-- | Point-vector subtraction -- adds the negation of the vector.
(.-^) :: Num a => Point a -> Vector a -> Point a
p .-^ v = p .+^ negV v
infix 6 .-^


-- Vectors ---------------------------------------------------------------------

data Vector a = Vector !a !a
  deriving (Show,Eq)

-- | The zero vector.
zeroV :: Num a => Vector a
zeroV  = Vector 0 0

-- | Calculate the magnitude of a vector.
magnitude :: Floating a => Vector a -> a
magnitude v = sqrt (v <.> v)

-- | Negate a vector.
negV :: Num a => Vector a -> Vector a
negV (Vector x y) = Vector (-x) (-y)

-- | Add two vectors.
(+^) :: Num a => Vector a -> Vector a -> Vector a
Vector a b +^ Vector c d = Vector (a+c) (b+d)
infixl 6 +^

-- | Scale a vector by a scalar.
(*^) :: Num a => a -> Vector a -> Vector a
r *^ Vector x y = Vector (r*x) (r*y)
infix 5 *^

-- | Subtract two vectors
(-^) :: Num a => Vector a -> Vector a -> Vector a
Vector a b -^ Vector c d = Vector (a-c) (b-d)
infix 6 -^

-- | The inner product of two vectors.
(<.>) :: Num a => Vector a -> Vector a -> a
Vector a b <.> Vector c d = a * c + b * d
infix 5 <.>

-- | The affine combination
combination :: Num a => a -> Point a -> a -> Point a -> Point a
combination _a1 p a2 q = p .+^ (a2 *^ (q -. p))

-- | Turn a vector into its unit version.
unitV :: Floating a => Vector a -> Vector a
unitV v = (1 / magnitude v) *^ v


-- Affine Transformations ------------------------------------------------------

data Matrix a = Matrix
  { mat00 :: !a, mat01 :: !a, mat02 :: !a
  , mat10 :: !a, mat11 :: !a, mat12 :: !a
  } deriving (Eq,Show)

idM :: Num a => Matrix a
idM  = Matrix
  { mat00 = 1, mat01 = 0, mat02 = 0
  , mat10 = 0, mat11 = 1, mat12 = 0
  }

-- | Transform a point by a matrix.
transformPoint :: Num a => Matrix a -> Point a -> Point a
transformPoint m (Point a b) = o .+^ ((a *^ r1) +^ (b *^ r2))
  where
  r1 = Vector (mat00 m) (mat10 m)
  r2 = Vector (mat10 m) (mat11 m)
  o  = Point (mat02 m) (mat12 m)

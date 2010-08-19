module Physics.Shape (
    -- * Shapes
    Shape
  , center
  , distanceBetween
  , direction
  , moveShape
  , transformShape

    -- * Construction
  , circle
  , polygon
  , rectangle
  , triangle

    -- * Collision Checking
  , shapeAABB
  ) where

import Graphics
import Math.AffinePlane
import Math.Utils
import Physics.AABB
import Utils

import Control.Applicative (Applicative(..),(<$>))
import Control.Monad (when,zipWithM_,forM_,unless,join)
import Data.IORef (IORef,newIORef,readIORef,writeIORef)
import Data.Ord (comparing)
import qualified Control.Monad.Primitive as Prim
import qualified Data.Set                as Set
import qualified Data.Vector.Mutable     as Vec


-- Shapes ----------------------------------------------------------------------

type Index    = Int
type Vertex   = Point GLfloat
type Edge     = Line GLfloat
type Vertices = Vec.MVector (Prim.PrimState IO) Vertex

data Shape
  = SCircle  !(IORef Vertex) !(IORef GLfloat)
  | SPolygon !(IORef Vertex) !Vertices

instance Render Shape where
  render (SCircle cref rref) = do
    Point x y <- readIORef cref
    r         <- readIORef rref
    renderPrimitive Points (vertex2d x y)
    renderPrimitive LineLoop $ do
      let step i = vertex2d (r * sin i + x) (r * cos i + y)
      mapM_ step [0, 0.1 .. 2 * pi]
  render (SPolygon cref vs) = do
    renderPrimitive Points (render =<< readIORef cref)
    es <- polyEdges vs
    renderPrimitive Lines (mapM_ render es)

-- | Get the center of a shape.
center :: Shape -> IO Vertex
center (SCircle  ref _) = readIORef ref
center (SPolygon ref _) = readIORef ref

-- | Get the distance between the centers of two shapes.
distanceBetween :: Shape -> Shape -> IO GLfloat
distanceBetween s1 s2 = distance <$> center s1 <*> center s2

-- | Return the vector that points from shape 1 to shape 2.
direction :: Shape -> Shape -> IO (Vector GLfloat)
direction s1 s2 = do
  c1 <- center s1
  c2 <- center s2
  return (unitV (c2 -. c1))


-- Construction ----------------------------------------------------------------

-- | Create a circle, with the given center and radius.
circle :: Point GLfloat -> GLfloat -> IO Shape
circle c r = do
  unless (r > 0) (fail "Invalid radius")
  SCircle <$> newIORef c <*> newIORef r

-- | Create a polygon, given a list of points in counter-clockwise order.  The
-- polygon can be just two points, forming a line.
polygon :: Point GLfloat -> [Vertex] -> IO Shape
polygon c ps = do
  let numVerts = length ps
  when (numVerts < 2) (fail "Not enough vertices")
  vs <- Vec.new numVerts
  zipWithM_ (Vec.write vs) [0 ..] ps
  ref <- newIORef c
  return (SPolygon ref vs)

rectangle :: Vertex -> GLfloat -> GLfloat -> IO Shape
rectangle c@(Point x y) w h = do
  when (w < 0) (fail "Width of rectangle is negative")
  when (h < 0) (fail "Height of rectangle is negative")
  let w2 = w / 2
      h2 = h / 2
  polygon c
    [ Point (x + w2) (y + h2)
    , Point (x - w2) (y + h2)
    , Point (x - w2) (y - h2)
    , Point (x + w2) (y - h2)
    ]

triangle :: Vertex -> Vertex -> Vertex -> Vertex -> IO Shape
triangle c x y z = polygon c [x,y,z]


-- Movement --------------------------------------------------------------------

-- | Move a shape by a vector.
moveShape :: Vector GLfloat -> Shape -> IO ()
moveShape (Vector x y) = transformShape (idM { mat02 = x, mat12 = y })

-- | Transform a shape by a matrix.
transformShape :: Matrix GLfloat -> Shape -> IO ()
transformShape mat (SCircle cref rref) = do
  let move = transformPoint mat
  c <- readIORef cref
  writeIORef cref $! move c
transformShape mat (SPolygon ref vs) = do
  let move = transformPoint mat
  c <- readIORef ref
  writeIORef ref $! move c
  forM_ [0 .. Vec.length vs] $ \ i -> do
    p <- Vec.read vs i
    Vec.write vs i $! move p


-- Polygon Utilities -----------------------------------------------------------

getVertex :: Vertices -> Index -> IO Vertex
getVertex vs i = Vec.read vs (i `mod` Vec.length vs)

getEdge :: Vertices -> Index -> IO Edge
getEdge vs i = Line <$> getVertex vs i <*> getVertex vs (i+1)

polyEdges :: Vertices -> IO [Edge]
polyEdges vs = loop 0
  where
  len = Vec.length vs
  loop n | n >= len  = return []
         | otherwise = (:) <$> getEdge vs n <*> loop (n-1)


-- Collision Checking ----------------------------------------------------------

shapeAABB :: Shape -> IO AABB
shapeAABB s = do
  (l,r) <- projectOnto' s (Vector 1 0)
  (b,t) <- projectOnto' s (Vector 0 1)
  let w = r - l
      h = t - b
  return $! AABB (Point l t) w h

-- | Project a shape onto a vector, yielding a one-dimensional line.
projectOnto :: Shape -> Vector GLfloat -> IO (GLfloat,GLfloat)
projectOnto s v = projectOnto' s (unitV v)

-- | Project a shape onto a unit-vector, producing a one-dimensional line.
projectOnto' :: Shape -> Vector GLfloat -> IO (GLfloat,GLfloat)
projectOnto' (SCircle cref rref) u = do
  c <- readIORef cref
  r <- readIORef rref
  let v  = c -. zero
      r' = r *^ u
      h  = v +^ r'
      l  = v -^ r'
  l `seq` h `seq` return (l <.> u,h <.> u)
projectOnto' (SPolygon cref vs) u = do
  c <- readIORef cref
  let nu     = norm u
      proj p = ((p -. zero) <.> u) / (nu * nu)
      len    = Vec.length vs
      loop l h n
        | n == len  = return (l,h)
        | otherwise = do
          x <- getVertex vs n
          let a = proj x
              rest | a < l     = loop a h (n+1)
                   | a > h     = loop l a (n+1)
                   | otherwise = loop l h (n+1)
          rest
  x0 <- getVertex vs 0
  let a0 = proj x0
  loop a0 a0 1

data Collision = Collision
  { collisionIntersect :: !Intersection
  , collisionOverlap   :: !GLfloat
  , collisionNormal    :: !(Vector GLfloat)
  } deriving (Eq,Show)

invertCollision :: Collision -> Collision
invertCollision c = c { collisionNormal = negV (collisionNormal c) }

data Intersection
  = IntersectPoint !(Point GLfloat)
  | IntersectLine !(Line GLfloat)
    deriving (Eq,Show)

-- | Check for collision between two shapes.
shapeCollision :: Shape -> Shape -> IO (Maybe Collision)
shapeCollision s1 s2 =
  case (s1,s2) of
    (SCircle cref1 rref1, SCircle cref2 rref2) ->
      circleCircleCollision cref1 rref1 cref2 rref2

    (SCircle cref1 rref, SPolygon cref2 vs) -> do
      mb <- polygonCircleCollision cref2 vs cref1 rref
      return (invertCollision <$> mb)

    (SPolygon cref1 vs, SCircle cref2 rref) ->
      polygonCircleCollision cref1 vs cref2 rref

    (SPolygon cref1 vs1, SPolygon cref2 vs2) ->
      polygonPolygonCollision cref1 vs1 cref2 vs2

-- | Check for collision between two circles.
circleCircleCollision :: IORef (Point GLfloat) -> IORef GLfloat
                      -> IORef (Point GLfloat) -> IORef GLfloat
                      -> IO (Maybe Collision)
circleCircleCollision cref1 rref1 cref2 rref2 = do
  c1 <- readIORef cref1
  r1 <- readIORef rref1
  c2 <- readIORef cref2
  r2 <- readIORef rref2
  let d       = r1 + r2
      v       = c2 -. c1
      v'      = unitV v
      overlap = d - norm v
      p       = c1 .+^ (r1 *^ v')
  if overlap >= 0
     then return (Just (Collision (IntersectPoint p) overlap v'))
     else return Nothing

-- | Check for collision between a polygon and a circle
polygonCircleCollision :: IORef (Point GLfloat) -> Vertices
                       -> IORef (Point GLfloat) -> IORef GLfloat
                       -> IO (Maybe Collision)
polygonCircleCollision cref1 vs cref2 rref = do
  error "polygonCircleCollision"

test = join (shapeCollision <$> rect (Point 0 0) <*> rect (Point 2 0))
  where
  rect p = rectangle p 2 2

-- | Check for collision between two polygons
polygonPolygonCollision :: IORef (Point GLfloat) -> Vertices
                        -> IORef (Point GLfloat) -> Vertices
                        -> IO (Maybe Collision)
polygonPolygonCollision cref1 vs1 cref2 vs2 =

  check vs1 vs2 >>=? \ (p0,i0,u0) ->
  check vs2 vs1 >>=? \ (p1,i1,u1) -> do
  cs <- if p0 > p1
           then contact vs1 vs2 id   i0 p0
           else contact vs2 vs1 negV i1 p1
  print cs
  error "polygonPolygonCollision"

  where

  contact vs poly k i p = do
    e <- getEdge vs i
    polygonContactPoints vs poly (k (lineV e)) p

  check vs cvs = loop vs cvs 1 (Vec.length vs) =<< step 0 vs cvs

  step i vs cvs = do
    e@(Line p _) <- getEdge vs i
    let d = unitV (normalV (lineV e))
    minimalPoint p d cvs

  loop vs cvs i end int@(p,_,_)
    | i == end  = return (Just int)
    | p > 0     = return Nothing
    | otherwise = do
      int'@(p',_,_) <- step i vs cvs
      if p < p'
         then loop vs cvs (i+1) end int'
         else loop vs cvs (i+1) end int

data Contact = Contact
  { conPoint   :: !Vertex
  , conNormal  :: !(Vector GLfloat)
  , conOverlap :: !GLfloat
  } deriving (Show,Eq)

instance Ord Contact where
  compare = comparing conPoint

-- | Generate a set of contact points between two polygons.
polygonContactPoints :: Vertices -> Vertices -> Vector GLfloat -> GLfloat
                     -> IO (Set.Set Contact)
polygonContactPoints vs1 vs2 d z =
  collect vs2 vs1 =<< collect vs1 vs2 Set.empty
  where
  collect vs poly = loop 0
    where
    len = Vec.length vs
    loop i ps
      | i == len  = return ps
      | otherwise = do
        p <- getVertex vs i
        b <- pointInPolygon p poly
        if b
           then loop (i+1) (Set.insert (Contact p d z) ps)
           else loop (i+1) ps

-- | Determine if a point falls inside of a polygon.
pointInPolygon :: Vertex -> Vertices -> IO Bool
pointInPolygon v vs = loop 0
  where
  len = Vec.length vs
  loop i
    | i == len  = return True
    | otherwise = do
      e@(Line a _) <- getEdge vs i
      let n = normalV (lineV e)
          p = (v -. a) <.> n
      if p < 0 || withinZero p
         then loop (i+1)
         else return False

-- | Find a minimal point on a polygon, relative to the axis provided.
minimalPoint :: Vertex -> Vector GLfloat -> Vertices -> IO (GLfloat,Index,Bool)
minimalPoint p d vs = do
  (p0,i0,u0) <- step 0
  loop 1 p0 i0 u0

  where

  pd = proj p

  -- the projection of points along the line defined by p and d.
  nd     = norm2 d
  proj x = (x -. zero) <.> d

  -- edges produce a unique value of False, as the contact isn't defined by a
  -- single point.
  step i = do
    e@(Line _ a) <- getEdge vs (i-1)
    let pj     = proj a
        v      = lineV e <.> d
        unique = not (withinZero v)
    return (pj,i,unique)

  len = Vec.length vs
  loop n pj i u
    | n == len  = return (pj - pd,i,u)
    | otherwise = do
      (pj',i',u') <- step n
      -- keep the stepped side if the contact was with an edge
      if pj' < pj || pj' == pj && not u'
         then loop (n+1) pj' i' u'
         else loop (n+1) pj  i  u

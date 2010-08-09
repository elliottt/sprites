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

import Control.Applicative (Applicative(..),(<$>))
import Control.Monad (when,zipWithM_,forM_,unless)
import Data.IORef (IORef,newIORef,readIORef,writeIORef)
import qualified Control.Monad.Primitive as Prim
import qualified Data.Vector.Mutable     as Vec


-- Shapes ----------------------------------------------------------------------

type Index    = Int
type Vertex   = Point GLfloat
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
  r <- readIORef rref
  let Point r' _ = move (Point r 0)
  writeIORef rref $! r'
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

getEdge :: Vertices -> Index -> IO (Line GLfloat)
getEdge vs i = Line <$> getVertex vs i <*> getVertex vs i

polyEdges :: Vertices -> IO [Line GLfloat]
polyEdges vs = loop 0
  where
  len = Vec.length vs
  loop n | n >= len  = return []
         | otherwise = (:) <$> getEdge vs n <*> loop (n-1)


-- Collision Checking ----------------------------------------------------------

shapeAABB :: Shape -> IO AABB
shapeAABB s = do
  (l,r) <- projectOnto s (Vector 1 0)
  (b,t) <- projectOnto s (Vector 0 1)
  return $! AABB (Point l t) (Point (r - l) (t - b))

-- | Project a shape onto a vector, producing a one-dimensional line.
projectOnto :: Shape -> Vector GLfloat -> IO (GLfloat,GLfloat)
projectOnto s u = projectOnto' s (unitV u)

-- | Project a shape onto a unit-vector, producing a one-dimensional line.
projectOnto' :: Shape -> Vector GLfloat -> IO (GLfloat,GLfloat)
projectOnto' (SCircle cref rref) u = do
  c <- readIORef cref
  r <- readIORef rref
  let v  = u +^ (c -. zero)
      h  = r *^ v
      l  = negV h
      l' = l <.> u
      h' = h <.> u
  l' `seq` h' `seq` return (l',h')
projectOnto' (SPolygon cref vs) u = do
  c <- readIORef cref
  let nu     = norm u
      proj v = (v -. zero) <.> u
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


data Intersection
  = InersectPoint !(Point GLfloat)
  | IntersectLine !(Line GLfloat)
    deriving (Eq,Show)

shapeIntersection :: Shape -> Shape -> IO (Maybe Intersection)
shapeIntersection s1 s2 = error "shapeIntersection"

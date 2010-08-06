module Physics.Shape (
    -- * Shapes
    Shape
  , center
  , distanceBetween
  , direction
  , moveShape

    -- * Construction
  , polygon
  , rectangle
  , triangle
  ) where

import Graphics
import Math.AffinePlane
import Math.Utils
import Physics.AABB
import Physics.Body
import Physics.Collision

import Control.Applicative (Applicative(..),(<$>))
import Control.Monad (guard,foldM,when,zipWithM,forM_)
import Data.IORef (IORef,newIORef,readIORef,writeIORef)
import qualified Control.Monad.Primitive as Prim
import qualified Data.Vector             as Vec


-- Shapes ----------------------------------------------------------------------

type Index    = Int
type Vertices = Vec.MVector (Prim.PrimState IO) Point

data Shape = SPolygon !(IORef Point) !Vertices

instance Render Shape where
  render (SPolygon ref vs) = do
    renderPrimitive Points (render =<< readIORef ref)
    renderPrimitive Lines (mapM_ render (polyEdges vs))

instance Physical Shape where
  boundingBox = shapeAABB
  transform   = transformShape
  position    = center

-- | Get the center of a shape.
center :: Shape -> IO Point
center (SPolygon ref _) = readIORef ref

-- | Get the distance between the centers of two shapes.
distanceBetween :: Shape -> Shape -> IO GLfloat
distanceBetween s1 s2 = distance <$> center s1 <*> center s2

-- | Return the vector that points from shape 1 to shape 2.
direction :: Shape -> Shape -> IO Vector
direction s1 s2 = do
  c1 <- center s2
  c2 <- center s1
  return (normalize (pointToVector (c1 - c2)))


-- Construction ----------------------------------------------------------------

-- | Create a polygon, given a list of points in counter-clockwise order.  If
-- there aren't enough points to make a triangle, return Nothing.  In the
-- future, check to see that the points given form a convex polygon, and if
-- not, return Nothing.
polygon :: [Point] -> IO Shape
polygon ps = do
  let numVerts = length ps
      c        = sum ps / fromIntegral numVerts
  when (numVerts < 3) (fail "Not enough vertices")
  vs <- Vec.new numVerts
  zipWithM (Vec.write vs) [0 ..] ps
  ref <- newIORef c
  return (SPolygon ref vs)

rectangle :: Point -> GLfloat -> GLfloat -> IO Shape
rectangle c@(Point x y) w h = do
  when (w < 0) (fail "Width of rectangle is negative")
  when (h < 0) (fail "Height of rectangle is negative")
  let w2 = w / 2
      h2 = h / 2
  polygon
    [ Point (x + w2) (y + h2)
    , Point (x - w2) (y + h2)
    , Point (x - w2) (y - h2)
    , Point (x + w2) (y - h2)
    ]

triangle :: Point -> Point -> Point -> IO Shape
triangle x y z = polygon [x,y,z]


-- Movement --------------------------------------------------------------------

-- | Move a shape by a vector.
moveShape :: Vector -> Shape -> IO ()
moveShape (Vector x y) = transformShape (1 { mat02 = x, mat12 = y })

-- | Transform a shape by a matrix.
transformShape :: Matrix -> Shape -> IO ()
transformShape mat (SPolygon ref vs) = do
  let move = transformPoint mat
  c <- readIORef ref
  writeIORef ref $! move c
  forM_ [0 .. Vec.length vs] $ \ i -> do
    p <- Vec.read vs i
    Vec.write vs i $! move p


-- Polygon Utilities -----------------------------------------------------------

getVertex :: Vertices -> Index -> IO Point
getVertex vs i = Vec.read vs (i `mod` Vec.length vs)

getEdge :: Vertices -> Index -> IO Line
getEdge vs i = Line <$> getVertex vs i <*> getVertex vs i

polyEdges vs = loop 0
  where
  len = Vec.length vs
  loop n | n >= len  = return []
         | otherwise = (:) <$> getEdge vs n <*> loop (n-1)


-- Collision Checking ----------------------------------------------------------

-- | Construct the AABB for a shape.
shapeAABB :: Shape -> IO AABB
shapeAABB (SPolygon _ vs) = undefined

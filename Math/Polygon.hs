module Math.Polygon where

import Math.Line
import Math.Matrix
import Math.Normalize
import Math.Point

import Control.Monad (guard,foldM)
import Data.Maybe (catMaybes)
import Graphics.Rendering.OpenGL.GL (GLfloat)

import Debug.Trace


data Polygon = Polygon
  { polyPoints :: [Point]
  , polyCenter :: !Point
  , polyRadius :: !GLfloat
  } deriving (Eq,Show)

instance Normalize Polygon where
  normalize poly = poly
    { polyPoints = ps
    , polyCenter = origin
    , polyRadius = maximum (map (distance origin) ps)
    }
    where
    origin = Point 0 0
    ps     = normalize (polyPoints poly)

-- | Apply a transformation matrix to all of the points in the polygon.
transformPolygon :: Matrix -> Polygon -> Polygon
transformPolygon m p = p
  { polyPoints = map (transformPoint m) (polyPoints p)
  , polyCenter = transformPoint m (polyCenter p)
  }

-- | Make a polygon, centered at the origin.
rectangle :: GLfloat -> GLfloat -> Polygon
rectangle w h = Polygon
  { polyPoints =
    [ Point (-w2)   h2
    , Point   w2    h2
    , Point   w2  (-h2)
    , Point (-w2) (-h2)
    ]
  , polyCenter = Point 0 0
  , polyRadius = distance (Point 0 0) (Point w2 w2)
  }
  where
  w2 = w / 2
  h2 = h / 2

-- | Get the edges of the polygon.
polyEdges :: Polygon -> [Line]
polyEdges poly = zipWith step ps (drop 1 (cycle ps))
  where
  ps         = polyPoints poly
  step p1 p2 = Line p1 p2

-- | Test two polygons for bounding circle overlap.
radiusOverlap :: Polygon -> Polygon -> Bool
radiusOverlap p1 p2 = r1 + r2 > d
  where
  d  = distance (polyCenter p1) (polyCenter p2)
  r1 = polyRadius p1
  r2 = polyRadius p2

-- | Given a list of things, return the smallest and the largest.
range :: Ord a => [a] -> Maybe (a,a)
range []       = Nothing
range (z:rest) = loop z z rest
  where
  loop l h []                 = Just (l,h)
  loop l h (a:as) | a < l     = loop a h as
                  | a > h     = loop l a as
                  | otherwise = loop l h as

-- | The result of a collision.
data Collision = Collision
  { collisionDirection :: !Vector  -- ^ Direction of the collision
  , collisionLength    :: !GLfloat -- ^ The size of the overlap
  } deriving Show

-- | Check a collision between two polygons by first testing collision between
-- their bounding circles, then by testing for collision using the split axis
-- theorem.
collides :: Polygon -> Polygon -> Maybe Collision
collides p1 p2 = do
  guard (radiusOverlap p1 p2)
  let c        = normalize (polyCenter p1 - polyCenter p2)
  let clen     = vectorLength c
  let step z e = do
        let axis   = perpendicular e
            len    = vectorLength axis
            step p = abs (axis `dot` p) / len
        (l1,r1) <- range (map step (polyPoints p1))
        (l2,r2) <- range (map step (polyPoints p2))
        let overlap | r1 >= l2  = r1 - l2
                    | l1 >= r2  = l1 - r2
                    | otherwise = -1
        if overlap == 0
           then return z
           else guard (overlap > 0) >> return (max overlap z)

  overlap <- foldM step 0 (polyEdges p1 ++ polyEdges p2)
  return Collision
    { collisionDirection = scaleVector overlap c
    , collisionLength    = overlap * clen
    }

test = collides r r'
  where
  r  = rectangle 2 2
  r' = transformPolygon (1 { mat02 = 1, mat12 = 1 }) r

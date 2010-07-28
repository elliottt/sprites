module Math.Polygon where

import Math.Line
import Math.Matrix
import Math.Normalize
import Math.Point

import Control.Monad (guard,foldM)
import Data.Maybe (catMaybes)
import Graphics.Rendering.OpenGL.GL (GLfloat)


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

movePolygon :: Vector -> Polygon -> Polygon
movePolygon v p = p
  { polyCenter = polyCenter p + v
  , polyPoints = map (+ v) (polyPoints p)
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

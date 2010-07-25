module Math.Polygon where

import Math.Line
import Math.Matrix
import Math.Normalize
import Math.Point

import Control.Monad (guard,msum)
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

transformPolygon :: Matrix -> Polygon -> Polygon
transformPolygon m p = p
  { polyPoints = map (mulPoint m) (polyPoints p)
  , polyCenter = mulPoint m (polyCenter p)
  }

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

polyEdges :: Polygon -> [Line]
polyEdges poly = zipWith step ps (drop 1 (cycle ps))
  where
  ps         = polyPoints poly
  step p1 p2 = Line p1 p2

data Collision = Collision deriving Show

radiusOverlap :: Polygon -> Polygon -> Bool
radiusOverlap p1 p2 = r1 + r2 <= d
  where
  d  = distance (polyCenter p1) (polyCenter p2)
  r1 = polyRadius p1
  r2 = polyRadius p2

range :: Ord a => [a] -> Maybe (a,a)
range []       = Nothing
range (z:rest) = loop z z rest
  where
  loop l h []                 = Just (l,h)
  loop l h (a:as) | a < l     = loop a h as
                  | a > h     = loop l a as
                  | otherwise = loop l h as

collides :: Polygon -> Polygon -> Maybe Collision
collides p1 p2 = do
  guard (radiusOverlap p1 p2)
  msum $ do
    e <- polyEdges p1 ++ polyEdges p2
    let axis   = perpendicular e
        len    = distance (Point 0 0) axis
        step p = abs (axis `dot` p) / len
    return $ do
      (l1,r1) <- range (map step (polyPoints p1))
      (l2,r2) <- range (map step (polyPoints p2))
      guard (r1 >= l2 || l1 >= r2)
      return Collision

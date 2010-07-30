module Physics.AABB where

import Graphics
import Math.Point
import Math.Utils

-- | An axis aligned bounding box.  The second point is relative to the first,
-- specifying the extent of the bounding box.
data AABB = AABB !Point !Point deriving Show

-- | Render an AABB as a transparent rectangle, so that whatever is underneath
-- will still be visible.  This instance will most likely only be used for
-- debugging.
instance Render AABB where
  render aabb =
    withColor4_ 1 1 1 0.5 $
    renderPrimitive Quads $ do
      let ((x1,x2),(y1,y2)) = aabbBounds aabb
      vertex2d x1 y2
      vertex2d x2 y2
      vertex2d x2 y1
      vertex2d x1 y1

aabbBounds :: AABB -> ((GLfloat,GLfloat),(GLfloat,GLfloat))
aabbBounds (AABB (Point x y) (Point w h)) = ((x,x+w),(y-h,y))

aabbOverlap :: AABB -> AABB -> Bool
aabbOverlap b1 b2 =
  rangeOverlap x1 x2 >= 0 && rangeOverlap y1 y2 >= 0
  where
  (x1,y1) = aabbBounds b1
  (x2,y2) = aabbBounds b2

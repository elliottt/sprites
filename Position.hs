module Position where

import Graphics
import Render

class HasPosition a where
  getPosition :: a -> Position
  setPosition :: Position -> a -> a

getX :: HasPosition a => a -> GLfloat
getX a = posX (getPosition a)

setX :: HasPosition a => GLfloat -> a -> a
setX x a = setPosition pos' a
  where
  pos  = getPosition a
  pos' = pos { posX = x }

getY :: HasPosition a => a -> GLfloat
getY a = posY (getPosition a)

setY :: HasPosition a => GLfloat -> a -> a
setY y a = setPosition pos' a
  where
  pos  = getPosition a
  pos' = pos { posY = y }

getRot :: HasPosition a => a -> GLfloat
getRot a = posRot (getPosition a)

setRot :: HasPosition a => GLfloat -> a -> a
setRot r a = setPosition pos' a
  where
  r' | r > 360   = r / 360
     | otherwise = r
  pos  = getPosition a
  pos' = pos { posRot = r' }

data Position = Position
  { posX   :: !GLfloat
  , posY   :: !GLfloat
  , posRot :: !GLfloat
  } deriving (Eq,Show)

instance HasPosition Position where
  getPosition p   = p
  setPosition p _ = p

applyPosition :: Position -> IO ()
applyPosition p = do
  translate (posX p) (posY p) 0
  rotate (posRot p) 0 0 1

moveBy :: HasPosition a => a -> Position -> a
moveBy a by = setPosition p' a
  where
  p  = getPosition a
  p' = p
    { posX   = posX p   + posX by
    , posY   = posY p   + posY by
    , posRot = posRot p + posRot by
    }

data At a = At
  { atPos  :: !Position
  , atData :: a
  }

instance HasPosition (At a) where
  getPosition        = atPos
  setPosition pos at = at { atPos = pos }

instance Render a => Render (At a) where
  render at = withMatrix $ do
    applyPosition (atPos at)
    render (atData at)

instance Update a => Update (At a) where
  update now at = update now (atData at)

data Rect = Rect
  { rectX :: !GLfloat
  , rectY :: !GLfloat
  , rectW :: !GLfloat
  , rectH :: !GLfloat
  } deriving (Eq,Show)

rectTopLeft :: Rect -> IO ()
rectTopLeft r = vertex2d (rectX r) (rectY r)

rectTopRight :: Rect -> IO ()
rectTopRight r = vertex2d (rectW r) (rectY r)

rectBottomRight :: Rect -> IO ()
rectBottomRight r = vertex2d (rectW r) (rectH r)

rectBottomLeft :: Rect -> IO ()
rectBottomLeft r = vertex2d (rectX r) (rectH r)

isOverlapping :: Rect -> Rect -> Bool
isOverlapping (Rect x1 y1 w1 h1) (Rect x2 y2 w2 h2) =
  x1 >= x2 && x1 <= w2 && y1 >= y2 && y1 <= h2

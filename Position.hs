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
  pos  = getPosition a
  pos' = pos { posRot = r }

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

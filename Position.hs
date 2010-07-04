module Position where

import Graphics

data Position = Position
  { posX   :: !GLfloat
  , posY   :: !GLfloat
  , posRot :: !GLfloat
  } deriving (Eq,Show)

applyPosition :: Position -> IO ()
applyPosition p = do
  translate (posX p) (posY p) 0

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

module Position where

import Graphics
import Math.Point
import Render

import Data.IORef (IORef,newIORef,readIORef,writeIORef)


-- Positions -------------------------------------------------------------------

data Position = Position
  { posX   :: !GLfloat
  , posY   :: !GLfloat
  , posRot :: !GLfloat
  } deriving (Eq,Show)

applyPosition :: Position -> IO ()
applyPosition p = do
  translate (posX p) (posY p) 0
  rotate (posRot p) 0 0 1

moveBy :: Position -> Position -> Position
moveBy by p = p
  { posX   = posX p   + posX by
  , posY   = posY p   + posY by
  , posRot = posRot p + posRot by
  }

incrX :: GLfloat -> Position -> Position
incrX x p = p { posX = posX p + x }

incrY :: GLfloat -> Position -> Position
incrY y p = p { posY = posY p + y }


-- Static Positions ------------------------------------------------------------

data At a = At
  { atPos  :: !Position
  , atData :: !a
  }

instance Functor At where
  fmap f at = at { atData = f (atData at) }

instance Render a => Render (At a) where
  render at = withMatrix $ do
    applyPosition (atPos at)
    render (atData at)

instance Update a => Update (At a) where
  update now at = update now (atData at)


-- Dynamic Positions -----------------------------------------------------------

data DynPos a = DynPos
  { dynPos  :: !(IORef Position)
  , dynData :: !a
  }

instance Functor DynPos where
  fmap f dyn = dyn { dynData = f (dynData dyn) }

instance Render a => Render (DynPos a) where
  render dyn = do
    pos <- readIORef (dynPos dyn)
    withMatrix $ do
      applyPosition pos
      render (dynData dyn)

instance Update a => Update (DynPos a) where
  update now dyn = update now (dynData dyn)

mkDynPos :: Position -> a -> IO (DynPos a)
mkDynPos pos a = do
  ref <- newIORef pos
  return $! DynPos
    { dynPos  = ref
    , dynData = a
    }

changePos :: (Position -> Position) -> DynPos a -> IO ()
changePos k dyn = do
  let ref = dynPos dyn
  pos <- readIORef ref
  writeIORef ref $! k pos

getDynPos :: DynPos a -> IO Position
getDynPos dyn = readIORef (dynPos dyn)


-- Rectangles ------------------------------------------------------------------

data Rect = Rect
  { rectX :: !GLfloat
  , rectY :: !GLfloat
  , rectW :: !GLfloat
  , rectH :: !GLfloat
  } deriving (Eq,Show)

rectTopLeft :: Rect -> Point
rectTopLeft r = Point (rectX r) (rectY r)

rectTopRight :: Rect -> Point
rectTopRight r = Point (rectW r) (rectY r)

rectBottomRight :: Rect -> Point
rectBottomRight r = Point (rectW r) (rectH r)

rectBottomLeft :: Rect -> Point
rectBottomLeft r = Point (rectX r) (rectH r)

rectQuads :: Rect -> (Rect,Rect,Rect,Rect)
rectQuads (Rect x y w h) =
  ( Rect x  y  w2 h2
  , Rect w2 y  w2 h2
  , Rect w2 h2 w2 h2
  , Rect x  h2 w2 h2 )
  where
  w2 = w / 2
  h2 = h / 2

bottomRight :: Rect -> (GLfloat,GLfloat)
bottomRight (Rect x y w h) = (x + w, y - h)

isOverlapping :: Rect -> Rect -> Bool
isOverlapping r1@(Rect x1 y1 _ _) r2@(Rect x2 y2 _ _) =
  not (x1 > w2 || x2 > w1 || y1 < h2 || y2 < h1)
  where
  (w1,h1) = bottomRight r1
  (w2,h2) = bottomRight r2

isWithin :: Position -> Rect -> Bool
isWithin (Position x1 y1 _) (Rect x2 y2 w2 h2) =
  x1 >= x2 && x1 <= r && y1 >= b && y1 <= y2
  where
  r = x2 + w2
  b = y2 - h2

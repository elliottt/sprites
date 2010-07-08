module Position where

import Graphics
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
  { dynData :: !a
  , dynPos  :: !(IORef Position)
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

mkDynPos :: a -> Position -> IO (DynPos a)
mkDynPos a pos = DynPos a `fmap` newIORef pos

changePos :: (Position -> Position) -> DynPos a -> IO ()
changePos k dyn = do
  let ref = dynPos dyn
  pos <- readIORef ref
  writeIORef ref $! k pos


-- Rectangles ------------------------------------------------------------------

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

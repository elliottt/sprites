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

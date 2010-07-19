module Animation (
    -- * Animations
    Animation, mkAnimation
  , reset
  , advance
  , seek
  , frame

    -- * Frames
  , Frame, mkFrame
  , applyFrame
  , Frames, mkFrames
  ) where

import Graphics
import Render
import Time

import Control.Monad (when)
import Data.Array (Array,listArray,(!))
import Data.IORef (IORef,newIORef,readIORef,writeIORef)
import qualified Graphics.Rendering.OpenGL.GL as GL


-- Animations ------------------------------------------------------------------

data Animation = Animation
  { animationFrames     :: !Frames
  , animationPosition   :: IORef (Int,Time)
  }

mkAnimation :: Frames -> IO Animation
mkAnimation fs = do
  ref <- newIORef (0,0)
  return Animation
    { animationFrames     = fs
    , animationPosition   = ref
    }

instance Update Animation where
  update now a = do
    let ref = animationPosition a
    (p,next) <- readIORef ref
    when (now > next) $ do
      let fs  = animationFrames a
          len = frameLength (animationFrames a)
          p'  = (p + 1) `rem` len
          f   = frameFrames fs ! p'
      writeIORef ref (p',now + frameDelay f)

-- | Reset the frame position
reset :: Animation -> IO ()
reset a = do
  let ref = animationPosition a
  (p,next) <- readIORef ref
  writeIORef ref (0,next)

-- | Advance the frame position by one.
advance :: Animation -> IO ()
advance a | len <= 1  = return ()
          | otherwise = update
  where
  len    = frameLength (animationFrames a)
  update = do
    let ref = animationPosition a
    (p,next) <- readIORef ref
    writeIORef ref ((p + 1) `rem` len, next)

seek :: Animation -> Int -> IO ()
seek a i = do
  let ref = animationPosition a
  (p,next) <- readIORef ref
  let fs = animationFrames a
  let i' | i < frameLength fs = i
         | otherwise          = p
  writeIORef ref (i',next)

frame :: Animation -> IO Frame
frame a = do
  let fs  = animationFrames a
  (p,_) <- readIORef (animationPosition a)
  return (frameFrames fs ! p)


-- Frames ----------------------------------------------------------------------

data Frame = Frame
  { frameTexture :: !Texture
  , frameDelay   :: !Interval
  }

mkFrame :: Texture -> Interval -> Frame
mkFrame t d = Frame
  { frameTexture = t
  , frameDelay   = d
  }

applyFrame :: Frame -> IO ()
applyFrame f = setTexture2d (frameTexture f)

data Frames = Frames
  { frameFrames :: !(Array Int Frame)
  , frameLength :: !Int
  }

mkFrames :: [Frame] -> Frames
mkFrames fs = Frames
  { frameFrames = listArray (0,len-1) fs
  , frameLength = len
  }
  where
  len = length fs

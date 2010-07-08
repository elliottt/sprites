module Animation (
    -- * Animations
    Animation, mkAnimation
  , stepAnimation
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
import Time

import Data.Array (Array,listArray,(!))
import qualified Graphics.Rendering.OpenGL.GL as GL


-- Animations ------------------------------------------------------------------

data Animation = Animation
  { animationFrames     :: !Frames
  , animationPosition   :: !Int
  , animationNextUpdate :: Time
  }

mkAnimation :: Frames -> Animation
mkAnimation fs = a
  where
  a = Animation
    { animationFrames     = fs
    , animationPosition   = 0
    , animationNextUpdate = frameDelay (frame a)
    }

stepAnimation :: Time -> Animation -> Animation
stepAnimation now a
  | now > animationNextUpdate a = a'
  | otherwise                   = a
  where
  a' = (advance a)
    { animationNextUpdate = addInterval (frameDelay (frame a')) now
    }

reset :: Animation -> Animation
reset a = a { animationPosition = 0 }

advance :: Animation -> Animation
advance a | len <= 1  = a
          | otherwise = a { animationPosition = pos' }
  where
  len  = frameLength (animationFrames a)
  pos' = (animationPosition a + 1) `rem` len

seek :: Animation -> Int -> Maybe Animation
seek a i | i < frameLength fs = Just (a { animationPosition = i })
         | otherwise          = Nothing
  where
  fs = animationFrames a

frame :: Animation -> Frame
frame a = frameFrames fs ! pos
  where
  fs  = animationFrames a
  pos = animationPosition a


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

module Animation (
    -- * Animations
    Animation, mkAnimation
  , reset
  , advance
  , seek
  , frame

    -- * Frames
  , Frames, mkFrames
  ) where

import Data.Array (Array,listArray,(!))
import qualified Graphics.Rendering.OpenGL.GL as GL


-- Animations ------------------------------------------------------------------

data Animation = Animation
  { animationFrames   :: !Frames
  , animationPosition :: !Int
  }

mkAnimation :: Frames -> Animation
mkAnimation fs = Animation
  { animationFrames   = fs
  , animationPosition = 0
  }

reset :: Animation -> Animation
reset a = a { animationPosition = 0 }

advance :: Animation -> Animation
advance a | len <= 1  = a
          | otherwise = a { animationPosition = pos' }
  where
  len  = frameLength (animationFrames a)
  pos' = (animationPosition a + 1) `quot` len

seek :: Animation -> Int -> Maybe Animation
seek a i | i < frameLength fs = Just (a { animationPosition = i })
         | otherwise          = Nothing
  where
  fs = animationFrames a

frame :: Animation -> Texture
frame a = frameTextures fs ! pos
  where
  fs  = animationFrames a
  pos = animationPosition a


-- Frames ----------------------------------------------------------------------

type Texture = GL.TextureObject

data Frames = Frames
  { frameTextures :: !(Array Int Texture)
  , frameLength   :: !Int
  }

mkFrames :: [Texture] -> Frames
mkFrames ts = Frames
  { frameTextures = listArray (0,len-1) ts
  , frameLength   = len
  }
  where
  len = length ts

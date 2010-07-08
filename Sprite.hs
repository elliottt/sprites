module Sprite (
    -- * Sprites
    Sprite, mkSprite
  ) where

import Animation
import Graphics
import Position
import Render
import Time

import Data.IORef (newIORef,IORef,readIORef,writeIORef)
import qualified Graphics.Rendering.OpenGL.GL as GL


-- Basic Sprites ---------------------------------------------------------------

data Sprite = Sprite
  { spriteAnimation :: IORef Animation
  , spriteRect      :: !Rect
  }

mkSprite :: Animation -> GLfloat -> GLfloat -> IO Sprite
mkSprite a w h = do
  let w2 = w / 2
      h2 = h / 2
      r  = Rect (-w2) h2 w2 (-h2)
  ref <- newIORef a
  return $! Sprite
    { spriteAnimation = ref
    , spriteRect      = r
    }

instance Render Sprite where
  render s = do
    a <- readIORef (spriteAnimation s)
    applyFrame (frame a)
    renderPrimitive Quads $ do
      let r = spriteRect s
      texCoord2d 0 0
      rectTopLeft r
      texCoord2d 1 0
      rectTopRight r
      texCoord2d 1 1
      rectBottomRight r
      texCoord2d 0 1
      rectBottomLeft r

instance Update Sprite where
  update now s = do
    let ref = spriteAnimation s
    a <- readIORef ref
    writeIORef ref $! stepAnimation now a

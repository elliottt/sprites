module Sprite (
    -- * Sprites
    Sprite, mkSprite
  , stepSprite
  ) where

import Animation
import Graphics
import Position
import Render

import qualified Graphics.Rendering.OpenGL.GL as GL


-- Basic Sprites ---------------------------------------------------------------

data Sprite = Sprite
  { spriteAnimation :: !Animation
  , spriteRect      :: !Rect
  }

mkSprite :: Animation -> GLfloat -> GLfloat -> Sprite
mkSprite a w h = Sprite
  { spriteAnimation = a
  , spriteRect      = r
  }
  where
  w2 = w / 2
  h2 = h / 2
  r  = Rect (-w2) h2 w2 (-h2)

stepSprite :: Sprite -> Sprite
stepSprite s = s
  { spriteAnimation = advance (spriteAnimation s)
  }

instance Render Sprite where
  render s = do
    setTexture2d (frame (spriteAnimation s))
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

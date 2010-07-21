module Sprite (
    -- * Sprites
    Sprite
  , mkSprite, mkSpriteRect, mkSpriteWidthHeight
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
  { spriteAnimation :: !Animation
  , spriteTL        :: !Point
  , spriteTR        :: !Point
  , spriteBR        :: !Point
  , spriteBL        :: !Point
  }

-- | Primitive sprite constructor.  Using this doesn't guarantee that the sprite
-- will end up as a rectangle.
mkSprite :: Animation -> Point -> Point -> Point -> Point -> Sprite
mkSprite a tl tr br bl = Sprite
  { spriteAnimation = a
  , spriteTL        = tl
  , spriteTR        = tr
  , spriteBR        = br
  , spriteBL        = bl
  }

-- | Make a sprite using a rectangle.
mkSpriteRect :: Animation -> Rect -> Sprite
mkSpriteRect a r = mkSprite a (rectTopLeft     r) (rectTopRight   r)
                              (rectBottomRight r) (rectBottomLeft r)

-- | Make a sprite using its width and height.
mkSpriteWidthHeight :: Animation -> GLfloat -> GLfloat -> Sprite
mkSpriteWidthHeight a w h = mkSpriteRect a r
  where
  w2 = w / 2
  h2 = h / 2
  r = Rect (-w2) h2 w2 (-h2)

instance Render Sprite where
  render s = do
    applyFrame =<< frame (spriteAnimation s)
    renderPrimitive Quads $ do
      texCoord2d 0 0
      point (spriteTL s)
      texCoord2d 1 0
      point (spriteTR s)
      texCoord2d 1 1
      point (spriteBR s)
      texCoord2d 0 1
      point (spriteBL s)

instance Update Sprite where
  update now s = update now (spriteAnimation s)

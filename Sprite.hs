module Sprite (
    -- * Sprites
    Sprite
  , mkSprite
  , spriteTL, spriteBR
  ) where

import Animation
import Graphics
import Math.Point
import Time

import Data.IORef (newIORef,IORef,readIORef,writeIORef)


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

instance Render Sprite where
  render s = do
    applyFrame =<< frame (spriteAnimation s)
    renderPrimitive Quads $ do
      texCoord2d 0 0
      render (spriteTL s)
      texCoord2d 1 0
      render (spriteTR s)
      texCoord2d 1 1
      render (spriteBR s)
      texCoord2d 0 1
      render (spriteBL s)

instance Update Sprite where
  update now s = update now (spriteAnimation s)

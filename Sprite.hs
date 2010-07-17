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

isOverlapping :: Rect -> Rect -> Bool
isOverlapping (Rect x1 y1 w1 h1) (Rect x2 y2 w2 h2) =
  x1 >= x2 && x1 <= w2 && y1 >= y2 && y1 <= h2


-- Basic Sprites ---------------------------------------------------------------

data Sprite = Sprite
  { spriteAnimation :: IORef Animation
  , spriteTL        :: !Point
  , spriteTR        :: !Point
  , spriteBR        :: !Point
  , spriteBL        :: !Point
  }

-- | Primitive sprite constructor.  Using this doesn't guarantee that the sprite
-- will end up as a rectangle.
mkSprite :: Animation -> Point -> Point -> Point -> Point -> IO Sprite
mkSprite a tl tr br bl = do
  ref <- newIORef a
  return $! Sprite
    { spriteAnimation = ref
    , spriteTL        = tl
    , spriteTR        = tr
    , spriteBR        = br
    , spriteBL        = bl
    }

-- | Make a sprite using a rectangle.
mkSpriteRect :: Animation -> Rect -> IO Sprite
mkSpriteRect a r = mkSprite a (rectTopLeft     r) (rectTopRight   r)
                              (rectBottomRight r) (rectBottomLeft r)

-- | Make a sprite using its width and height.
mkSpriteWidthHeight :: Animation -> GLfloat -> GLfloat -> IO Sprite
mkSpriteWidthHeight a w h = mkSpriteRect a r
  where
  w2 = w / 2
  h2 = h / 2
  r = Rect (-w2) h2 w2 (-h2)

instance Render Sprite where
  render s = do
    a <- readIORef (spriteAnimation s)
    applyFrame (frame a)
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
  update now s = do
    let ref = spriteAnimation s
    a <- readIORef ref
    writeIORef ref $! stepAnimation now a

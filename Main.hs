
import Animation
import Graphics
import Position
import Render
import Sprite
import Time

import Control.Monad (forever)

import qualified Graphics.Rendering.OpenGL.GL as GL

main = do
  initGraphics "Test" 800 600
  t  <- loadTexture "test.png"
  t2 <- loadTexture "over9000-6.jpg"
  let a   = mkAnimation (mkFrames [mkFrame t 1000, mkFrame t2 1000])
      rot = Position 0 0 0.5
  sp  <- mkSpriteWidthHeight a 2 2
  dyn <- mkDynPos (Position 0 0 0) =<< mkDynPos (Position 1 1 0) sp

  forever $ do
    now <- getTicks
    update now dyn
    changePos (moveBy rot) dyn
    changePos (moveBy rot) (dynData dyn)

    clearScreen
    translate 0 0 (-6)
    render dyn
    updateScreen


import Animation
import Graphics
import Sprite

import Control.Monad (forever)

main = do
  initGraphics "Test" 800 600
  t <- loadTexture "NeHe.bmp"
  let a = mkAnimation (mkFrames [t])
      s = mkSprite a 2 2
  forever $ do
    clear

    translate 0 0 (-6)

    drawSprite s

    update

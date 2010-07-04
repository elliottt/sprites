
import Animation
import Graphics
import Position
import Render
import Sprite

import Control.Monad (forever)

main = do
  initGraphics "Test" 800 600
  t <- loadTexture "test.png"
  let a  = mkAnimation (mkFrames [t])
      sa = At
        { atData = mkSprite a 2 2
        , atPos  = Position 1 1 0
        }

      loop at p = do
        clear

        withMatrix $ do
          translate 0 0 (-6)
          applyPosition p
          scale 0.5 0.5 0.5
          render at

        update

        let at' = setRot ((getRot at) + 0.5) at
            p'  = setRot (getRot p + 0.5) p

        loop at' p'

  loop sa (Position 0 0 0)

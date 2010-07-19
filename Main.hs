
import Animation
import Event
import Graphics
import Position
import Render
import Sprite
import Time

import System.Exit (exitSuccess)

import qualified Graphics.Rendering.OpenGL.GL as GL

main = do
  initGraphics "Test" 800 600
  t  <- loadTexture "test.png"
  t2 <- loadTexture "over9000-6.jpg"
  a  <- mkAnimation (mkFrames [mkFrame t 1000, mkFrame t2 1000])
  let rot = Position 0 0 0.5
  let sp  = mkSpriteWidthHeight a 2 2
  dyn <- mkDynPos (Position 0 0 0) =<< mkDynPos (Position 1 1 0) sp

  withEventManager $ \em -> do
    em `listen` \ QuitEvent -> exitSuccess
    em `listen` \ (TickEvent now delta) -> do
      update now a

      changePos (moveBy rot) dyn
      changePos (moveBy rot) (dynData dyn)

      clearScreen
      translate 0 0 (-6)
      render dyn
      updateScreen

    em `listen` \ (KeyUp sym) -> do
      putStr "Key up: "
      print sym

    em `listen` \ (KeyDown sym) -> do
      putStr "Key down: "
      print sym

    em `listen` (print :: MouseMotion     -> IO ())
    em `listen` (print :: MouseButtonUp   -> IO ())
    em `listen` (print :: MouseButtonDown -> IO ())

    eventLoop em

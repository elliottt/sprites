module Test where

import Animation
import Event
import Dungeon
import Graphics
import Position
import Render
import Sprite
import Time

import System.Exit (exitSuccess)

import qualified Graphics.Rendering.OpenGL.GL as GL

main = do
  initGraphics "Test" 800 600
  land  <- loadTexture "land.png"
  water <- loadTexture "water.png"

  landA  <- mkAnimation (mkFrames [mkFrame land  0])
  waterA <- mkAnimation (mkFrames [mkFrame water 0])
  let landSp  = mkSpriteWidthHeight landA  0.1 0.1
      waterSp = mkSpriteWidthHeight waterA 0.1 0.1

  d <- newDungeon Sea (50,50) binaryCell
  improveDungeonMany Sea (improveTest 4) 6000 d

  let mkTile _ (x,y) c = do
        let pos d = At
              { atData = d
              , atPos  = Position (fromIntegral x * 0.1)
                                  (fromIntegral y * 0.1)
                                  0
              }
        case c of
          Land -> return (pos landSp)
          Sea  -> return (pos waterSp)

  d' <- mapDungeon Sea mkTile d

  view <- mkDynPos (Position (-2.5) (-2.5) 0) ()

  withEventManager $ \em -> do
    em `listen` \ QuitEvent -> exitSuccess

    em `listen` \ (TickEvent now delta) -> do
      clearScreen
      translate 0 0 (-4)
      applyPosition =<< getDynPos view
      render d'
      updateScreen

    em `listen` \ (KeyDown sym) -> do
      case symKey sym of
        SDLK_UP     -> changePos (incrY (-0.1)) view
        SDLK_DOWN   -> changePos (incrY   0.1)  view
        SDLK_LEFT   -> changePos (incrX   0.1)  view
        SDLK_RIGHT  -> changePos (incrX (-0.1)) view
        SDLK_ESCAPE -> exitSuccess
        _           -> return ()

    eventLoop em

{-# LANGUAGE FlexibleInstances #-}

module Test where

import Animation
import Event
import Dungeon
import Graphics
import Position
import QuadTree as QuadTree
import Render
import Sprite
import Time

import System.Exit (exitSuccess)

import qualified Graphics.Rendering.OpenGL.GL as GL

instance HasBounds (At Sprite) where
  getBounds at = Rect x y (x1 - x0) (y1 - y0)
    where
    Position x y _ = atPos at
    Point x0 y0    = spriteTL (atData at)
    Point x1 y1    = spriteBR (atData at)

main = do
  initGraphics "Test" 800 600
  land  <- mkAnimationFromFile "images/land.anim"
  water <- mkAnimationFromFile "images/water.anim"

  let landSp  = mkSpriteWidthHeight land  0.1 0.1
      waterSp = mkSpriteWidthHeight water 0.1 0.1

  d <- newDungeon Sea (100,100) binaryCell
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

      step lkp pos t c = do
        sp <- mkTile lkp pos c
        case QuadTree.insert sp t of
          Nothing -> fail ("Unable to insert " ++ show pos ++ " into QuadTree")
          Just t' -> return t'

  d' <- foldDungeon Sea step (emptyQuadTree (Rect 0 10 10 10)) d

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

{-# LANGUAGE FlexibleInstances #-}

module Test where

import Event
import Graphics
import Math.Point
import Physics.Body
import Physics.Shape
import Physics.Vector
import Physics.World

import Data.IORef (newIORef,writeIORef,readIORef)
import Data.Maybe (fromJust)
import System.Exit (exitSuccess)

main = do
  initGraphics "Test" 800 600

  let ground = mkPhysicalState $ fromJust $ rectangle (Point 0 0) 10 0.1
  let square = mkPhysicalState $ fromJust $ rectangle (Point 0 10) 1 1
  let world  = (emptyWorld 1000 1000)
        { worldGravity = Just (Vector 0 (-0.1))
        }

  ref <- newIORef $ addBody ground { psStatic = True }
                  $ addBody square
                    world

  withEventManager $ \em -> do

    setLineWidth 2
    setPointSize 2
    color3 1 1 1

    em `listen` \ QuitEvent -> exitSuccess

    em `listen` \ (TickEvent now delta) -> do
      w <- readIORef ref
      let w' = stepWorld delta w
      writeIORef ref w'

      clearScreen
      translate 0 (-5) (-20)
      render w'
      updateScreen

    eventLoop em

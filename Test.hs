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

  let ground = staticBody $ fromJust $ rectangle (Point 0 0) 10 0.1
      wall   = staticBody $ fromJust $ rectangle (Point (-5) 5) 0.1 10
  let square = dynamicBody $ fromJust $ rectangle (Point 0 5) 1 1
  let s2     = dynamicBody $ fromJust $ rectangle (Point 2 5) 1 1
  let world  = (emptyWorld 1000 1000)
        { worldGravity     = Just (Vector 0 (-0.1))
        }

  ref <- newIORef $ addBody ground { psStatic = True }
                  $ addBody wall   { psStatic = True }
                  $ addBody (setDebug True $ applyImpulse (Vector 0.01 0)
                                           $ setRestitution 0.8 square)
                  -- $ addBody s2
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

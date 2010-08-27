{-# LANGUAGE FlexibleInstances #-}

module Test where

import Event
import Graphics
import Math.AffinePlane
import Physics.Body
import Physics.Shape
import Physics.World

import Data.IORef (newIORef,writeIORef,readIORef)
import System.Exit (exitSuccess)

main = do
  initGraphics "Test" 800 600

  ground <- staticBody  =<< rectangle (Point 0 0) 10 0.1
  wall   <- staticBody  =<< rectangle (Point (-5) 5) 0.1 10
  square <- dynamicBody =<< rectangle (Point 0 5) 1 1
  s2     <- dynamicBody =<< rectangle (Point 2 5) 1 1
  let world = (emptyWorld 1000 1000)
        { worldGravity     = Just (Vector 0 (-0.5))
        }

  ref <- newIORef $ addBody ground { psStatic = True }
                  $ addBody wall   { psStatic = True }
                  $ addBody (setDebug True $ applyImpulse (Vector 0.01 0)
                                           $ setRestitution 0.8 square)
                  $ addBody s2
                    world

  withEventManager $ \em -> do

    setLineWidth 2
    setPointSize 2
    color3 1 1 1

    em `listen` \ QuitEvent -> exitSuccess

    em `listen` \ t@(TickEvent now delta) -> do
      w' <- stepWorld delta =<< readIORef ref
      writeIORef ref w'

      clearScreen
      translate 0 (-5) (-20)
      render w'
      updateScreen

    eventLoop em

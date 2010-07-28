{-# LANGUAGE FlexibleInstances #-}

module Test where

import Event
import Graphics
import Math.Matrix
import Math.Point
import Math.Polygon
import World

import Data.IORef (newIORef,writeIORef,readIORef)
import System.Exit (exitSuccess)

main = do
  initGraphics "Test" 800 600

  let r1 = movePolygon (Point (-1) 0) (rectangle 1 1)
      s0 = State
        { sVelocity     = 0
        , sAcceleration = 0
        , sMass         = 0.5
        , sFriction     = 0
        }

      r2 = movePolygon (Point 1 0) (rectangle 1 1)

  world <- newIORef
    [ BPolygon r1 s0 { sAcceleration = Point 0.05 0, sMass = 1}
    , BPolygon r2 s0
    ]

  withEventManager $ \em -> do

    setLineWidth 4
    setPointSize 4
    color3 1 1 1

    em `listen` \ QuitEvent -> exitSuccess

    em `listen` \ (TickEvent now delta) -> do

      w' <- stepWorld delta =<< readIORef world
      writeIORef world w'

      clearScreen
      translate 0 0 (-10)
      render w'
      updateScreen

    eventLoop em

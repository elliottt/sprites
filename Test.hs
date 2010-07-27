{-# LANGUAGE FlexibleInstances #-}

module Test where

import Event
import Graphics
import Math.Point
import Math.Polygon
import Render
import World

import Data.IORef (newIORef,writeIORef,readIORef)
import System.Exit (exitSuccess)

main = do
  initGraphics "Test" 800 600

  let body = toBody ConvexBody
        { cbPolygon      = rectangle 1 1
        , cbFriction     = 0
        , cbMass         = 1.0
        , cbVelocity     = Point 0.01 0.01
        , cbAcceleration = Point 0.0 0.0
        }

  world <- newIORef [body]

  withEventManager $ \em -> do

    setLineWidth 4
    color3 1 1 1

    em `listen` \ QuitEvent -> exitSuccess

    em `listen` \ (TickEvent now delta) -> do

      w <- readIORef world
      let w' = stepWorld delta w
      writeIORef world w'

      clearScreen
      translate 0 0 (-10)
      render w'
      updateScreen

    eventLoop em

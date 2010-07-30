{-# LANGUAGE FlexibleInstances #-}

module Test where

import Event
import Graphics

import System.Exit (exitSuccess)

main = do
  initGraphics "Test" 800 600

  withEventManager $ \em -> do

    setLineWidth 4
    setPointSize 4
    color3 1 1 1

    em `listen` \ QuitEvent -> exitSuccess

    em `listen` \ (TickEvent now delta) -> do
      clearScreen
      translate 0 0 (-10)
      updateScreen

    eventLoop em

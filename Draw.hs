module Draw where

import Animation
import Event
import Graphics
import Position
import Sprite
import Time
import Dungeon

import System.Exit (exitSuccess)

import qualified Data.Map as Map
import Data.Array.IO

import Debug.Trace


fileName (nw,ne,sw,se) = "./images/tiles/" ++ map ch [nw,ne,sw,se] ++ ".png"
  where ch Sea  = 's'
        ch Land = 'l'

-- not animated, for the moment
loadSprite f =
  do t <- loadTexture f
     a <- mkAnimation (mkFrames [mkFrame t 0])
     return (mkSpriteWidthHeight a 1 1)

loadSpriteDir d =
  do s <- loadSprite (fileName d)
     return (d,s)

loadSprites = Map.fromList `fmap` mapM loadSpriteDir dirs
  where dirs  = [ (nw,ne,sw,se) | nw <- ter, ne <- ter, sw <- ter, se <- ter ]
        ter   = [ Land, Sea ]


drawSprite s (x,y) = render $ At (Position x y 0) s
  
drawCell s d pos p@(x,y) =
  do nw <- readArray d (x,y)
     ne <- readArray d (x+1,y)
     sw <- readArray d (x,y+1)
     se <- readArray d (x+1,y+1)
     case Map.lookup (nw,ne,sw,se) s of
       Just sp -> drawSprite sp (pos p)
       Nothing -> return ()
    
drawDun s d =
  do ((x0,y0),(x1,y1)) <- getBounds d
     let x2 = (x1 - x0) `div` 2
         y2 = (y1 - y0) `div` 2
         pos (x,y) = (fromIntegral (x - x2), fromIntegral (y2 - y))
     mapM_ (drawCell s d pos) $
        [ (i,j) | i <- [x0 .. x1 - 1], j <- [ y0 .. y1 - 1 ] ]


main = do
  initGraphics "Test" 800 600
  m <- loadSprites
  d <- newDungeon Sea (16,16) binaryCell
  improveDungeonMany Sea (improveTest 4) 1000 d
  
  clearScreen
  translate 0 0 (-16)
  drawDun m d
  updateScreen

  withEventManager $ \em -> do
    em `listen` \ QuitEvent -> exitSuccess
    eventLoop em

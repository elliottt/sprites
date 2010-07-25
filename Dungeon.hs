{-# LANGUAGE TypeSynonymInstances #-}

module Dungeon where

import Render hiding (update)

import Control.Monad
import Data.Array.IO
import Data.List
import System.Random

-- Cell Positions --------------------------------------------------------------

type AbsPos = (Int,Int)
type RelPos = (Int,Int)

relToAbs :: AbsPos -> RelPos -> AbsPos
relToAbs (x0,y0) (dx,dy) = (x0 + dx, y0 + dy)


-- Dungeons --------------------------------------------------------------------

type Dungeon a = IOArray AbsPos a

type Improve a = Lookup a -> a -> IO a

type Probability a = [(Int,a)]

type Lookup a = RelPos -> IO a

newDungeon :: a -> (Int,Int) -> Probability a -> IO (Dungeon a)
newDungeon z dim prob = do
  let is = ((1,1),dim)
  d <- newArray is z
  forM_ (range is) (update d (replace prob))
  return d

update :: Dungeon a -> (a -> IO a) -> AbsPos -> IO ()
update arr f pos = writeArray arr pos =<< f =<< readArray arr pos

replace :: Probability a -> a -> IO a
replace prob c = do
  p <- randomRIO (1,100)
  return $ maybe c snd $ find ((< p) . fst) prob

pickCell :: Dungeon a -> IO AbsPos
pickCell d = do
  (_,(w,h)) <- getBounds d
  x <- randomRIO (1,w)
  y <- randomRIO (1,h)
  return (x,y)

readRel :: a -> Dungeon a -> AbsPos -> Lookup a
readRel z d pos rel = do
  bounds <- getBounds d
  let a = relToAbs pos rel
  if inRange bounds a
     then readArray d a
     else return z

improveDungeon :: a -> Improve a -> Dungeon a -> IO ()
improveDungeon c improve d = do
  pos <- pickCell d
  update d (improve (readRel c d pos)) pos

improveDungeonMany :: a -> Improve a -> Int -> Dungeon a -> IO ()
improveDungeonMany c improve i d =
  replicateM_ i (improveDungeon c improve d)

printDungeon :: (a -> Char) -> Dungeon a -> IO ()
printDungeon draw d = do
  cells <- getAssocs d
  let eqRow ((x1,_),_) ((x2,_),_) = x1 == x2
      putRow row = putStrLn (map (draw . snd) row)
  mapM_ putRow (groupBy eqRow cells)

mapDungeon :: a -> (Lookup a -> AbsPos -> a -> IO b) -> Dungeon a
           -> IO (Dungeon b)
mapDungeon z step d = do
  bounds <- getBounds d
  arr    <- newArray_ bounds
  forM_ (range bounds) $ \ ix ->
    writeArray arr ix =<< step (readRel z d ix) ix =<< readArray d ix
  return arr

foldDungeon :: a -> (Lookup a -> AbsPos -> b -> a -> IO b) -> b
            -> Dungeon a -> IO b
foldDungeon z f b0 d = do
  bounds <- getBounds d
  let step b ix = do
        a  <- readArray d ix
        f (readRel z d ix) ix b a
  foldM step b0 (range bounds)

instance Render a => Render (Dungeon a) where
  render d = mapM_ render =<< getElems d


-- Example ---------------------------------------------------------------------

data Cell = Land | Sea deriving (Eq,Ord,Show)

printCell :: Cell -> Char
printCell Land = '#'
printCell Sea  = ' '

opposite :: Cell -> Cell
opposite Land = Sea
opposite Sea  = Land

randomCell :: IO Cell
randomCell  = do
  b <- randomIO
  if b
     then return Land
     else return Sea

binaryCell :: [(Int,Cell)]
binaryCell  = [(50,Land)]

type Threshold = Int

improveTest :: Threshold -> Improve Cell
improveTest n readCell c = do
  ns <- neighbors readCell
  let lands = length (filter (== Land) ns)
  case c of
    Land | lands >= n -> return Land
         | otherwise  -> return Sea
    Sea  | lands <= n -> return Sea
         | otherwise  -> return Land

improveTrivial :: Improve Cell
improveTrivial _ c = return c

neighbors :: (RelPos -> IO Cell) -> IO [Cell]
neighbors readCell = mapM readCell
                   $ filter (/= (0,0)) [ (x,y) | x <- [-1..1], y <- [-1..1] ]

testDungeon :: (Int,Int) -> Improve Cell -> Int -> IO ()
testDungeon dim imp i = do
  d <- newDungeon Sea dim binaryCell
  improveDungeonMany Sea imp i d
  printDungeon printCell d

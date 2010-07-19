module Dungeon where

import Control.Monad
import Data.Array.IO
import Data.Function
import Data.List
import System.Random

data Cell = Land | Sea deriving (Eq,Show)

opposite :: Cell -> Cell
opposite Land = Sea
opposite Sea  = Land

randomCell :: IO Cell
randomCell  = do
  b <- randomIO
  if b
     then return Land
     else return Sea

type Dungeon = IOArray (Int,Int) Cell

newDungeon :: (Int,Int) -> Probability -> IO Dungeon
newDungeon dim prob = do
  let is = ((1,1),dim)
  d <- newArray is Sea
  forM_ (range is) (update d (replace prob))
  return d

update :: Dungeon -> (Cell -> IO Cell) -> (Int,Int) -> IO ()
update arr f pos = writeArray arr pos =<< f =<< readArray arr pos

type Improve = ((Int,Int) -> IO Cell) -> Cell -> IO Cell

type Probability = [(Int,Cell)]

replace :: Probability -> Cell -> IO Cell
replace prob c = do
  p <- randomRIO (1,100)
  return $ maybe c snd $ find ((< p) . fst) prob

pickCell :: Dungeon -> IO (Int,Int)
pickCell d = do
  (_,(w,h)) <- getBounds d
  x <- randomRIO (1,w)
  y <- randomRIO (1,h)
  return (x,y)

relToAbs :: AbsPos -> RelPos -> AbsPos
relToAbs (x0,y0) (dx,dy) = (x0 + dx, y0 + dy)

type RelPos = (Int,Int)
type AbsPos = (Int,Int)

readRel :: Cell -> Dungeon -> AbsPos -> RelPos -> IO Cell
readRel z d pos rel = do
  bounds <- getBounds d
  let a = relToAbs pos rel
  if inRange bounds a
     then readArray d a
     else return z

improveDungeon :: Cell -> Improve -> Dungeon -> IO ()
improveDungeon c improve d = do
  pos <- pickCell d
  update d (improve (readRel c d pos)) pos

drawCell :: Cell -> Char
drawCell Land = '#'
drawCell Sea  = ' '

improveDungeonMany :: Cell -> Improve -> Int -> Dungeon -> IO ()
improveDungeonMany c improve i d =
  replicateM_ i (improveDungeon c improve d)

printDungeon :: Dungeon -> IO ()
printDungeon d = do
  cells <- getAssocs d
  let rows = map (map (drawCell . snd)) (groupBy ((==) `on` (fst.fst)) cells)
  mapM_ putStrLn rows

-- -----------------------------------------------------------------------------

binaryCell :: [(Int,Cell)]
binaryCell  = [(50,Land)]

type Threshold = Int

improveTest :: Threshold -> Improve
improveTest n readCell c = do
  ns <- neighbors readCell
  let lands = length (filter (== Land) ns)
  case c of
    Land | lands >= n -> return Land
         | otherwise  -> return Sea
    Sea  | lands <= n -> return Sea
         | otherwise  -> return Land

improveTrivial :: Improve
improveTrivial _ c = return c

neighbors :: (RelPos -> IO Cell) -> IO [Cell]
neighbors readCell = mapM readCell
                   $ filter (/= (0,0)) [ (x,y) | x <- [-1..1], y <- [-1..1] ]

testDungeon :: (Int,Int) -> Improve -> Int -> IO ()
testDungeon dim imp i = do
  d <- newDungeon dim binaryCell
  improveDungeonMany Sea imp i d
  printDungeon d

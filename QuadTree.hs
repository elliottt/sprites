{-# LANGUAGE FlexibleInstances #-}

module QuadTree where

import Graphics
import Position

import Control.Applicative (Applicative(..),Alternative(..),(<$>))
import Control.Monad (guard)

data QuadTree a
  = Node !Rect (QuadTree a) (QuadTree a) (QuadTree a) (QuadTree a)
  | Leaf !Rect [a]
    deriving Show

emptyQuadTree :: Rect -> QuadTree a
emptyQuadTree bounds = Leaf bounds []

class HasBounds a where
  getBounds :: a -> Rect

instance HasBounds Rect where
  getBounds = id

insert :: HasBounds a => a -> QuadTree a -> Maybe (QuadTree a)
insert a = loop
  where
  r   = getBounds a
  pos = Position (rectX r) (rectY r) 0

  loop t = do
    let loop' t' = loop t' <|> return t'
    case t of

      Node r tl tr br bl -> do
        guard (pos `isWithin` r)
        Node r <$> loop' tl <*> loop' tr <*> loop' br <*> loop' bl

      Leaf r as -> do
        guard (pos `isWithin` r)
        if length as < 1
           then return (Leaf r (a:as))
           else do
             let (tl,tr,br,bl) = rectQuads r
             loop' $ Node r (emptyQuadTree tl) (emptyQuadTree tr)
                            (emptyQuadTree br) (emptyQuadTree bl)

viewable :: HasBounds a => Rect -> QuadTree a -> [a]
viewable r t =
  case t of

    Node r' tl tr br bl | r `isOverlapping` r' ->
      viewable r tl ++ viewable r tr ++ viewable r br ++ viewable r bl

    Leaf r' as | r `isOverlapping` r' ->
      [ a | a <- as, r `isOverlapping` getBounds a ]

    _ -> []

instance (Render a, HasBounds a) => Render (QuadTree a) where
  render qt = do
    mapM_ render (viewable (Rect 0 1 1 1) qt)

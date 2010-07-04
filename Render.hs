module Render where

class Render a where
  render :: a -> IO ()

instance Render a => Render (Maybe a) where
  render Nothing  = return ()
  render (Just a) = render a

instance Render a => Render [a] where
  render = mapM_ render

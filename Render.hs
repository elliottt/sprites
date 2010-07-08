module Render where

import Time

-- | Things that can be rendered.
class Render a where
  render :: a -> IO ()

instance Render a => Render (Maybe a) where
  render Nothing  = return ()
  render (Just a) = render a

instance Render a => Render [a] where
  render = mapM_ render


-- | Things that have some mutable state, that depends on time.
class Update a where
  update :: Interval -> a -> IO ()

instance Update a => Update (Maybe a) where
  update _  Nothing  = return ()
  update dt (Just a) = update dt a

instance Update a => Update [a] where
  update dt = mapM_ (update dt)

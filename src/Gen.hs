{-# LANGUAGE TupleSections #-}
{-# LANGUAGE MultiParamTypeClasses #-}

module Gen (
    module Gen
  , R.StdGen, R.mkStdGen, R.newStdGen
  ) where

import MonadLib
import qualified System.Random as R

newtype Gen a = Gen { unGen :: R.StdGen -> IO (a,R.StdGen) }

runGen :: R.StdGen -> Gen a -> IO (a,R.StdGen)
runGen g (Gen k) = k g

instance Functor Gen where
  fmap f (Gen k) = Gen $ \g -> do
    (a,g') <- k g
    return (f a, g')

instance Monad Gen where
  return x = Gen (\g -> return (x,g))
  m >>= f  = Gen $ \g -> do
    (a,g') <- runGen g m
    runGen g' (f a)

instance BaseM Gen IO where
  inBase m = Gen (\g -> (,g) `fmap` m)

random :: R.Random a => Gen a
random  = Gen (return . R.random)

between :: R.Random a => (a,a) -> Gen a
between r = Gen (return . R.randomR r)

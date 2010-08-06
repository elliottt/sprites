{-# LANGUAGE TypeSynonymInstances #-}

module Tests where

import AffinePlane

import Control.Monad (ap)
import Test.QuickCheck

instance Integral a => Arbitrary (Point a) where
  arbitrary = Point `fmap` arbitrarySizedIntegral
                    `ap`   arbitrarySizedIntegral

instance Integral a => Arbitrary (Vector a) where
  arbitrary = Vector `fmap` arbitrarySizedIntegral
                     `ap`   arbitrarySizedIntegral

runTests = mapM_ run tests
  where
  tests     = theorems ++ identities
  run (n,m) = label n >> m
  label n   = do
    let len = length n
    putStr "-- "
    putStr n
    putStr " "
    putStrLn (replicate (76 - len) '-')

theorems =
  [ ("prop_HeadToTail",   quickCheck prop_HeadToTail)
  , ("prop_DotSymmetric", quickCheck prop_DotSymmetric)
  , ("prop_DotBiLinear",  quickCheck prop_DotBiLinear)
  , ("prop_DotPositive",  quickCheck prop_DotPositive)
  ]

prop_HeadToTail p q r = (p -. q) +^ (q -. r) == p -. r

prop_DotSymmetric u v = u <.> v == v <.> u

prop_DotBiLinear :: Int -> Vector Int -> Int -> Vector Int -> Vector Int -> Bool
prop_DotBiLinear a u b v w =
  (a *^ u) +^ (b *^ v) <.> w == a * (u <.> w) + b * (v <.> w)

prop_DotPositive v = v /= zeroV ==> v <.> v >= 0


identities =
  [ ("prop_SubIdent",         quickCheck prop_SubIdent)
  , ("prop_NegSubIdent",      quickCheck prop_NegSubIdent)
  , ("prop_VecAddSubIdent",   quickCheck prop_VecAddSubIdent)
  , ("prop_PointVecSubIdent", quickCheck prop_PointVecSubIdent)
  , ("prop_PointAddSubIdent", quickCheck prop_PointAddSubIdent)
  , ("prop_PointVecSubAdd",   quickCheck prop_PointVecSubAdd)
  ]

prop_SubIdent q = q -. q == zeroV

prop_NegSubIdent r q = r -. q == negV (q -. r)

prop_VecAddSubIdent v q r = v +^ (q -. r) == (q .+^ v) -. r

prop_PointVecSubIdent q r v = q -. (r .+^ v) == (q -. r) -^ v

prop_PointAddSubIdent p q = p == q .+^ (p -. q)

prop_PointVecSubAdd q v r w = (q .+^ v) -. (r .+^ w) == (q -. r) +^ (v -^ w)

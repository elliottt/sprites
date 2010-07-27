{-# LANGUAGE ExistentialQuantification #-}
{-# LANGUAGE DeriveDataTypeable #-}

module World where

import Graphics
import Math.Matrix
import Math.Point
import Math.Polygon
import Render
import Time

import Data.Typeable (Typeable,cast)
import Graphics.Rendering.OpenGL.GL (GLfloat)


-- World Bodies ----------------------------------------------------------------

class (Render b, Typeable b) => Body b where
  toBody :: b -> SomeBody
  toBody  = SomeBody

  fromBody :: SomeBody -> Maybe b
  fromBody (SomeBody b) = cast b

  step :: Interval -> b -> b

data SomeBody = forall b. Body b => SomeBody b deriving Typeable

instance Render SomeBody where
  render (SomeBody b) = render b

instance Body SomeBody where
  toBody   = id
  fromBody = Just
  step dt (SomeBody b) = SomeBody (step dt b)

data ConvexBody = ConvexBody
  { cbPolygon      :: !Polygon
  , cbFriction     :: !GLfloat
  , cbMass         :: !GLfloat
  , cbVelocity     :: !Vector
  , cbAcceleration :: !Vector
  } deriving (Show,Typeable)

instance Render ConvexBody where
  render cb = renderPrimitive Lines (mapM_ render (polyEdges (cbPolygon cb)))

instance Body ConvexBody where
  step dt b = b
    { cbPolygon  = movePolygon v' (cbPolygon b)
    , cbVelocity = v'
    }
    where
    v' = cbVelocity b + scaleVector (fromIntegral dt) (cbAcceleration b)


-- World Management ------------------------------------------------------------

type World = [SomeBody]

stepWorld :: Interval -> World -> World
stepWorld dt = map (step dt)

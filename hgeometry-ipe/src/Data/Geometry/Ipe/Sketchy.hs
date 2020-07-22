{-# LANGUAGE TemplateHaskell #-}
--------------------------------------------------------------------------------
-- |
-- Module      :  Data.Geometry.Ipe.Sketchy
-- Copyright   :  (C) Frank Staals
-- License     :  see the LICENSE file
-- Maintainer  :  Frank Staals
--
-- Sketchy style rendering of primitives based on
--
-- <https://openaccess.city.ac.uk/id/eprint/1274/1/wood_sketchy_2012.pdf Sketchy Rendering for Information Visualization>
-- by Jo Wood,Member, Petra Isenberg, Tobias Isenberg,,Jason Dykes,
-- Nadia Boukhelifa and Aidan Slingsby,
--
--
--------------------------------------------------------------------------------
module Data.Geometry.Ipe.Sketchy where

import           Control.Lens
import           Control.Monad.Random.Class
import           Data.Ext
import           Data.Geometry.CatmulRomSpline (Spline)
import qualified Data.Geometry.CatmulRomSpline as Spline
import           Data.Geometry.LineSegment
import           Data.Geometry.Point
import           Data.Geometry.Polygon
import           Data.Geometry.Vector
import           Data.List.NonEmpty (NonEmpty(..))
import qualified Data.List.NonEmpty as NonEmpty
import           Data.Util
import           System.Random (Random(..))

import           Control.Monad.Random

--------------------------------------------------------------------------------
-- * Rendering Line Segments

-- | Renders a line segment using a Sketchy style.
--
-- as in the paper, we render a segment using two strokes.
sketch     :: (MonadRandom m, Floating r, Random r, Ord r)
           => r -> LineSegment 2 p r -> m (Two (Spline 2 r))
sketch r s = Two <$> sketchSegment r s <*> sketchSegment r s

-- | Renders a line segment in a 'sketchy' style using a Catmul-rom spline.
sketchSegment                                      :: (MonadRandom m, Floating r, Random r, Ord r)
                                                   => r -> LineSegment 2 p r -> m (Spline 2 r)
sketchSegment r s@(LineSegment' (p :+ _) (q :+ _)) =
    Spline.fromPoints <$> perturb r p <*> a <*> b  <*> perturb r q
  where
    v = q .-. p
    u = signorm . perp $ v  -- perpendicular vector to v


    -- offset for the midpoint perpendicular to the segment by (1/200)^th of the length
    da = let d = 1 / 200 in (*^ u) <$> getRandomR (-d,d)
    -- offset for the 3/4th point, in the range r perpendicular to the
    -- segment and in the range (1/10)^th times the length in the
    -- direction along the segment.
    db = let d = 1 / 10
         in (\du dv -> du *^ u ^+^ dv *^ v) <$> getRandomR (-r,r) <*> getRandomR (-d,d)

    a = (interpolate (1/2) s .+^) <$> da
    b = (interpolate (3/4) s .+^) <$> db

--------------------------------------------------------------------------------
-- * Rendering Arcs


-- sketchArc :: r ->

--------------------------------------------------------------------------------
-- * Hachures

data HachureSettings r = HachureSettings { _direction :: Vector 2 r
                                         -- ^ Direction of the hachures
                                         , _distance  :: r
                                         -- ^ distance between the hachures
                                         }
                       deriving (Show,Eq)
makeLenses ''HachureSettings

-- | Compute Hachures for a polygon
hachure          :: (MonadRandom m, Floating r, Random r, Ord r)
                 => r -> HachureSettings r -> Polygon t p r -> m (NonEmpty (Spline 2 r))
hachure r hs pg = undefined
  -- run a sweep, three types of events: insert segment, delete segment, create hachure




data Event r = Insert (LineSegment 2 () r)
             | Delete (LineSegment 2 () r)
             | Hachure
             deriving (Show,Eq)

--------------------------------------------------------------------------------
-- * Helper Functions

-- | Perpendicular vector
perp                  :: Num r => Vector 2 r -> Vector 2 r
perp (Vector2 vx vy) = Vector2 (-vy) vx

-- | Perturbs a point
perturb     :: (MonadRandom m, Random r, Num r, Ord r, Arity d) => r -> Point d r -> m (Point d r)
perturb r p = (p .+^) <$> getPertubation r

-- | Get a vector uniformly at random within the disk of radius r
getPertubation   :: (MonadRandom m, Random r, Ord r, Num r, Arity d) => r -> m (Vector d r)
getPertubation r = go
  where
    go = do v <- getPertubation' r
            if quadrance v <= r*r then pure v
                                  else go -- retry

getPertubation' :: (MonadRandom m, Random r, Num r, Arity d) => r -> m (Vector d r)
getPertubation' = boxPertubation . pure

boxPertubation   :: (MonadRandom m, Random r, Num r, Arity d) => Vector d r -> m (Vector d r)
boxPertubation v = getRandomR ((-1) *^ v,v)

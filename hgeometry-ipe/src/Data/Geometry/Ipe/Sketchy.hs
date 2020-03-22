module Data.Geometry.Ipe.Sketchy where

import Control.Monad.Random.Class
import Data.Ext
import Data.Geometry.CatmulRomSpline
import Data.Geometry.LineSegment
import Data.Geometry.Point
import Data.Geometry.Vector
import System.Random (Random(..))

import Control.Monad.Random

--------------------------------------------------------------------------------

sketch                                      :: (MonadRandom m, Fractional r, Random r, Ord r)
                                            => r -> LineSegment 2 p r -> m (Spline 2 r)
sketch r s@(LineSegment' (p :+ _) (q :+ _)) =
    fromPoints <$> perturb r p <*> a <*> b  <*> perturb r q
  where
    v@(Vector2 vx vy) = q .-. p
    u = Vector2 (-vy) vx  -- perpendicular vector to v
    l = quadrance v

    -- offset for the midpoint perpendicular to the segment by (1/200)^th of the length
    da = let d = 1 / 200 in (*^ u) <$> getRandomR (-d,d)
    -- offset for the 3/4th point, in the range r perpendicular to the
    -- segment and in the range (1/10)^th times the length in the
    -- direction along the segment.
    db = let d = 1 / 10
         in (\du dv -> du *^ u ^+^ dv *^ v) <$> getRandomR (-r,r) <*> getRandomR (-d,d)
    -- TODO: Normalize the du w.r. u; i.e. if u is a unit vector then this is fine, but
    -- now the scaling w.r.t u is way too big.

    a = (interpolate (1/2) s .+^) <$> da
    b = (interpolate (3/4) s .+^) <$> db


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

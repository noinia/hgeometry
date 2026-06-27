{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeData #-}
module Main
  (main) where

import Data.List.NonEmpty (NonEmpty(..))
import Control.Monad.IO.Class
import Data.Default
import Control.DeepSeq (NFData)
import Control.Lens
-- import qualified Data.Foldable as F
import Data.Functor.Classes
-- import qualified Data.List.NonEmpty as NonEmpty
import Data.Semigroup.Foldable
import Data.Vector.NonEmpty.Internal (NonEmptyVector(..))
import GHC.Generics (Generic)
import HGeometry.Box
import HGeometry.Point
import HGeometry.Properties
import HGeometry.Transformation
import HGeometry.Vector
import HGeometry.Matrix
import HGeometry.BezierSpline
import HGeometry.Vector.NonEmpty.Util ()
import Hiraffe.Graph
import Data.Kind (Type)
import Data.Coerce
import Data.Distributive
import Ipe
import HGeometry.Number.Real.Rational
import CatmulRomSpline
import System.Random
import System.Random.Stateful
import HGeometry.LineSegment
import Ipe.Draw
import Prelude hiding (sqrt)
import HGeometry.Number.Radical

import Data.Sequence as Seq
import Debug.Trace
--------------------------------------------------------------------------------

type R = Double -- RealNumber 5



--------------------------------------------------------------------------------

spline :: CatmulRomSegment (Point 2 R)
spline = CatmulRomSegment (Point2 (-1) 1) (Point2 0 0) (Point2 10 0) (Point2 11 1)



--------------------------------------------------------------------------------

instance ( IpeWriteText r, Point_ point 2 r, IpeWriteText r
         ) => IpeWrite (CubicBezier point) where
  ipeWrite = ipeWrite . Path . Seq.singleton . CubicBezierSegment . fmap (view asPoint)



data HandyConfig r = HandyConfig { _roughness :: !r
                                    -- ^ Scaling for random
                                    -- perturbations.  Determines the
                                    -- radius (in output points) in
                                    -- which vertices may be
                                    -- perturbed.
                                 , _bowing :: !r
                                 -- ^ Scaling of the 'bowing' of lines at their midpoint.

                                 -- , _hachureAngle :: {-#UNPACK #-}!Float
                                 --   -- ^ Angle of diagonal hachuring
                                 --   --
                                 --   -- (CCW with respect to the
                                 --   -- positive x-axis)

                                 --   -- TODO: Figure out in what parameter to specify this
                                 -- , _anglePerturbation :: {-#UNPACK #-}!Float
                                 --  -- ^ Random perturbation in hachure angle per object drawn.
                                 -- , _fillWeight
                                 --   -- ^ Hachure filling characteristics.
                                 -- , _fillGap :: !r
                                 --   -- ^ gap between hachures
                                 }
                   deriving (Show,Read,Eq,Ord)

makeLenses ''HandyConfig

instance Fractional r => Default (HandyConfig r) where
  def = HandyConfig { _roughness = 5
                    , _bowing    = (1/200)
                    }

type data Handy (backend :: Type) (r :: Type) (gen :: Type) (m :: Type -> Type)

type instance Rendered (Handy backend r gen m) =
  HandyConfig r -> gen -> m (Rendered backend)

  -- Handy backend r

-- -- | Runs a handy render using the default config in the IO monad.
-- runHandy           :: (MonadIO m, Num r

--                       ) => Handy backend r -> m (Rendered backend)
-- runHandy (Handy h) = fst . h def <$> getStdGen


-- instance Semigroup (Rendered backend) => Semigroup (Handy backend r) where
--   (Handy f) <> (Handy g) = Handy $ \config gen -> let (out1, gen')  = f config gen
--                                                       (out2, gen'') = g config gen'
--                                                   in (out1 <> out2, gen'')
-- instance Monoid (Rendered backend) => Monoid (Handy backend r) where
--   mempty = Handy $ \_config gen -> (mempty, gen)



-- | Given points a, b, c, d, produce three catmul rom spline segments that together
-- draw a spline from a to d.
catmulRom         :: point -> point -> point -> point -> Vector 3 (CatmulRomSegment point)
catmulRom a b c d = Vector3 (CatmulRomSegment a a b c)
                            (CatmulRomSegment a b c d)
                            (CatmulRomSegment b c d d)

-- THis should produce a CatmulRomSpline; so that we produce one Path rather than 3

instance ( Point_ point 2 r, Fractional r, Radical r
         , Monoid (m (Rendered backend))
         , Monoid (Rendered backend)
         , StatefulGen gen m
         , Ord r, UniformRange r

         , IsDrawable backend (Point 2 r), Show r

         , IsDrawable backend (CatmulRomSegment (Point 2 r))
           -- we are leaking a bit of info this way; not sure what to do about that though.
         ) => IsDrawable (Handy backend r gen m) (ClosedLineSegment point) where
  type AttrOf (Handy backend r gen m) (ClosedLineSegment point) =
    AttrOf backend (CatmulRomSegment (Point 2 r))

  draw                    :: [ Attr backend (CatmulRomSegment (Point 2 r)) ]
                          -> ClosedLineSegment point
                          -> HandyConfig r -> gen -> m (Rendered backend)
  draw ats seg config gen = drawSingle <> drawSingle
    where
      -- | We draw a CatmulRom spline with four actual vertices:
      --
      -- the start and endpoint are slightly perturbed enpoints of the segment
      -- we add a midpoint that has been slihgly vertically offset w.r.t the segment
      -- and a third vertex at roughly (3/4)th of the segment that also has been offset.
      drawSingle :: m (Rendered backend)
      drawSingle = do p      <- perturb $ seg^.start.asPoint
                      q      <- perturb $ seg^.end.asPoint
                      m      <- (\b' -> pt (1/2) .+^ (b' *^ w))
                                <$> uniformRM (negate b, b) gen
                      o      <- (\offset -> pt (3/4) .+^ toVec offset)
                                <$> uniformRM (negated dims, dims) gen

                      pure $ foldMap (draw @backend ats) $ catmulRom p m o q

      pt t = Point $ lerp t (seg^.end.vector) (seg^.start.vector)

      toVec (Vector2 dx dy) = (dx *^ v) ^+^ (dy *^ signorm w)

      -- max amount by which we may offset the midpoint
      b = config^.bowing

      v@(Vector2 x y) = (seg^.end) .-. (seg^.start)
      w               = Vector2 (-y) x -- vector perpendicular to the segment

      -- in handy; they pick the o point in a box of width (length
      -- seg)/10 and height r.  we the offset in a box of half that
      -- size.
      dims   = Vector2 (1/20) r
      r = config^.roughness

      -- function to perturb one of the endpoints
      perturb p = (p .+^) <$> uniformIn gen r



-- | Given a positive radius r, generates a vector uniformly at random
-- in the ball of radius r
uniformIn        :: ( Ord r, Num r, UniformRange r
                    , Ord (Vector d r)
                    , StatefulGen gen m, Applicative (Vector d), Has_ Metric_ d r
                    )
                 => gen -> r -> m (Vector d r)
uniformIn gen r = go
  where
    ub = pure r
    lb = negated ub
    go = do v <- uniformRM (lb, ub) gen
            if quadrance v <= r*r then pure v else go



      -- v <- generateA $ const (uniformRM (0, r))


--------------------------------------------------------------------------------0

main :: IO ()
main = do -- print $ coordinateWise (prefix :: Vector 4 R -> Vector 2 R)
          --                        (Vector4 (Point3 1 2 3 :: Point 3 R)
          --                                 (Point3 4 5 6 :: Point 3 R)
          --                                 (Point3 7 8 9 :: Point 3 R)
          --                                 (Point3 1 2 3 :: Point 3 R)
          --                        )
          -- printAsIpeSelection [toCubicBezier spline]
          -- (v :: Vector 2 Int) <- uniformIn globalStdGen 10

          -- printAsIpeSelection $ foldMap (draw @(Ipe R) [])
          --                     $ catmulRom origin (Point2 50 5) (Point2 75 5) (Point2 100 0)

          let handyCfg = def
              seg :: ClosedLineSegment (Point 2 R)
              seg = ClosedLineSegment (Point2 0 0) (Point2 100 10)
          res <- draw @(Handy (Ipe R) R (AtomicGenM StdGen) IO) [] seg handyCfg globalStdGen
          printAsIpeSelection (res :: [IpeObject R])

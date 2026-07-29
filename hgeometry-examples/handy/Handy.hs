{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeData #-}
{-# LANGUAGE UndecidableInstances #-}
module Handy
  ( Handy
  , HandyConfig(HandyConfig), roughness, hachureVector, hachurePerturbationLimit, hachureWeight
  ) where

import Data.Default
import Control.Lens
import Ipe
import Ipe.Draw
import HGeometry.Vector
import GHC.Generics (Generic)
import HGeometry.Box
import HGeometry.Point
import HGeometry.Properties
import HGeometry.Transformation
import HGeometry.Polygon
import HGeometry.Polygon.WithHoles
import HGeometry.Vector
import HGeometry.Matrix
import HGeometry.BezierSpline
import HGeometry.LineSegment
import HGeometry.Number.Radical
import HGeometry.Foldable.Util
import Ipe
import Ipe.Draw
import Hachuring
import System.Random
import System.Random.Stateful
import CatmulRomSpline
import Data.Kind (Type)

--------------------------------------------------------------------------------

-- | Settings/parameters to configure the Handy Sketchy renderer with.
data HandyConfig r = HandyConfig { _roughness :: !r
                                    -- ^ Scaling for random
                                    -- perturbations.  Determines the
                                    -- radius (in output points) in
                                    -- which vertices may be
                                    -- perturbed.
                                 , _bowing :: !r
                                 -- ^ Scaling of the 'bowing' of lines at their midpoint.

                                 , _hachureVector :: {-#UNPACK#-}!(Vector 2 r)
                                 -- ^ Vector describing the
                                 -- orientation in which we compute
                                 -- hachures. The hachures itself are
                                 -- perpendicular to this vector, and
                                 -- are separated by this vector

                                 , _hachurePerturbationLimit :: {-#UNPACK#-}!(Maybe r)
                                  -- ^ Upperbound on the size of the perturbation on
                                   -- the hachureVector. If Nothing no perturbation will be used
                                   -- (and thus the hachures of all objects are perpendicular)

                                 , _hachureWeight :: !(IpePen r)
                                 -- ^ pen width to use for the hachures

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
                   deriving (Show,Eq,Ord)

makeLenses ''HandyConfig

instance Fractional r => Default (HandyConfig r) where
  def = HandyConfig { _roughness                = 5
                    , _bowing                   = (1/200)
                    , _hachureVector            = Vector2 5 (-5)
                    , _hachurePerturbationLimit = Just 1
                    , _hachureWeight            = IpePen (Valued 2)
                    }
  -- TODO: maybe extract the hachureStuff into a hachureConfig that is per-object based?


type data Handy (backend :: Type) (r :: Type) (gen :: Type) (m :: Type -> Type)

type instance Rendered (Handy backend r gen m) =
  HandyConfig r -> gen -> m (Rendered backend)

  -- Handy backend r


--------------------------------------------------------------------------------

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


instance ( Point_ point 2 r, Fractional r, Radical r
         , Monoid (m (Rendered backend))
         , Monoid (Rendered backend)
         , StatefulGen gen m
         , Ord r, UniformRange r
         , VertexContainer f point
         , IsDrawable backend (CatmulRomSegment (Point 2 r))
         , Default (AttrOf backend (CatmulRomSegment (Point 2 r)))
         , HasFill   (AttrOf backend (CatmulRomSegment (Point 2 r))) (Maybe color)
         , HasStroke (AttrOf backend (CatmulRomSegment (Point 2 r))) (Maybe color)
         , HasPen    (AttrOf backend (CatmulRomSegment (Point 2 r))) (Maybe (IpePen r))
         , HasFromFoldable1 f
           -- we are leaking a bit of info this way; not sure what to do about that though.
         ) => IsDrawable (Handy backend r gen m) (SimplePolygonF f point) where
  type AttrOf (Handy backend r gen m) (SimplePolygonF f point) =
    AttrOf backend (CatmulRomSegment (Point 2 r))

  draw ats poly config gen = fill' <> stroke'
    where
      stroke' = foldMapOf outerBoundaryEdgeSegments
                          (\s -> draw @(Handy backend r gen m) (ats <> [fill .~ Nothing])
                                      s config gen
                          ) poly
      -- reset the fill attribute and draw
      fill' = do offset <- case config^.hachurePerturbationLimit of
                             Nothing  -> pure zero
                             Just lim -> uniformRM (pure (negate lim), pure lim) gen
                 computeFill (config^.hachureVector ^+^ offset)

      computeFill v = case (applyAttrs ats def)^.fill of
        Nothing -> mempty
        Just fc -> let fillAts  = ats <> [ stroke ?~ fc
                                         , fill   .~ Nothing
                                         , pen     ?~ config^.hachureWeight
                                         -- , lineCap ?~ 1
                                         -- TODO: set linecap
                                         ]
                       hachures = hachuring v poly
                   in foldMap (\h -> draw @(Handy backend r gen m) fillAts h config gen) hachures

instance ( Point_ point 2 r, Fractional r, Radical r
         , Monoid (m (Rendered backend))
         , Monoid (Rendered backend)
         , StatefulGen gen m
         , Ord r, UniformRange r
         , VertexContainer f point
         , HoleContainer h f point
         , IsDrawable backend (CatmulRomSegment (Point 2 r))
           -- we are leaking a bit of info this way; not sure what to do about that though.
         ) => IsDrawable (Handy backend r gen m) (PolygonalDomainF h f point) where
  type AttrOf (Handy backend r gen m) (PolygonalDomainF h f point) =
    AttrOf backend (CatmulRomSegment (Point 2 r))

  draw ats poly = foldMapOf outerBoundaryEdgeSegments draw' poly
               <> foldMapOf (theHoles.folded.outerBoundaryEdgeSegments) draw' poly
    where
      draw' = draw @(Handy backend r gen m) ats


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

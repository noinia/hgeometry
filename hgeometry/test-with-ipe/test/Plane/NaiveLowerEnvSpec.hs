module Plane.NaiveLowerEnvSpec
  ( spec
  ) where


import           Control.Lens hiding (IsEmpty, IsNonEmpty)
import           Data.Foldable
import           Data.Foldable1 as F1
import           Data.List.NonEmpty (NonEmpty(..))
import qualified Data.List.NonEmpty as NonEmpty
import           Data.Map.NonEmpty (NEMap, pattern IsEmpty, pattern IsNonEmpty)
import qualified Data.Map.NonEmpty as NEMap
import           Data.Maybe (fromMaybe)
import           Data.Ord (comparing)
import qualified Data.Set as Set
import qualified Data.Text as Text
import           Golden
import           HGeometry.Box
import           HGeometry.Ext
import           HGeometry.HyperPlane
import           HGeometry.Intersection
import           HGeometry.Number.Real.Rational
import           HGeometry.Plane.LowerEnvelope
import qualified HGeometry.Plane.LowerEnvelope.Connected.Randomized as Randomized
import           HGeometry.Point
import           HGeometry.Point.Either
import           HGeometry.Polygon
import           HGeometry.Polygon.Convex
import           HGeometry.Polygon.Simple
import           HGeometry.Properties
import           HGeometry.Sequence.Alternating (separators)
import           HGeometry.Triangle
import           HGeometry.Vector
-- import           HGeometry.VoronoiDiagram
import qualified HGeometry.VoronoiDiagram as VD
import           Hiraffe.Graph.Class
import           Ipe
import           Ipe.Color
import           System.OsPath
import           System.Random
import           Test.Hspec
import           Test.Hspec.WithTempFile
import           Test.QuickCheck.Instances ()

--------------------------------------------------------------------------------

type R = RealNumber 5


type instance Intersection (Triangle corner) (Region r vertex) =
  Maybe (ClippedBoundedRegion r vertex corner)

-- instance Triangle corner `HasIntersectionWith` (Region r vertex)
-- instance Triangle corner `IsIntersectableWith` (Region r vertex) where
--   tri `intersect` reg = case reg of
--     Bounded   convex -> tri `intersect` convex
--     Unbounded convex -> tri `intersect` convex

{-
-- |
bruteForceLowerEnvelopeIn     :: ( Plane_ plane r, Ord plane, Ord r, Fractional r
                                 -- , HasDefiners vertexData plane
                                 , Point_ corner 2 r
                                 -- , Ord vertexData -- figure out why we need this?
                                 , Foldable1 set

                                 , Show r, Show corner, Show plane -- TODO: remove these
                                 )
                              => Triangle corner
                              -> set plane
                              -> NEMap plane (BoundedRegion r (MDVertex r plane) (Point 2 r))
bruteForceLowerEnvelopeIn tri planes = case bruteForceLowerEnvelope planes of
    Nothing      -> lowestPlane
    Just diagram -> case NEMap.mapMaybe (tri `intersect`) (asMap diagram) of
                      IsEmpty                   -> lowestPlane
                      IsNonEmpty clippedDiagram -> clippedDiagram
  where
    lowestPlane = let h = F1.minimumBy (comparing $ evalAt (tri'^.head1)) planes
                  in NEMap.singleton h polyTri

    tri'    = (^.asPoint) <$> tri
    polyTri = fromMaybe (error "absurd: bruteForceLowerEnvelopeIn illegal triangle")
            $ fromPoints (Extra <$> tri')


-}


spec :: Spec
spec = describe "lower envelope in bounded region" $ do
         pure ()

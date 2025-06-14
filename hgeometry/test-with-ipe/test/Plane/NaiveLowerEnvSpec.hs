{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE UndecidableInstances  #-}
module Plane.NaiveLowerEnvSpec
  ( spec
  ) where


import           Control.Lens hiding (IsEmpty, IsNonEmpty)
import           Control.Subcategory.Functor
import           Data.Bifunctor
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
import           HGeometry.Polygon.Simple.PossiblyDegenerate
import           HGeometry.Properties
import           HGeometry.Sequence.Alternating (separators)
import           HGeometry.Triangle
import           HGeometry.Vector
-- import           HGeometry.VoronoiDiagram
import qualified HGeometry.VoronoiDiagram as VD
import           HGeometry.VoronoiDiagram.ViaLowerEnvelope (pointToPlane)
import           Hiraffe.Graph.Class
import           Ipe
import           Ipe.Color
import           System.OsPath
import           System.Random
import           Test.Hspec
import           Test.Hspec.WithTempFile
import           Test.QuickCheck.Instances ()
import qualified HGeometry.VoronoiDiagram as VD

--------------------------------------------------------------------------------

type R = RealNumber 5

type instance Intersection (Triangle corner) (Region r vertex) = Maybe (ClippedMDCell' r vertex)



-- | Cells in the Minimization diagram (i.e. the projected lower envelope of planes)
type ClippedMDCell r plane = ClippedMDCell' r (MDVertex r plane)

-- | Helper type for representing cells in a minimzation diagram. These cells are possibly
-- degenerate convex polygons, whose vertices are either of type 'vertex' or of type
-- 'Point 2 r'.
newtype ClippedMDCell' r vertex = ClippedMDCell
  (PossiblyDegenerateSimplePolygon (OriginalOrExtra vertex (Point 2 r))
                                   (ClippedBoundedRegion r vertex (Point 2 r)))

type instance NumType   (ClippedMDCell' r vertex) = r
type instance Dimension (ClippedMDCell' r vertex) = 2

deriving instance (Show vertex, Point_ vertex 2 r, Show r) => Show (ClippedMDCell' r vertex)
deriving instance (Eq vertex, Eq r)     => Eq (ClippedMDCell' r vertex)

instance Functor (ClippedMDCell' r) where
  fmap f (ClippedMDCell poly) = ClippedMDCell $ bimap (first f) (fmap (first f)) poly



instance ( Point_ vertex 2 r, Fractional r, Ord r
         , Show r
         )
         => HasDefaultIpeOut (ClippedMDCell' r vertex) where
  type DefaultIpeOut (ClippedMDCell' r vertex) = Group
  defIO (ClippedMDCell cell) = case cell of
    ActualPolygon cell' -> ipeGroup [iO $ defIO $ (cell'&vertices %~ (^.asPoint)
                                                  :: ConvexPolygonF NonEmpty (Point 2 r)
                                                  )
                                    ]
    _                   -> ipeGroup []


instance ( Fractional r, Ord r
         , Show r
         )
         => HasDefaultIpeOut (ClippedLowerEnvelope' r plane) where
  type DefaultIpeOut (ClippedLowerEnvelope' r plane) = Group
  defIO (ClippedLowerEnvelope env) = ipeGroup $ foldMap ((:[]) . iO . defIO) env


instance (Point_ vertex 2 r, Point_ corner 2 r, Ord r, Fractional r
         ) => Triangle corner `HasIntersectionWith` (Region r vertex)
instance (Point_ vertex 2 r, Point_ corner 2 r, Ord r, Fractional r
         ) => Triangle corner `IsIntersectableWith` (Region r vertex) where
  tri `intersect` reg = ClippedMDCell <$> case reg of
    BoundedRegion   convex -> tri `intersect` convex
    UnboundedRegion convex -> tri `intersect` convex



-- rename to minimization diagram

-- | A clipped lower envelope
type ClippedLowerEnvelope plane  = ClippedLowerEnvelope' (NumType plane) plane

-- | A clipped lower envelope
newtype ClippedLowerEnvelope' r plane =
  ClippedLowerEnvelope (NEMap plane (ClippedMDCell r plane))

type instance NumType   (ClippedLowerEnvelope' r plane) = r
type instance Dimension (ClippedLowerEnvelope' r plane) = 2


deriving instance (Show r, Num r, Show plane) => Show (ClippedLowerEnvelope' r plane)

instance Constrained (ClippedLowerEnvelope' r) where
  type Dom (ClippedLowerEnvelope' r) plane = Ord plane

instance CFunctor (ClippedLowerEnvelope' r) where
  -- cmap   :: Ord plane'
  --        => (plane -> plane') -> ClippedLowerEnvelope' r plane -> ClippedLowerEnvelope' r plane'
  cmap f (ClippedLowerEnvelope m) = ClippedLowerEnvelope $
    NEMap.foldMapWithKey (\plane cell -> NEMap.singleton (f plane) (fmap f <$> cell)) m


-- | A clipped Voronoi diagram
type ClippedVoronoiDiagram point = ClippedLowerEnvelope point
-- which as the same representation as a clipped lower envelope


-- | Use a navive O(n^4) time algorithm to compute the lower envelope inside a given
-- triangle.
bruteForceLowerEnvelopeIn     :: forall plane r corner set.
                                 ( Plane_ plane r, Ord plane, Ord r, Fractional r
                                 , Point_ corner 2 r
                                 , Foldable1 set

                                 , Show r, Show corner, Show plane -- TODO: remove these
                                 )
                              => Triangle corner
                              -> set plane
                              -> ClippedLowerEnvelope plane
bruteForceLowerEnvelopeIn tri planes = ClippedLowerEnvelope
                                     $ case bruteForceLowerEnvelope planes of
    Nothing      -> lowestPlane
    Just diagram -> case NEMap.mapMaybe (tri `intersect`) (asMap diagram) of
                      IsEmpty                   -> lowestPlane
                      IsNonEmpty clippedDiagram -> clippedDiagram
  where
    lowestPlane = let h = F1.minimumBy (comparing $ evalAt (tri'^.head1)) planes
                  in NEMap.singleton h (ClippedMDCell . ActualPolygon $ polyTri)

    tri'    = (^.asPoint) <$> tri


    polyTri = fromMaybe (error "absurd: bruteForceLowerEnvelopeIn illegal triangle")
            $ fromPoints (Extra <$> tri')


-- | Given a function to compute a lower envelope; construct use it to construct the
-- Voronoi diagram inside the given triangle.
voronoiDiagramInWith :: ( Point_ point 2 r, Functor nonEmpty, Ord point
                        , Ord r, Fractional r, Foldable1 nonEmpty
                        )
                     => (Triangle corner -> nonEmpty (Plane r :+ point) -> ClippedLowerEnvelope (Plane r :+ point))
                     -> Triangle corner
                     -> nonEmpty point
                     -> ClippedVoronoiDiagram point
voronoiDiagramInWith lowerEnv tri =
  cmap (^.extra) . lowerEnv tri . fmap (\p -> pointToPlane p :+ p)


-- | Computes the Voronoi inside the given triangle.
voronoiDiagramIn :: ( Point_ point 2 r, Point_ corner 2 r, Functor nonEmpty, Ord point
                    , Ord r, Fractional r, Foldable1 nonEmpty

                    , Show r, Show corner, Show point-- TODO: remove these
                    )
                 => Triangle corner -> nonEmpty point -> ClippedVoronoiDiagram point
voronoiDiagramIn = voronoiDiagramInWith bruteForceLowerEnvelopeIn

-- type ClippedLowerEnvelope' r plane = NEMap plane (ClippedMDCell r (MDVertex r plane))

-- type ClippedMDCell' r vertex =
--   PossiblyDegenerateSimplePolygon (OriginalOrExtra vertex (Point 2 r))
--                                   (ClippedBoundedRegion r vertex (Point 2 r))


--------------------------------------------------------------------------------


spec :: Spec
spec = describe "lower envelope in bounded region" $ do
         testIpe [osp|trivial.ipe|]
                 [osp|trivial_clipped_out|]
         -- testIpe [osp|simplest.ipe|]
         --         [osp|simplest_clipped_out|]
         -- testIpe [osp|simpler.ipe|]
         --         [osp|simpler_clipped_out|]
         -- testIpe [osp|simple.ipe|]
         --         [osp|simple_clipped_out|]
         -- testIpe [osp|simple1.ipe|]
         --         [osp|simple1_clipped_out|]
         -- testIpe [osp|foo.ipe|]
         --         [osp|foo_clipped_out|]
         -- testIpe [osp|degenerate.ipe|]
         --         [osp|degenerate_clipped_out|]
         -- testIpe [osp|degenerate1.ipe|]
         --         [osp|degenerate1_clipped_out|]
         -- testIpe [osp|degenerate2.ipe|]
         --         [osp|degenerate2_clipped_out|]


-- | Build voronoi diagrams on the input points
testIpe            :: OsPath -> OsPath -> Spec
testIpe inFp outFp = do
    (points :: NonEmpty (Point 2 R :+ _)) <- runIO $ do
      inFp' <- getDataFileName ([osp|test-with-ipe/VoronoiDiagram/|] <> inFp)
      NonEmpty.fromList <$> readAllFrom inFp'
    let tri = Triangle origin (Point2 100 0) (Point2 0 100)
        vd = voronoiDiagramIn tri $ view core <$> points
        -- vd = rVoronoiDiagram $ view core <$> points
        vv = VD.voronoiVertices $ view core <$> points
        out = [ iO' points
              , iO' vd
              ] <> [ iO'' v $ attr SStroke red | v <- Set.toAscList vv ]
    goldenWith [osp|data/test-with-ipe/Plane/LowerEnvelope/|]
               (ipeFileGolden { name = outFp })
               (addStyleSheet opacitiesStyle $ singlePageFromContent out)

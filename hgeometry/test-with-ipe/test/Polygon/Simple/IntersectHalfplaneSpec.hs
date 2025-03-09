{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE QuasiQuotes #-}
module Polygon.Simple.IntersectHalfplaneSpec where


import           Control.Lens
import           Data.Bifunctor
import           Data.Foldable (toList, for_)
import           Data.Foldable1
import           Data.List.NonEmpty (NonEmpty(..))
import qualified Data.List.NonEmpty as NonEmpty
import           Data.Maybe (isJust, mapMaybe, maybeToList)
import qualified Data.Text as Text
import           Data.Traversable
import           Data.Vector.NonEmpty (NonEmptyVector)
import           Golden
import           HGeometry.Box
import qualified HGeometry.Box as Box
import           HGeometry.Cyclic
import           HGeometry.Ext
import           HGeometry.Foldable.Util
import           HGeometry.HalfLine
import           HGeometry.HalfSpace
import           HGeometry.HyperPlane.Class
import           HGeometry.Instances ()
import           HGeometry.Intersection
import           HGeometry.Interval.EndPoint
import           HGeometry.Line
import           HGeometry.LineSegment
import           HGeometry.Number.Real.Rational
import           HGeometry.PlaneGraph
import           HGeometry.Point
import           HGeometry.Point.Either
import           HGeometry.Polygon.Convex
import           HGeometry.Polygon.Convex.Internal
import           HGeometry.Polygon.Instances ()
import           HGeometry.Polygon.Simple
import           HGeometry.Properties
import           HGeometry.Triangle
import qualified HGeometry.Triangle as Triangle
import           HGeometry.Vector
import           Ipe
import           Ipe.Color
import           System.OsPath
import           Test.Hspec
import           Test.Hspec.QuickCheck
import           Test.Hspec.WithTempFile
import           Test.QuickCheck

import           Debug.Trace
import           Data.Functor.Classes
--------------------------------------------------------------------------------

type R = RealNumber 5


--------------------------------------------------------------------------------
-- to ipe

--------------------------------------------------------------------------------

-- type HalfPlane line = HalfSpaceF line

--------------------------------------------------------------------------------



--------------------------------------------------------------------------------











-- itest :: NonEmpty ((Int, Vector 2 Int), (Char, Vector 2 Char))
-- itest = runIdentity $ withCyclicNeighbours (Indexed $ \i x -> pure (i,x)) (NonEmpty.fromList "abcde")


-- test4 = runIdentity $ (withCyclicNeighbours.withIndex) (Indexed $ \i x -> pure (i,x)) (NonEmpty.fromList "abcde")

-- itest = toNonEmptyOf (withCyclicSuccessor.withIndex) (NonEmpty.fromList "abcde")


--------------------------------------------------------------------------------

-- outerBoundaryEdges :: IndexedFold1 (VertexIx polygon,VertexIx polygon) polygon
--                                      (Vertex polygon, Vertex polygon)


-- outerBoundaryWithNeighbours :: (HasOuterBoundary polygon, VertexIx polygon ~ Int) => IndexedFold1 (VertexIx polygon, (VertexIx polygon, VertexIx polygon)) polygon (Vertex polygon, (Vertex polygon, Vertex polygon))










--------------------------------------------------------------------------------


--   LinePV u (v .-. u) `intersect` line of


--   let (u,v) = bimap (^.asPoint) (^.asPoint) e
--                            in

-- -- TODO:: Appropriately handle degeneracies


-- flatten :: NonEmpty (NonEmpty (a, b)) -> NonEmpty (b, NonEmpty a)
-- flatten = fmap (\((x,b) :| xs) -> (b, x :| map fst xs))


-- I would have liked to give the following instance for simple polygons,
-- but recombining the chains is more complicated I initially realized;
-- i.e. collectComponents correctly computes the polygonal chains that lie in the halfplane
-- however, we have to stich them together "in some order along" the bounding line.
-- With winding polygons it is nto so clear how to compute which pieces to connect.
{-
instance ( Point_ vertex 2 r, Fractional r, Ord r, VertexContainer f vertex
         , VertexContainer f (OriginalOrExtra vertex (Point 2 r))
         , HasFromFoldable1 f
         , Show r, Show vertex
         ) => IsIntersectableWith (HalfSpaceF (LinePV 2 r)) (SimplePolygonF f vertex) where
  halfPlane `intersect` poly = collectComponents (halfPlane^.boundingHyperPlane)
                             . groupWith (\v -> (v^.asPoint) `intersects` halfPlane) . Cyclic
                             $ toNonEmptyOf vertices poly

instance ( HasIntersectionWith (HalfSpaceF line) (SimplePolygonF f vertex)
         ) => HasIntersectionWith (HalfSpaceF line :+ extra)
                                  (SimplePolygonF f vertex :+ extra') where
  (halfPlane :+ _) `intersects` (poly :+ _) = halfPlane `intersects` poly
-}


--------------------------------------------------------------------------------
-- * Halfspace x Rectangle Intersection

type instance Intersection (HalfSpaceF line) (Rectangle corner) =
  Maybe (PossiblyDegenerateSimplePolygon (CanonicalPoint corner)
                                         (ConvexPolygon (CanonicalPoint corner))
        )

-- this type is not entirely right; as we need to constrain the dimension to 2

instance ( Point_ corner 2 r, Num r, Ord r
         ) => HasIntersectionWith (HalfSpaceF (LinePV 2 r)) (Rectangle corner) where
  halfPlane `intersects` rect' = any (`intersects` halfPlane) ((^.asPoint) <$> Box.corners rect')

instance ( Point_ corner 2 r, Fractional r, Ord r
         , Show r
         ) => IsIntersectableWith (HalfSpaceF (LinePV 2 r)) (Rectangle corner) where
  halfPlane `intersect` rect' = fmap (fmap flatten')
                             <$> halfPlane `intersect` (toConvexPolygon rect')
    where
      flatten' = \case
        Original p -> p
        Extra p    -> p

      toConvexPolygon :: Rectangle corner -> ConvexPolygon (Point 2 r)
      toConvexPolygon = uncheckedFromCCWPoints . fmap (^.asPoint) . Box.corners

--------------------------------------------------------------------------------
-- * Halfspace x Triangle Intersection

type instance Intersection (HalfSpaceF line) (Triangle corner) =
  Maybe (PossiblyDegenerateSimplePolygon (CanonicalPoint corner)
                                         (ConvexPolygon (CanonicalPoint corner))
        )
-- this type is not entirely right; as we need to constrain the dimension to 2

instance ( Point_ corner 2 r, Num r, Ord r
         ) => HasIntersectionWith (HalfSpaceF (LinePV 2 r)) (Triangle corner) where
  halfPlane `intersects` tri = anyOf (Triangle.corners.traverse1.asPoint)
                                     (`intersects` halfPlane) tri

instance ( Point_ corner 2 r, Fractional r, Ord r
         , Show r
         ) => IsIntersectableWith (HalfSpaceF (LinePV 2 r)) (Triangle corner) where
  halfPlane `intersect` tri = fmap (fmap flatten')
                           <$> halfPlane `intersect` (toConvexPolygon tri)
    where
      flatten' = \case
        Original p -> p
        Extra p    -> p

      toConvexPolygon :: Triangle corner -> ConvexPolygon (Point 2 r)
      toConvexPolygon = uncheckedFromCCWPoints . fmap (^.asPoint) . view (Triangle.corners)
                      . toCounterClockwiseTriangle

--------------------------------------------------------------------------------


spec :: Spec
spec = describe "simple polygon x halfspace intersection" $ do
         testIpe [osp|polygonHalfspaceIntersection.ipe|]
                 [osp|polygonHalfspaceIntersection.out|]
         testIpe [osp|convexHalfspaceIntersection.ipe|]
                 [osp|convexHalfspaceIntersection.out|]
         testIpe [osp|halfSpaceIntersectionDegenerate.ipe|]
                 [osp|halfSpaceIntersectionDegenerate.out|]


loadInputs      :: OsPath -> IO ( NonEmpty (HalfSpaceF (LinePV 2 R) :+ _)
                                , NonEmpty (ConvexPolygon (Point 2 R) :+ _)
                                )
loadInputs inFp = do
        inFp'      <- getDataFileName ([osp|test-with-ipe/Polygon/Convex/|] <> inFp)
        Right page <- readSinglePageFile inFp'
        let (rays :: NonEmpty (HalfLine (Point 2 R) :+ _))     = NonEmpty.fromList $ readAll page
            (pgs  :: NonEmpty (ConvexPolygon (Point 2 R) :+ _)) = NonEmpty.fromList $ readAll page
        -- take the left halfplane of every halfline
        pure (over core (leftHalfPlane . asOrientedLine) <$> rays, pgs)

testIpe            :: OsPath -> OsPath -> Spec
testIpe inFp outFp = describe (show inFp) $ do
    (halfPlanes, polygons) <-  runIO $ loadInputs inFp

    for_ polygons $ \polygon ->
      for_ halfPlanes $ \halfPlane -> do
        it ("intersects halfplane and polygon") $
          (halfPlane `intersects` polygon) `shouldBe` True -- TODO; fix
        it ("intersect halfplane and polygon propper polygons") $ do
           for_ (halfPlane `intersect` polygon) $ \case
             DegenerateVertex _     -> pure ()
             DegenerateEdge _       -> pure ()
             ActualPolygon poly ->
               fromPoints @(SimplePolygon _) (toNonEmptyOf vertices poly)
               `shouldSatisfy` isJust

    let forMap'  = flip foldMap
        content' = forMap' polygons $ \polygon ->
                      forMap' halfPlanes $ \halfPlane ->
                         [ iO' polygon
                         , iO $ defIO (halfPlane^.core)
                         , iO $ ipeGroup [ renderComponent comp
                                         | comp <- maybeToList $ halfPlane `intersect` polygon
                                         ]
                         ]
    describe "compute intersection" $
      goldenWith [osp|data/test-with-ipe/Polygon/Convex/|]
                 (ipeFileGolden { name = outFp })
                   (addStyleSheet opacitiesStyle $ singlePageFromContent content')


renderComponent :: forall vertex f r.
                   ( Point_ vertex 2 r
                   , VertexContainer f (OriginalOrExtra vertex (Point 2 r))
                   , HasFromFoldable1 f
                   -- , HasVertices (ConvexPolygonF f (OriginalOrExtra vertex (Point 2 r)))
                   --               (ConvexPolygonF f (Point 2 r))
                   --  VertexContainer f (OriginalOrExtra vertex (Point 2 r))
                   -- , VertexContainer f (Point 2 r)
                   )
                => HalfPlaneConvexPolygonIntersection f r vertex -> IpeObject r
renderComponent = \case
    DegenerateVertex v -> iO $ defIO (v^.asPoint)
                             ! attr SStroke red
    DegenerateEdge e   -> iO $ defIO (view asPoint <$> e)
                             ! attr SStroke red
    ActualPolygon poly -> iO $ ipeSimplePolygon poly
                             ! attr SFill red

--------------------------------------------------------------------------------


instance (Fractional r, Ord r, Show r) => HasDefaultIpeOut (HalfSpaceF (LinePV 2 r)) where
  type DefaultIpeOut (HalfSpaceF (LinePV 2 r)) = Group
  defIO = ipeHalfPlane

-- | Default rendering of halfplanes
ipeHalfPlane :: (Show r, Fractional r, Ord r) => IpeOut (HalfSpaceF (LinePV 2 r)) Group r
ipeHalfPlane = ipeHalfPlaneIn defaultBox

-- | Draw a halfplane in the given rectangle.
--
-- We draw both the border (in black) and the interior (20% transparant gray) of the halfpace
ipeHalfPlaneIn          :: (Ord r, Fractional r, Show r)
                        => Rectangle (Point 2 r) -> IpeOut (HalfSpaceF (LinePV 2 r)) Group r
ipeHalfPlaneIn rect' hl = case hl `intersect` rect' of
    Nothing -> ipeGroup [] -- this should not really happen I guess?
    Just is -> case is of
      ActualPolygon interior -> ipeGroup [ iO $ ipeSimplePolygon interior
                                              ! attr SFill gray
                                              ! attr SOpacity (Text.pack "20%")
                                         , boundary
                                         ]
      _                      -> ipeGroup [ boundary ]
  where
    boundary = iO $ ipeLineIn rect' (hl^.boundingHyperPlane)

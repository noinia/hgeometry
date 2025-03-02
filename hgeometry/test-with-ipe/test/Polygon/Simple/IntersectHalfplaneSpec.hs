{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE QuasiQuotes #-}
module Polygon.Simple.IntersectHalfplaneSpec where


import           Control.Lens
import           Data.Bifunctor
import           Data.Foldable (toList, for_)
import           Data.Foldable1
import           Data.Functor.Contravariant (phantom)
import           Data.List.NonEmpty (NonEmpty(..))
import qualified Data.List.NonEmpty as NonEmpty
import           Data.Maybe (isJust, mapMaybe, maybeToList)
import           Data.Traversable
import           Golden
import           HGeometry.Box
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

type HalfPlane line = HalfSpaceF line

--------------------------------------------------------------------------------

-- | A simple polygon (or a "subtype" thereof) that may be degenerate; i.e. may be a single
-- vertex, or a point.
data PossiblyDegenerateSimplePolygon vertex polygon =
    DegenerateVertex vertex
  | DegenerateEdge (ClosedLineSegment vertex)
  | ActualPolygon polygon
  deriving (Show,Eq,Functor)

instance Bifunctor PossiblyDegenerateSimplePolygon where
  bimap f g = \case
    DegenerateVertex v -> DegenerateVertex (f v)
    DegenerateEdge e   -> DegenerateEdge (fmap f e)
    ActualPolygon poly -> ActualPolygon (g poly)

--------------------------------------------------------------------------------

-- | A HalfPlane and a simple polygon intersect in a bunch of components, each of
-- which is a possiblyDegenerate simple polygon.
type instance Intersection (HalfPlane line) (SimplePolygonF f point) =
  [HalfPlane_x_SimplePolygon_Component f (NumType point) point]
-- | A single Component of a HalfPlane x SimplePolygon intersection.
type HalfPlane_x_SimplePolygon_Component f r vertex =
  PossiblyDegenerateSimplePolygon vertex (SimplePolygonF f (OriginalOrExtra vertex (Point 2 r)))


-- | If we drag along extra information in the halfplane polygon intersection we lose it
type instance Intersection (HalfPlane line :+ extra) (SimplePolygonF f point :+ extra') =
  Intersection (HalfPlane line) (SimplePolygonF f point)

----------------------------------------

-- | A HalfPlane and a Convex polygon intersect in a single component, which is a
-- possiblyDegenerate convex polygon.
type instance Intersection (HalfPlane line) (ConvexPolygonF f point) =
  Maybe (HalfPlaneConvexPolygonIntersection f (NumType point) point)

-- | A single Component of a HalfPlane x ConvexPolygon intersection.
type HalfPlaneConvexPolygonIntersection f r vertex =
  PossiblyDegenerateSimplePolygon vertex (ConvexPolygonF f (OriginalOrExtra vertex (Point 2 r)))

-- | If we drag along extra information in the halfplane polygon intersection we lose it
type instance Intersection (HalfPlane line :+ extra) (ConvexPolygonF f point :+ extra') =
  Intersection (HalfPlane line) (ConvexPolygonF f point)

--------------------------------------------------------------------------------




--------------------------------------------------------------------------------
-- * Intersection between Halfspaces and possibly degenerate convex polygons

instance ( Point_ vertex 2 r, Num r, Ord r, VertexContainer f vertex
         , HyperPlane_ line 2 r
         ) => HalfSpaceF line `HasIntersectionWith`
              PossiblyDegenerateSimplePolygon vertex (ConvexPolygonF f vertex) where
  halfSpace `intersects` degenPoly = case degenPoly of
    DegenerateVertex v -> (v^.asPoint) `intersects` halfSpace
    DegenerateEdge e   -> e `intersects` halfSpace
    ActualPolygon poly -> halfSpace `intersects` poly



-- | Intersecting a halfplane witha possibly degenerate convex polygon
-- gives us a possibly degenerate polygon again.
--
type instance Intersection (HalfPlane line)
                           (PossiblyDegenerateSimplePolygon vertex (ConvexPolygonF f vertex))
  = Maybe (PossiblyDegenerateSimplePolygon
              (OriginalOrExtra vertex (CanonicalPoint vertex))
              (ConvexPolygonF f (OriginalOrExtra vertex (CanonicalPoint vertex)))
          )
   -- we lose some information here; if we are a degenreate point we are guaranteed
   -- to be an original; the type also allows it to be an Extra.

instance ( Point_ vertex 2 r, Fractional r, Ord r, VertexContainer f vertex
         , VertexContainer f (OriginalOrExtra vertex (Point 2 r))
         , HyperPlane_ line 2 r
         , IsIntersectableWith (HalfSpaceF line) (ConvexPolygonF f vertex)
         -- this one is satisfied for e.g. line ~ LinePV

         , IsIntersectableWith (LinePV 2 r) line
         , Intersection (LinePV 2 r) line ~ Maybe (LineLineIntersectionG r line')
         ) => HalfSpaceF line `IsIntersectableWith`
              PossiblyDegenerateSimplePolygon vertex (ConvexPolygonF f vertex) where
  halfSpace `intersect` degenPoly = case degenPoly of
    DegenerateVertex v -> DegenerateVertex (Original v) <$ ((v^.asPoint) `intersect` halfSpace)
    DegenerateEdge e   -> e `intersect` halfSpace <&> \case
      ClosedLineSegment_x_HalfSpace_Point v           -> DegenerateVertex (Original v)
      ClosedLineSegment_x_HalfSpace_SubSegment e      -> DegenerateEdge e
      ClosedLineSegment_x_HalfSpace_CompleteSegment e -> DegenerateEdge (Original <$> e)

    ActualPolygon poly -> first Original <$> halfSpace `intersect` poly

--------------------------------------------------------------------------------
-- * Intersection of HalfSpaces and Line segments

instance ( Point_ point d r, Ord r, Num r
         , HyperPlane_ plane d r
         ) => HasIntersectionWith (ClosedLineSegment point) (HalfSpaceF plane) where
  seg `intersects` halfSpace = (seg^.start.asPoint) `intersects` halfSpace
                            || (seg^.end.asPoint)   `intersects` halfSpace


type instance Intersection (ClosedLineSegment point) (HalfSpaceF plane) =
  Maybe (ClosedSegmentHalfSpaceIntersection (CanonicalPoint point)
                                            point
        )


-- | Models the intersection of a closed linesegment and halfspace
-- if the segment intersects the bounding hyperplane, the intersection point
-- is of type extra.
data ClosedSegmentHalfSpaceIntersection extra point =
    ClosedLineSegment_x_HalfSpace_Point           point
  | ClosedLineSegment_x_HalfSpace_SubSegment      (ClosedLineSegment (OriginalOrExtra point extra))
  -- ^ the subsegment is always oriented from the intersection point towards the original point
  -- note that this may reverse the original input segment.
  | ClosedLineSegment_x_HalfSpace_CompleteSegment (ClosedLineSegment point)
  deriving (Show,Eq)


instance ( Point_ point 2 r, Ord r, Fractional r
         , HyperPlane_ plane 2 r
         , IsIntersectableWith (LinePV 2 r) plane
         , Intersection (LinePV 2 r) plane ~ Maybe (LineLineIntersectionG r line')
         ) => IsIntersectableWith (ClosedLineSegment point) (HalfSpaceF plane) where
  seg `intersect` halfSpace = case (seg^.start.asPoint) `intersect` halfSpace of
      Nothing -> case mxEnd of
        Nothing   -> Nothing
        Just xEnd -> Just $ case xEnd of
          Point_x_HalfSpace_OnBoundary _ -> ClosedLineSegment_x_HalfSpace_Point (seg^.end)
          Point_x_HalfSpace_Interior   t -> subSegment (seg^.end) t (seg^.start.asPoint)

      Just xStart -> Just $ case xStart of
        Point_x_HalfSpace_OnBoundary _ -> case mxEnd of
          Nothing  -> ClosedLineSegment_x_HalfSpace_Point (seg^.start)
          Just _   -> completeSeg
        Point_x_HalfSpace_Interior s   -> case mxEnd of
          Nothing  -> subSegment (seg^.start) s (seg^.end.asPoint)
          Just _   -> completeSeg
    where
      completeSeg = ClosedLineSegment_x_HalfSpace_CompleteSegment seg
      mxEnd = (seg^.end.asPoint)   `intersect` halfSpace

      -- Compute the subsegment ; we are guarnteed that inP is inside and outP is outside
      -- the haflspace
      subSegment inP' inP outP = ClosedLineSegment_x_HalfSpace_SubSegment
                               . ClosedLineSegment (Original inP')
                               $ case LinePV inP (outP .-. inP)
                                       `intersect` (halfSpace^.boundingHyperPlane) of
        Just (Line_x_Line_Point p) -> Extra p
        _                          -> error "line segment x halfspace intersection: absurd"



--------------------------------------------------------------------------------

loadInputs' inFp = do
        inFp'      <- getDataFileName ([osp|test-with-ipe/Polygon/Simple/|] <> inFp)
        Right page <- readSinglePageFile inFp'
        let (rays :: NonEmpty (HalfLine (Point 2 R) :+ _))     = NonEmpty.fromList $ readAll page
            (pgs  :: NonEmpty (ConvexPolygon (Point 2 R) :+ _)) = NonEmpty.fromList $ readAll page
        -- take the left halfpalne of every halfline
        pure (over core (leftHalfPlane . asOrientedLine) <$> rays, pgs)

--------------------------------------------------------------------------------
-- * Intersection of Triangle and ConvexPolygon

type instance Intersection (Triangle corner) (ConvexPolygonF f vertex) =
  Maybe (PossiblyDegenerateSimplePolygon (OriginalOrCanonical vertex)
                                         (ConvexPolygonF f (OriginalOrCanonical vertex))
        )

instance ( Point_ point 2 r, Point_ point' 2 r, Num r, Ord r, VertexContainer f point
         ) => HasIntersectionWith (Triangle point') (ConvexPolygonF f point) where
  triangle `intersects` poly = allOf (vertices.asPoint) (`intersects` triangle) poly


type V vertex r = OriginalOrExtra vertex (Point 2 r)

instance ( Point_ vertex 2 r, Point_ corner 2 r, Fractional r, Ord r, VertexContainer f vertex
         , VertexContainer f (OriginalOrCanonical vertex)
         , VertexContainer f (OriginalOrExtra (OriginalOrCanonical vertex) (Point 2 r))
         , HasFromFoldable1 f
         ) => IsIntersectableWith (Triangle corner) (ConvexPolygonF f vertex) where
  triangle `intersect` poly = foldr intersect' (Just $ ActualPolygon $ poly&vertices %~ Original)
                                               (intersectingHalfPlanes triangle)
    where
      intersect'      :: HalfSpaceF (LinePV 2 r)
                      -> Maybe (PossiblyDegenerateSimplePolygon (V vertex r)
                                                                (ConvexPolygonF f (V vertex r)))
                      -> Maybe (PossiblyDegenerateSimplePolygon (V vertex r)
                                                                (ConvexPolygonF f (V vertex r)))
      intersect' h mp = do p <- mp
                           bimap flatten (fmap flatten) <$> h `intersect` p

-- | Flatten two nested originals
flatten :: OriginalOrExtra (OriginalOrExtra vertex extra) extra -> OriginalOrExtra vertex extra
flatten = \case
  Extra e    -> Extra e
  Original o -> o








-- itest :: NonEmpty ((Int, Vector 2 Int), (Char, Vector 2 Char))
-- itest = runIdentity $ withCyclicNeighbours (Indexed $ \i x -> pure (i,x)) (NonEmpty.fromList "abcde")


-- test4 = runIdentity $ (withCyclicNeighbours.withIndex) (Indexed $ \i x -> pure (i,x)) (NonEmpty.fromList "abcde")

-- itest = toNonEmptyOf (withCyclicSuccessor.withIndex) (NonEmpty.fromList "abcde")


--------------------------------------------------------------------------------

-- outerBoundaryEdges :: IndexedFold1 (VertexIx polygon,VertexIx polygon) polygon
--                                      (Vertex polygon, Vertex polygon)


-- outerBoundaryWithNeighbours :: (HasOuterBoundary polygon, VertexIx polygon ~ Int) => IndexedFold1 (VertexIx polygon, (VertexIx polygon, VertexIx polygon)) polygon (Vertex polygon, (Vertex polygon, Vertex polygon))










--------------------------------------------------------------------------------

-- | Convert a traversal into a fold.
asFold1   :: Traversal1 s t a b -> Fold1 s a
asFold1 t = \aFa -> phantom . t (phantom . aFa)


-- | Collect the connected components
collectComponents   :: forall cyclic f vertex r. ( Point_ vertex 2 r, Ord r, Fractional r
                       , VertexContainer f (OriginalOrExtra vertex (Point 2 r))
                       , Traversable1 cyclic, HasFromFoldable1 f
                       )
                    => LinePV 2 r -- ^ the bounding line of the halfplane
                    -> cyclic (Bool, NonEmpty vertex)
                    -> [HalfPlaneConvexPolygonIntersection f r vertex]
collectComponents l = foldMapOf (asFold1 withCyclicNeighbours) f
  where
    -- We go through the components with their neighbours. Each component
    -- is a non-empty list of vertices in CCW order along the polygon.
    --
    -- For each component [v1,..,vn] we may need to add two vertices; the intersection
    -- point of l with the edge between the last vertex um of the previous component and
    -- v1, and the intersection point of l with the edge between vn and the first vertex
    -- w1 of the next component
    f :: ((Bool, NonEmpty vertex), V2 (Bool, NonEmpty vertex))
      -> [HalfPlaneConvexPolygonIntersection f r vertex]
    f ((b, current@(v1 :| rest)), V2 (_, NonEmpty.last -> um) (_, w1 :| _))
      | not b     = []
      | otherwise = let vn     = NonEmpty.last current
                        extras = mapMaybe (intersectionPoint l) [(vn,w1), (um,v1)]
                    in pure $ case (NonEmpty.nonEmpty extras,NonEmpty.nonEmpty rest) of
                       (Nothing, Nothing)        -> DegenerateVertex v1
                       (Nothing, Just (p :| [])) -> DegenerateEdge
                                                  $ ClosedLineSegment p v1
                       (extras',  _)             -> ActualPolygon poly
                         where
                           poly = uncheckedFromCCWPoints
                                $ (fmap Extra <$> extras') <<> (Original <$> current)

-- | Helper to combine at most two a's into one
(<<>)     :: Semigroup a => Maybe a -> a -> a
xs <<> ys = case xs of
              Nothing  -> ys
              Just xs' -> xs' <> ys

-- | Compute the intersection between a line and the "edge" given by the two vertices.
-- We treat the edge as open; i.e. we only report the intersection if it is interior
--
-- pre:
intersectionPoint            :: (Point_ vertex 2 r, Ord r, Fractional r)
                             => LinePV 2 r -> (vertex, vertex) -> Maybe (Point 2 r)
intersectionPoint line (u,v) = case LinePV (u^.asPoint) (v .-. u) `intersect` line of
  Just (Line_x_Line_Point p) | p /= (u^.asPoint) -> Just p
  _                                              -> Nothing

-- TODO:: Appropriately handle degeneracies


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
         ) => IsIntersectableWith (HalfPlane (LinePV 2 r)) (SimplePolygonF f vertex) where
  halfPlane `intersect` poly = collectComponents (halfPlane^.boundingHyperPlane)
                             . groupWith (\v -> (v^.asPoint) `intersects` halfPlane) . Cyclic
                             $ toNonEmptyOf vertices poly

instance ( HasIntersectionWith (HalfPlane line) (SimplePolygonF f vertex)
         ) => HasIntersectionWith (HalfPlane line :+ extra)
                                  (SimplePolygonF f vertex :+ extra') where
  (halfPlane :+ _) `intersects` (poly :+ _) = halfPlane `intersects` poly
-}

instance ( Point_ vertex 2 r, Fractional r, Ord r, VertexContainer f vertex
         , VertexContainer f (OriginalOrExtra vertex (Point 2 r))
         , HasFromFoldable1 f
         ) => IsIntersectableWith (HalfPlane (LinePV 2 r)) (ConvexPolygonF f vertex) where
  halfPlane `intersect` poly = case comps of
      []  -> Nothing
      [c] -> Just c
      _   -> error "halfplane x convexPolygon intersection: absurd."
    where
      comps = collectComponents (halfPlane^.boundingHyperPlane)
            . groupWith (\v -> (v^.asPoint) `intersects` halfPlane) . Cyclic
            $ toNonEmptyOf vertices poly
  -- halfPlane `intersect` poly = case halfPlane `intersect` (toSimplePolygon poly) of
  --   [comp] -> Just $ ConvexPolygon <$> comp
  --             -- note that the intersection between a halfspace and a convex polygon
  --             -- is indeed guaranteed to be convex. Hence the 'ConvexPolygon' call here
  --             -- is safe.
  --   _      -> Nothing


instance ( IsIntersectableWith (HalfPlane line) (ConvexPolygonF f vertex)
         , HasIntersectionWith (HalfPlane line :+ extra) (ConvexPolygonF f vertex :+ extra')
         ) => IsIntersectableWith (HalfPlane line :+ extra)
                                  (ConvexPolygonF f vertex :+ extra') where
  (halfPlane :+ _) `intersect` (poly :+ _) = halfPlane `intersect` poly

--------------------------------------------------------------------------------


spec :: Spec
spec = describe "simple polygon x halfspace intersection" $ do
         testIpe [osp|polygonHalfspaceIntersection.ipe|]
                 [osp|polygonHalfspaceIntersection.out|]
         testIpe [osp|convexHalfspaceIntersection.ipe|]
                 [osp|convexHalfspaceIntersection.out|]

loadInputs      :: OsPath -> IO ( NonEmpty (HalfPlane (LinePV 2 R) :+ _)
                                , NonEmpty (ConvexPolygon (Point 2 R) :+ _)
                                )
loadInputs inFp = do
        inFp'      <- getDataFileName ([osp|test-with-ipe/Polygon/Simple/|] <> inFp)
        Right page <- readSinglePageFile inFp'
        let (rays :: NonEmpty (HalfLine (Point 2 R) :+ _))     = NonEmpty.fromList $ readAll page
            (pgs  :: NonEmpty (ConvexPolygon (Point 2 R) :+ _)) = NonEmpty.fromList $ readAll page
        -- take the left halfpalne of every halfline
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
      goldenWith [osp|data/test-with-ipe/Polygon/Simple/|]
                 (ipeContentGolden { name = outFp })
                 content'
    pure ()


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

ipeHalfPlane :: (Show r, Fractional r, Ord r) => IpeOut (HalfSpaceF (LinePV 2 r)) Group r
ipeHalfPlane = ipeHalfPlaneIn defaultBox

ipeHalfPlaneIn          :: (Ord r, Fractional r, Show r)
                        => Rectangle (Point 2 r) -> IpeOut (HalfSpaceF (LinePV 2 r)) Group r
ipeHalfPlaneIn rect' hl = ipeGroup [ iO $ ipeLineIn rect' (hl^.boundingHyperPlane)
                                   ]
  -- TDOO: I think we also want to display the interior

{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE QuasiQuotes #-}
module Polygon.Simple.IntersectHalfplaneSpec where


import           Control.Lens
import           Data.Foldable (toList, for_)
import           Data.Foldable1
import           Data.Functor.Contravariant (phantom)
import           Data.List.NonEmpty (NonEmpty(..))
import qualified Data.List.NonEmpty as NonEmpty
import           Data.Maybe (isJust, mapMaybe)
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
instance (Fractional r, Ord r, Show r) => HasDefaultFromIpe (HalfLine (Point 2 r)) where
  type DefaultFromIpe (HalfLine (Point 2 r)) = Path
  defaultFromIpe = _asHalfLine

-- | Try to parse an Line segment with an arrow head as a HalfLine
_asHalfLine :: (Fractional r, Ord r, Show r)
            => Prism' (IpeObject r) (HalfLine (Point 2 r) :+ IpeAttributes Path r)
_asHalfLine = prism' (\(hl :+ ats) -> IpePath (ipeHalfLine hl ! ats)) objToHalfLine
  where
    objToHalfLine = \case
      IpePath (path' :+ ats) -> case path'^?_asClosedLineSegment  of
        Just (ClosedLineSegment s t) -> case (hasAttr SArrow ats, hasAttr SRArrow ats) of
                                          (True,False) -> Just $ HalfLine s (t .-. s) :+ ats
                                          (False,True) -> Just $ HalfLine s (s .-. t) :+ ats
                                          _            -> Nothing
        Nothing                      -> Nothing
      _                    -> Nothing

    hasAttr a = isJust . lookupAttr a

--------------------------------------------------------------------------------

type HalfPlane line = HalfSpaceF line



--------------------------------------------------------------------------------

-- | A single component of a HalfPlane SimplePolygon intersection
data HalfPlanePolygonIntersection vertex polygon =
    HalfPlane_x_SimplePolygon_Vertex vertex
  | HalfPlane_x_SimplePolygon_Edge (ClosedLineSegment vertex)
  | HalfPlane_x_SimplePolygon_Polygon polygon
  deriving (Show,Eq,Functor)

type HalfPlaneSimplePolygonIntersection f r vertex =
  HalfPlanePolygonIntersection vertex (SimplePolygonF f (OriginalOrExtra vertex (Point 2 r)))

type HalfPlaneConvexPolygonIntersection f r vertex =
  HalfPlanePolygonIntersection vertex (ConvexPolygonF f (OriginalOrExtra vertex (Point 2 r)))


type instance Intersection (HalfPlane line) (SimplePolygonF f point) =
  [HalfPlaneSimplePolygonIntersection f (NumType point) point]

type instance Intersection (HalfPlane line :+ extra) (SimplePolygonF f point :+ extra') =
  Intersection (HalfPlane line) (SimplePolygonF f point)

type instance Intersection (HalfPlane line) (ConvexPolygonF f point) =
  Maybe (HalfPlaneConvexPolygonIntersection f (NumType point) point)

type instance Intersection (HalfPlane line :+ extra) (ConvexPolygonF f point :+ extra') =
  Maybe (HalfPlaneConvexPolygonIntersection f (NumType point) point)

instance ( Point_ point 2 r, Num r, Ord r, VertexContainer f point
         , HyperPlane_ line 2 r
         ) => HasIntersectionWith (HalfPlane line) (SimplePolygonF f point) where
  halfPlane `intersects` poly = anyOf (vertices.asPoint) (`intersects` halfPlane) poly

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

-- | Convert a traversal into a fold.
asFold1   :: Traversal1 s t a b -> Fold1 s a
asFold1 t = \aFa -> phantom . t (phantom . aFa)

-- | Collect the connected components
collectComponents   :: forall cyclic f vertex r. ( Point_ vertex 2 r, Ord r, Fractional r
                       , VertexContainer f (OriginalOrExtra vertex (Point 2 r))
                       , Traversable1 cyclic, HasFromFoldable1 f

                       , Show r, Show vertex
                       )
                    => LinePV 2 r -- ^ the bounding line of the halfplane
                    -> cyclic (Bool, NonEmpty vertex)
                    -> [HalfPlaneSimplePolygonIntersection f r vertex]
collectComponents l = foldMapOf (asFold1 withCyclicNeighbours) f
  where
    -- We go through the components with their neighbours. Each component
    -- is a non-empty list of vertices in CCW order along the polygon.
    --
    -- For each component [v1,..,vn] we may need to add two vertices; the intersection
    -- point of l with the edge between the last vertex um of the previous component and
    -- v1, and the intersection point of l with the edge between vn and the first vertex
    -- w1 of the next component
    --
    f :: ((Bool, NonEmpty vertex), V2 (Bool, NonEmpty vertex))
      -> [HalfPlaneSimplePolygonIntersection f r vertex]
    f x | traceShow x False = undefined

    f ((b, current@(v1 :| rest)), V2 (_, NonEmpty.last -> um) (_, w1 :| _))
      | not b     = []
      | otherwise = let vn     = NonEmpty.last current
                        extras = mapMaybe (intersectionPoint l) [(vn,w1), (um,v1)]
                    in pure $ case (NonEmpty.nonEmpty extras,NonEmpty.nonEmpty rest) of
                       (Nothing, Nothing)        -> HalfPlane_x_SimplePolygon_Vertex v1
                       (Nothing, Just (p :| [])) -> HalfPlane_x_SimplePolygon_Edge
                                                  $ ClosedLineSegment p v1
                       (extras',  _)             -> HalfPlane_x_SimplePolygon_Polygon poly
                         where
                           poly = uncheckedFromCCWPoints
                                $ (fmap Extra <$> extras') <<> (Original <$> current)

    -- todo; we are combining the components in the wrong way

xs <<> ys = case xs of
              Nothing  -> ys
              Just xs' -> xs' <> ys

-- | Compute the intersection between a line and the "edge" given by the two vertices.
intersectionPoint            :: (Point_ vertex 2 r, Ord r, Fractional r)
                             => LinePV 2 r -> (vertex, vertex) -> Maybe (Point 2 r)
intersectionPoint line (u,v) = case LinePV (u^.asPoint) (v .-. u) `intersect` line of
  Just (Line_x_Line_Point p) | p /= (u^.asPoint) -> Just p
  _                                              -> Nothing

-- TODO:: Appropriately handle degeneracies


-- flatten :: NonEmpty (NonEmpty (a, b)) -> NonEmpty (b, NonEmpty a)
-- flatten = fmap (\((x,b) :| xs) -> (b, x :| map fst xs))


instance ( Point_ vertex 2 r, Fractional r, Ord r, VertexContainer f vertex
         , VertexContainer f (OriginalOrExtra vertex (Point 2 r))
         , HasFromFoldable1 f
         , Show r, Show vertex
         ) => IsIntersectableWith (HalfPlane (LinePV 2 r)) (SimplePolygonF f vertex) where
  halfPlane `intersect` poly = collectComponents (halfPlane^.boundingHyperPlane)
                             . groupWith (\v -> (v^.asPoint) `intersects` halfPlane) . Cyclic
                             $ toNonEmptyOf vertices poly

-- instance ( HasIntersectionWith (HalfPlane line) (SimplePolygonF f vertex)
--          ) => HasIntersectionWith (HalfPlane line :+ extra)
--                                   (SimplePolygonF f vertex :+ extra') where
--   (halfPlane :+ _) `intersects` (poly :+ _) = halfPlane `intersects` poly

instance ( IsIntersectableWith (HalfPlane line) (SimplePolygonF f vertex)
         , HasIntersectionWith (HalfPlane line :+ extra) (SimplePolygonF f vertex :+ extra')
         ) => IsIntersectableWith (HalfPlane line :+ extra)
                                  (SimplePolygonF f vertex :+ extra') where
  (halfPlane :+ _) `intersect` (poly :+ _) = halfPlane `intersect` poly

instance ( Point_ point 2 r, Num r, Ord r, VertexContainer f point
         , HyperPlane_ line 2 r
         ) => HasIntersectionWith (HalfPlane line) (ConvexPolygonF f point) where
  halfPlane `intersects` poly = halfPlane `intersects` (toSimplePolygon poly)
    -- TODO there is a better, O(log n) time implementation. use that instead ...

instance ( Point_ vertex 2 r, Fractional r, Ord r, VertexContainer f vertex
         , VertexContainer f (OriginalOrExtra vertex (Point 2 r))
         , HasFromFoldable1 f
         , Show r, Show vertex
         ) => IsIntersectableWith (HalfPlane (LinePV 2 r)) (ConvexPolygonF f vertex) where
  halfPlane `intersect` poly = case halfPlane `intersect` (toSimplePolygon poly) of
    [comp] -> Just $ ConvexPolygon <$> comp
              -- note that the intersection between a halfspace and a convex polygon
              -- is indeed guaranteed to be convex. Hence the 'ConvexPolygon' call here
              -- is safe.
    _      -> Nothing

--------------------------------------------------------------------------------


spec :: Spec
spec = describe "simple polygon x halfspace intersection" $ do
         testIpe [osp|polygonHalfspaceIntersection.ipe|]
                 [osp|polygonHalfspaceIntersection.out|]
         testIpe [osp|polygonHalfspaceIntersection1.ipe|]
                 [osp|polygonHalfspaceIntersection1.out|]
         testIpe [osp|convexHalfspaceIntersection.ipe|]
                 [osp|convexHalfspaceIntersection.out|]

loadInputs      :: OsPath -> IO ( NonEmpty (HalfPlane (LinePV 2 R) :+ _)
                                , NonEmpty (SimplePolygon (Point 2 R) :+ _)
                                )
loadInputs inFp = do
        inFp'      <- getDataFileName ([osp|test-with-ipe/Polygon/Simple/|] <> inFp)
        Right page <- readSinglePageFile inFp'
        let (rays :: NonEmpty (HalfLine (Point 2 R) :+ _))     = NonEmpty.fromList $ readAll page
            (pgs  :: NonEmpty (SimplePolygon (Point 2 R) :+ _)) = NonEmpty.fromList $ readAll page
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
             HalfPlane_x_SimplePolygon_Vertex _     -> pure ()
             HalfPlane_x_SimplePolygon_Edge _       -> pure ()
             HalfPlane_x_SimplePolygon_Polygon poly ->
               fromPoints @(SimplePolygon _) (toNonEmptyOf vertices poly)
               `shouldSatisfy` isJust

    let forMap'  = flip foldMap
        content' = forMap' polygons $ \polygon ->
                      forMap' halfPlanes $ \halfPlane ->
                         [ iO' polygon
                         , iO $ defIO (halfPlane^.core)
                         , iO $ ipeGroup [ renderComponent comp
                                         | comp <- halfPlane `intersect` polygon
                                         ]
                         ]
    describe "compute intersection" $
      goldenWith [osp|data/test-with-ipe/Polygon/Simple/|]
                 (ipeContentGolden { name = outFp })
                 content'
    pure ()


renderComponent :: forall vertex f r.
                   ( Point_ vertex 2 r, Foldable1 f
                   , HasVertices (SimplePolygonF f (OriginalOrExtra vertex (Point 2 r)))
                                 (SimplePolygonF f (Point 2 r))
                   --  VertexContainer f (OriginalOrExtra vertex (Point 2 r))
                   -- , VertexContainer f (Point 2 r)
                   )
                => HalfPlaneSimplePolygonIntersection f r vertex -> IpeObject r
renderComponent = \case
    HalfPlane_x_SimplePolygon_Vertex v     -> iO $ defIO (v^.asPoint)
                                                 ! attr SStroke red
    HalfPlane_x_SimplePolygon_Edge e       -> iO $ defIO (view asPoint <$> e)
                                                 ! attr SStroke red
    HalfPlane_x_SimplePolygon_Polygon poly -> iO $ ipeSimplePolygon @f (poly&vertices %~ getPt)
                                                 ! attr SFill red
  where
    getPt = \case
      Original v -> v^.asPoint
      Extra p    -> p


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

{-# LANGUAGE UndecidableInstances #-}
module PlaneGraph.FromPolygonsSpec where

import           Control.Lens
import           Data.Coerce
import           Data.Foldable1
import           Data.Functor.Apply (WrappedApplicative(..))
import           Data.List.NonEmpty (NonEmpty(..))
import qualified Data.List.NonEmpty as NonEmpty
import           Data.Vector.NonEmpty (NonEmptyVector)
import           GHC.Generics (Generic)
import           HGeometry.Boundary
import           HGeometry.Ext
import           HGeometry.Foldable.Util
import           HGeometry.LineSegment
import           HGeometry.Number.Real.Rational
import           HGeometry.PlaneGraph
import           HGeometry.Point
import           HGeometry.Polygon
import           HGeometry.Properties
import           Hiraffe.PlanarGraph
import qualified Hiraffe.PlanarGraph.Dart as Dart
import           System.OsPath
import           Test.Hspec
import Ipe


--------------------------------------------------------------------------------

type R = RealNumber 5


-- deriving stock Show (PlaneGraph s v e f)

--------------------------------------------------------------------------------


-- | Given a connected Plane graph, constructs a PlaneGraph.
--
-- \(O(n)\)
fromCPlaneGraph   :: ( Point_ vertex 2 r, Ord r, Num r
                     ) => CPlaneGraph s vertex e f -> PlaneGraph s vertex e f
fromCPlaneGraph c = fromCPlaneGraphWith (outerFaceDart c) c

-- | Given a dart that thas the outer face on its left, and a Connected Plane Graph,
-- construct a PlaneGraph.
fromCPlaneGraphWith                  :: Dart.Dart s -> CPlaneGraph s v e f -> PlaneGraph s v e f
fromCPlaneGraphWith outerFaceDart' c = review _PlanarGraph
                                     $ fromConnected' (c^._CPlanarGraph) outerFaceDart'





-- | Given a set of line segments that may intersect only in endpoints, construct
-- the planeGraph these segments represent.
fromDisjointSegments      :: ( Foldable1 nonEmpty, Ord r, Num r
                             , LineSegment_ lineSegment point
                             , Point_ point 2 r
                             )
                          => nonEmpty lineSegment
                          -> PlaneGraph s (NonEmpty.NonEmpty point) lineSegment ()
fromDisjointSegments segs = undefined


-- | Construct a PlaneGraph from a single polygon (which may have holes).
fromPolygon      :: ()
                 => polygon
                 -> PlaneGraph s (vertex :+ VertexIx polygon)
                                 (EdgeIx polygon)
                                 PointLocationResult
fromPolygon poly = undefined
-- TODO: tag the holes ?


fromPolygons :: ( Polygon_ polygon vertex r
                , Foldable1 nonEmpty
                ) => nonEmpty polygon -> PlaneGraph s vertex () [polygon]
fromPolygons = undefined


--------------------------------------------------------------------------------

-- instance ConstructableGraph_ (PlanarGraph s Primal v e f) where
--   fromAdjacencyLists xs = PlanarGraph components
--                                       vtxData dartData faceData
--     where


--       gr :: Graph v e
--       gr = fromAdjacencyLists xs

--       components' = dff gr



    -- PlanarGraph comps






--------------------------------------------------------------------------------

-- | Construct a connected PlaneGraph from a single simple polygon.
--
-- the interior of the polygon will have faceId 0
--
-- running time: \(O(n)\).
fromSimplePolygon      :: ( SimplePolygon_ simplePolygon vertex r
                          , EdgeIx simplePolygon ~ Int
                          )
                       => simplePolygon
                       -> CPlaneGraph s (vertex :+ VertexIx simplePolygon)
                                        (EdgeIx simplePolygon)
                                        PointLocationResult
fromSimplePolygon poly = fromSimplePolygonWith poly Inside Outside

-- | Construct a plane graph from a simple polygon.
--
-- the interior of the polygon will have faceId 0
--
-- running time: \(O(n)\).
fromSimplePolygonWith                     :: ( SimplePolygon_ simplePolygon vertex r
                                             , EdgeIx simplePolygon ~ Int
                                             )
                                          => simplePolygon
                                          -> face
                                          -- ^ Data for the face representing the inside
                                          -> face
                                          -- ^ Data for the face representing the outside
                                          -> CPlaneGraph s (vertex :+ VertexIx simplePolygon)
                                                           (EdgeIx simplePolygon)
                                                           face
fromSimplePolygonWith poly inData outData = cPlaneGraph $ g&faceData   .~ fData
                                                           &vertexData .~ vData
  where
    cPlaneGraph = review _CPlanarGraph

    fData  = fromNonEmpty $ inData :| [outData]
    n      = numVertices poly
    -- We explicitly construct two darts around every vertex.
    g      = planarGraph $ foldMapOf (vertices.asIndex) mkDarts poly
    mkDarts i = NonEmpty.singleton $ NonEmpty.fromList
                [ (Dart.Dart (Dart.Arc i)               Dart.Positive, i)
                , (Dart.Dart (Dart.Arc $ (i+1) `mod` n) Dart.Negative, i)
                ]
    vData = fromFoldable1 $ toNonEmptyOf (vertices.asIndexedExt) poly
    -- this part may not be ideal; in the case of SimplePolygon vertex this is
    -- converting from array into NonEmpty and Back

-- TODO: the 'planarGraph' function might as well take the 'v' values
-- so that we don't have to fiddle with the vData ourselves.


--------------------------------------------------------------------------------

-- instance ( Point_ v 2 (NumType v)
--          , Ord (NumType v), Num (NumType v)

--          ) => PlanarGraph_ (PlaneGraph s v e f) where
--   type DualGraphOf (PlaneGraph s v e f) = PlanarGraph Dual s f e v


--------------------------------------------------------------------------------



{-
-}



instance ( -- PlanarGraph_ (Component s)
         -- , IsComponent s
         -- , EdgeIx   (Component s) ~ Dart.Dart (Wrap s)
         -- , Edge     (Component s) ~ Dart.Dart s
         ) => PlanarGraph_ (PlanarGraph w s v e f) where
  -- dualGraph, (incidentFaceOf | leftFaceOf), rightFaceOf, prevDartOf, nextDartOf, boundaryDartOf, boundaryDartOf, boundaryDarts
  type DualGraphOf (PlanarGraph w s v e f) = CPlanarGraph (DualOf w) s f e v
  type WorldOf     (PlanarGraph w s v e f) = w

  dualGraph =  computeDualGraph
  _DualFaceIx     _ = undefined
  _DualVertexIx   _ = undefined
  incidentFaceOf  d = undefined
  rightFaceOf     d = undefined
  prevDartOf      d = undefined
  nextDartOf      d = undefined
  boundaryDartOf  f = undefined
  boundaryDarts   f g = undefined



computeDualGraph :: PlanarGraph w s v e f -> CPlanarGraph (DualOf w) s f e v
computeDualGraph = undefined



testPoly :: SimplePolygon (Point 2 Int :+ Int)
testPoly = uncheckedFromCCWPoints $ NonEmpty.fromList
           [ origin :+ 0
           , Point2 10 0 :+ 1
           , Point2 10 10 :+ 2
           , Point2 0 10 :+ 3
           ]


instance VertexContainer f vertex => HasEdges' (SimplePolygonF f vertex) where
  type Edge   (SimplePolygonF f vertex) = ()
  type EdgeIx (SimplePolygonF f vertex) = VertexIx (SimplePolygonF f vertex)
  edgeAt u = \pUnitFUnit poly -> poly <$ indexed pUnitFUnit u ()
  -- unclear whether we should use conjoined here.
  numEdges = numVertices

instance VertexContainer f vertex
         => HasEdges (SimplePolygonF f vertex) (SimplePolygonF f vertex) where
  edges = conjoined trav (itrav.indexed)
    where
      trav        :: Applicative g
                  => (() -> g ()) -> SimplePolygonF f vertex -> g (SimplePolygonF f vertex)
      trav f poly = unwrapApplicative $
                    poly <$ (vertices' (\x -> x <$ WrapApplicative (f ())) poly)

      itrav        :: Applicative g
                   => (VertexIx (SimplePolygonF f vertex) -> () -> g ())
                   -> SimplePolygonF f vertex -> g (SimplePolygonF f vertex)
      itrav f poly = unwrapApplicative $
                     poly <$ vertices' (Indexed $ \v x -> x <$ WrapApplicative (f v ())) poly

      vertices' :: IndexedTraversal1' (VertexIx (SimplePolygonF f vertex))
                                      (SimplePolygonF f vertex) vertex
      vertices' = vertices

spec :: Spec
spec = describe "Constructing a PlaneGraph from overlapping Polygons" $ do
         it "vertices testPoly ok" $
           let g = fromSimplePolygonWith testPoly "In" "Out"
           in (g^..vertices) `shouldBe` (testPoly^..vertices.asIndexedExt)
         it "vertices testPoly ok indexed" $
           let g = fromSimplePolygonWith testPoly "In" "Out"
           in allOf (vertices.withIndex) (\(i,_ :+ j) -> coerce i == j) g `shouldBe` True
         -- it "all inside" $
         --   let g = fromSimplePolygonWith testPoly "In" "Out"
         --   in allOf (vertices.withIndex) (\v _ -> v&outNeighboursOfByDart )




--------------------------------------------------------------------------------

readInput      :: OsPath -> IO (NonEmpty (ClosedLineSegment (Point 2 R) :+ _))
readInput inFP = NonEmpty.fromList <$> readAllFrom inFP

testIpe inFP outFP = describe ("Constructing PlaneGraph from " <> show inFP) $ do
    segs <- runIO $ readInput inFP
    let (gr :: PlaneGraph () (NonEmpty (Point 2 R))
                             (ClosedLineSegment (Point 2 R) :+ _)
                             ()
          )  = fromDisjointSegments segs
    it "test" $ do
      show gr `shouldBe` ""

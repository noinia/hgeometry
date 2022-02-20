{-# LANGUAGE OverloadedStrings #-}
--------------------------------------------------------------------------------
-- |
-- Module      :  Data.Geometry.Polygon.Core
-- Copyright   :  (C) Frank Staals
-- License     :  see the LICENSE file
-- Maintainer  :  Frank Staals
--
-- A Polygon data type and some basic functions to interact with them.
--
--------------------------------------------------------------------------------
module Data.Geometry.Polygon.Core
  ( PolygonType(..)
  , Polygon(..)
  , Vertices
  , _SimplePolygon, _MultiPolygon
  , SimplePolygon, MultiPolygon, SomePolygon

    -- * Construction
  , fromPoints
  , fromCircularVector

  , simpleFromPoints
  , simpleFromCircularVector

  , unsafeFromPoints
  , unsafeFromCircularVector
  , unsafeFromVector
  , toVector
  , toPoints

  , isSimple

  , size
  , polygonVertices, listEdges

  , outerBoundary, outerBoundaryVector
  , unsafeOuterBoundaryVector
  , outerBoundaryEdges
  , outerVertex, unsafeOuterVertex
  , outerBoundaryEdge

  , polygonHoles, polygonHoles'
  , holeList

  , area, signedArea

  , centroid
  , pickPoint

  , isTriangle

  , isCounterClockwise
  , toCounterClockWiseOrder, toCounterClockWiseOrder'
  , toClockwiseOrder, toClockwiseOrder'
  , reverseOuterBoundary

  , findDiagonal

  , withIncidentEdges, numberVertices

  -- * Testing for Reflex or Convex

  , isReflexVertex, isConvexVertex, isStrictlyConvexVertex
  , reflexVertices, convexVertices, strictlyConvexVertices

    -- * Specialized folds
  , maximumVertexBy
  , minimumVertexBy
  , findRotateTo
  , rotateLeft
  , rotateRight
  ) where

import qualified Algorithms.Geometry.LineSegmentIntersection.BentleyOttmann as BO
import           Control.DeepSeq
import           Control.Lens                                               (Getter, Lens', Prism',
                                                                             Traversal', lens, over,
                                                                             prism', to, toListOf,
                                                                             view, (%~), (&), (.~),
                                                                             (^.))
import           Data.Aeson
import           Data.Bifoldable
import           Data.Bifunctor
import           Data.Bitraversable
import           Data.Ext
import qualified Data.Foldable                                              as F
import           Data.Geometry.Boundary
import           Data.Geometry.Box                                          (IsBoxable (..),
                                                                             boundingBoxList')
import           Data.Geometry.Line
import           Data.Geometry.LineSegment
import           Data.Geometry.Point
import           Data.Geometry.Properties
import           Data.Geometry.Transformation
import           Data.Geometry.Triangle                                     (Triangle (..),
                                                                             inTriangle)
import           Data.Geometry.Vector                                       (Additive (zero, (^+^)),
                                                                             Affine ((.+^), (.-.)),
                                                                             (*^), (^*), (^/))
import qualified Data.List                                                  as List
import qualified Data.List.NonEmpty                                         as NonEmpty
import           Data.Maybe                                                 (catMaybes)
import           Data.Ord                                                   (comparing)
import           Data.Semigroup                                             (sconcat)
import           Data.Semigroup.Foldable
import           Data.Util
import           Data.Vector                                                (Vector)
import qualified Data.Vector                                                as V
import           Data.Vector.Circular                                       (CircularVector)
import qualified Data.Vector.Circular                                       as CV
import qualified Data.Vector.Circular.Util                                  as CV


-- import Data.RealNumber.Rational

--------------------------------------------------------------------------------

{- $setup
>>> import Data.RealNumber.Rational
>>> import Data.Foldable
>>> import Control.Lens.Extras
>>> :{
-- import qualified Data.Vector.Circular as CV
let simplePoly :: SimplePolygon () (RealNumber 10)
    simplePoly = fromPoints . map ext $
      [ Point2 0 0
      , Point2 10 0
      , Point2 10 10
      , Point2 5 15
      , Point2 1 11
      ]
    simpleTriangle :: SimplePolygon () (RealNumber 10)
    simpleTriangle = fromPoints  . map ext $
      [ Point2 0 0, Point2 2 0, Point2 1 1]
    multiPoly :: MultiPolygon () (RealNumber 10)
    multiPoly = MultiPolygon
      (fromPoints . map ext $ [Point2 (-1) (-1), Point2 3 (-1), Point2 2 2])
      [simpleTriangle]
:} -}

-- | We distinguish between simple polygons (without holes) and polygons with holes.
data PolygonType = Simple | Multi

-- | Polygons are sequences of points and may or may not contain holes.
--
--   Degenerate polygons (polygons with self-intersections or fewer than 3 points)
--   are only possible if you use functions marked as unsafe.
data Polygon (t :: PolygonType) p r where
  SimplePolygon :: Vertices (Point 2 r :+ p)                -> SimplePolygon p r
  MultiPolygon  :: SimplePolygon p r -> [SimplePolygon p r] -> MultiPolygon  p r

newtype Vertices a = Vertices (CircularVector a)
  deriving (Functor, Foldable, Foldable1, Traversable, NFData, Eq, Ord)

-- | Prism to 'test' if we are a simple polygon
--
-- >>> is _SimplePolygon simplePoly
-- True
_SimplePolygon :: Prism' (Polygon Simple p r) (Vertices (Point 2 r :+ p))
_SimplePolygon = prism' SimplePolygon (\(SimplePolygon vs) -> Just vs)

-- | Prism to 'test' if we are a Multi polygon
--
-- >>> is _MultiPolygon multiPoly
-- True
_MultiPolygon :: Prism' (Polygon Multi p r) (Polygon Simple p r, [Polygon Simple p r])
_MultiPolygon = prism' (uncurry MultiPolygon) (\(MultiPolygon vs hs) -> Just (vs,hs))

instance Functor (Polygon t p) where
  fmap = bimap id

instance Bifunctor (Polygon t) where
  bimap = bimapDefault

instance Bifoldable (Polygon t) where
  bifoldMap = bifoldMapDefault

instance Bitraversable (Polygon t) where
  bitraverse f g p = case p of
    SimplePolygon vs   -> SimplePolygon <$> bitraverseVertices f g vs
    MultiPolygon vs hs -> MultiPolygon  <$> bitraverse f g vs
                                        <*> traverse (bitraverse f g) hs

instance (NFData p, NFData r) => NFData (Polygon t p r) where
  rnf (SimplePolygon vs)   = rnf vs
  rnf (MultiPolygon vs hs) = rnf (vs,hs)

bitraverseVertices     :: (Applicative f, Traversable t) => (p -> f q) -> (r -> f s)
                  -> t (Point 2 r :+ p) -> f (t (Point 2 s :+ q))
bitraverseVertices f g = traverse (bitraverse (traverse g) f)

-- | Polygon without holes.
type SimplePolygon = Polygon Simple

-- | Polygon with zero or more holes.
type MultiPolygon  = Polygon Multi

-- | Either a simple or multipolygon
type SomePolygon p r = Either (Polygon Simple p r) (Polygon Multi p r)

type instance Dimension (SomePolygon p r) = 2
type instance NumType   (SomePolygon p r) = r

-- | Polygons are per definition 2 dimensional
type instance Dimension (Polygon t p r) = 2
type instance NumType   (Polygon t p r) = r

instance (Show p, Show r) => Show (Polygon t p r) where
  show (SimplePolygon vs)   = "SimplePolygon " <> show (F.toList vs)
  show (MultiPolygon vs hs) = "MultiPolygon (" <> show vs <> ") (" <> show hs <> ")"

instance (Read p, Read r) => Read (SimplePolygon p r) where
  readsPrec d = readParen (d > app_prec) $ \r ->
      [ (unsafeFromPoints vs, t)
      | ("SimplePolygon", s) <- lex r, (vs, t) <- reads s ]
    where app_prec = 10

instance (Read p, Read r) => Read (MultiPolygon p r) where
  readsPrec d = readParen (d > app_prec) $ \r ->
      [ (MultiPolygon vs hs, t')
      | ("MultiPolygon", s) <- lex r
      , (vs, t) <- reads s
      , (hs, t') <- reads t ]
    where app_prec = 10

-- instance (Read p, Read r) => Show (Polygon t p r) where
--   show (SimplePolygon vs)   = "SimplePolygon (" <> show vs <> ")"
--   show (MultiPolygon vs hs) = "MultiPolygon (" <> show vs <> ") (" <> show hs <> ")"



instance (Eq p, Eq r) => Eq (Polygon t p r) where
  (SimplePolygon vs)   == (SimplePolygon vs')    = vs == vs'
  (MultiPolygon vs hs) == (MultiPolygon vs' hs') = vs == vs' && hs == hs'

instance PointFunctor (Polygon t p) where
  pmap f (SimplePolygon vs)   = SimplePolygon (fmap (first f) vs)
  pmap f (MultiPolygon vs hs) = MultiPolygon  (pmap f vs) (map (pmap f) hs)

instance Fractional r => IsTransformable (Polygon t p r) where
  transformBy = transformPointFunctor

instance IsBoxable (Polygon t p r) where
  boundingBox = boundingBoxList' . toListOf (outerBoundaryVector.traverse.core)


instance (ToJSON r, ToJSON p) => ToJSON (Polygon t p r) where
  toJSON     = \case
    (SimplePolygon vs)   -> object [ "tag"           .= ("SimplePolygon" :: String)
                                   , "vertices"      .= F.toList vs
                                   ]
    (MultiPolygon vs hs) -> object [ "tag"           .= ("MultiPolygon" :: String)
                                   , "outerBoundary" .= getVertices vs
                                   , "holes"         .= map getVertices hs
                                   ]
      where
        getVertices = view (outerBoundaryVector.to F.toList)

instance (FromJSON r, Eq r, Num r, FromJSON p) => FromJSON (Polygon Simple p r) where
  parseJSON = withObject "Polygon" $ \o -> o .: "tag" >>= \case
                                             "SimplePolygon" -> pSimple o
                                             (_ :: String)   -> fail "Not a SimplePolygon"
    where
      pSimple o = fromPoints <$> o .: "vertices"

instance (FromJSON r, Eq r, Num r, FromJSON p) => FromJSON (Polygon Multi p r) where
  parseJSON = withObject "Polygon" $ \o -> o .: "tag" >>= \case
                                             "MultiPolygon"  -> pMulti o
                                             (_ :: String)   -> fail "Not a MultiPolygon"
    where
      pMulti  o = (\vs hs -> MultiPolygon (fromPoints vs) (map fromPoints hs))
               <$> o .: "outerBoundary" <*> o .: "holes"

instance (Fractional r, Ord r) => HasSquaredEuclideanDistance (Boundary (Polygon t p r)) where
  pointClosestToWithDistance q = minimumBy (comparing snd)
                               . fmap (pointClosestToWithDistance q)
                               . listEdges . review _Boundary

instance (Fractional r, Ord r) => HasSquaredEuclideanDistance (Polygon t p r) where
  pointClosestToWithDistance q pg
    | q `intersects` pg = (q, 0)
    | otherwise         = pointClosestToWithDistance q (Boundary pg)


-- * Functions on Polygons

-- | Getter access to the outer boundary vector of a polygon.
--
-- >>> toList (simpleTriangle ^. outerBoundaryVector)
-- [Point2 0 0 :+ (),Point2 2 0 :+ (),Point2 1 1 :+ ()]
outerBoundaryVector :: forall t p r. Getter (Polygon t p r) (CircularVector (Point 2 r :+ p))
outerBoundaryVector = to g
  where
    g                     :: Polygon t p r -> CircularVector (Point 2 r :+ p)
    g (SimplePolygon (Vertices vs))                  = vs
    g (MultiPolygon (SimplePolygon (Vertices vs)) _) = vs

-- | Unsafe lens access to the outer boundary vector of a polygon.
--
-- >>> toList (simpleTriangle ^. unsafeOuterBoundaryVector)
-- [Point2 0 0 :+ (),Point2 2 0 :+ (),Point2 1 1 :+ ()]
--
-- >>> simpleTriangle & unsafeOuterBoundaryVector .~ CV.singleton (Point2 0 0 :+ ())
-- SimplePolygon [Point2 0 0 :+ ()]
unsafeOuterBoundaryVector :: forall t p r. Lens' (Polygon t p r) (CircularVector (Point 2 r :+ p))
unsafeOuterBoundaryVector = lens g s
  where
    g                     :: Polygon t p r -> CircularVector (Point 2 r :+ p)
    g (SimplePolygon (Vertices vs))                  = vs
    g (MultiPolygon (SimplePolygon (Vertices vs)) _) = vs

    s                           :: Polygon t p r -> CircularVector (Point 2 r :+ p)
                                -> Polygon t p r
    s SimplePolygon{}     vs = SimplePolygon (Vertices vs)
    s (MultiPolygon _ hs) vs = MultiPolygon (SimplePolygon (Vertices vs)) hs


-- | \( O(1) \) Lens access to the outer boundary of a polygon.
outerBoundary :: forall t p r. Lens' (Polygon t p r) (SimplePolygon p r)
outerBoundary = lens g s
  where
    g                     :: Polygon t p r -> SimplePolygon p r
    g poly@SimplePolygon{}    = poly
    g (MultiPolygon simple _) = simple

    s                           :: Polygon t p r -> SimplePolygon p r
                                -> Polygon t p r
    s SimplePolygon{} simple     = simple
    s (MultiPolygon _ hs) simple = MultiPolygon simple hs

-- | Lens access for polygon holes.
--
-- >>> multiPoly ^. polygonHoles
-- [SimplePolygon [Point2 0 0 :+ (),Point2 2 0 :+ (),Point2 1 1 :+ ()]]
polygonHoles :: forall p r. Lens' (Polygon Multi p r) [Polygon Simple p r]
polygonHoles = lens g s
  where
    g                     :: Polygon Multi p r -> [Polygon Simple p r]
    g (MultiPolygon _ hs) = hs
    s                     :: Polygon Multi p r -> [Polygon Simple p r]
                          -> Polygon Multi p r
    s (MultiPolygon vs _) = MultiPolygon vs

{- HLINT ignore polygonHoles' -}
-- | \( O(1) \). Traversal lens for polygon holes. Does nothing for simple polygons.
polygonHoles' :: Traversal' (Polygon t p r) [Polygon Simple p r]
polygonHoles' = \f -> \case
  p@SimplePolygon{}  -> pure p
  MultiPolygon vs hs -> MultiPolygon vs <$> f hs

-- | /O(1)/ Access the i^th vertex on the outer boundary. Indices are modulo \(n\).
--
-- >>> simplePoly ^. outerVertex 0
-- Point2 0 0 :+ ()
outerVertex   :: Int -> Getter (Polygon t p r) (Point 2 r :+ p)
outerVertex i = outerBoundaryVector . CV.item i

-- | \( O(1) \) read and \( O(n) \) write. Access the i^th vertex on the outer boundary
--
-- >>> simplePoly ^. unsafeOuterVertex 0
-- Point2 0 0 :+ ()
-- >>> simplePoly & unsafeOuterVertex 0 .~ (Point2 10 10 :+ ())
-- SimplePolygon [Point2 10 10 :+ (),Point2 10 0 :+ (),Point2 10 10 :+ (),Point2 5 15 :+ (),Point2 1 11 :+ ()]
unsafeOuterVertex   :: Int -> Lens' (Polygon t p r) (Point 2 r :+ p)
unsafeOuterVertex i = unsafeOuterBoundaryVector . CV.item i

-- | \( O(1) \) Get the n^th edge along the outer boundary of the polygon. The edge is half open.
outerBoundaryEdge     :: Int -> Polygon t p r -> LineSegment 2 p r
outerBoundaryEdge i p = let u = p^.outerVertex i
                            v = p^.outerVertex (i+1)
                        in LineSegment (Closed u) (Open v)


-- | Get all holes in a polygon
holeList                     :: Polygon t p r -> [Polygon Simple p r]
holeList SimplePolygon{}     = []
holeList (MultiPolygon _ hs) = hs


-- | \( O(1) \) Vertex count. Includes the vertices of holes.
size :: Polygon t p r -> Int
size (SimplePolygon (Vertices cv)) = F.length cv
size (MultiPolygon b hs)           = sum (map size (b:hs))

-- | \( O(n) \) The vertices in the polygon. No guarantees are given on the order in which
-- they appear!
polygonVertices                      :: Polygon t p r
                                     -> NonEmpty.NonEmpty (Point 2 r :+ p)
polygonVertices p@SimplePolygon{}    = toNonEmpty $ p^.outerBoundaryVector
polygonVertices (MultiPolygon vs hs) =
  sconcat $ toNonEmpty (polygonVertices vs) NonEmpty.:| map polygonVertices hs

-- FIXME: Get rid of 'Fractional r' constraint.
-- | \( O(n \log n) \) Check if a polygon has any holes, duplicate points, or
--   self-intersections.
isSimple :: (Ord r, Fractional r) => Polygon p t r -> Bool
isSimple p@SimplePolygon{}   = null . BO.interiorIntersections $ listEdges p
isSimple (MultiPolygon b []) = isSimple b
isSimple MultiPolygon{}      = False

requireThree :: String -> [a] -> [a]
requireThree _ lst@(_:_:_:_) = lst
requireThree label _ = error $
  "Data.Geometry.Polygon." ++ label ++ ": Polygons must have at least three points."

-- | \( O(n) \) Creates a polygon from the given list of vertices.
--
-- The points are placed in CCW order if they are not already. Overlapping
-- edges and repeated vertices are allowed.
--
fromPoints :: forall p r. (Eq r, Num r) => [Point 2 r :+ p] -> SimplePolygon p r
fromPoints = fromCircularVector . CV.unsafeFromList . requireThree "fromPoints"

-- | \( O(n) \) Creates a polygon from the given vector of vertices.
--
-- The points are placed in CCW order if they are not already. Overlapping
-- edges and repeated vertices are allowed.
--
fromCircularVector :: forall p r. (Eq r, Num r) => CircularVector (Point 2 r :+ p) -> SimplePolygon p r
fromCircularVector = toCounterClockWiseOrder . unsafeFromCircularVector

-- | \( O(n \log n) \) Creates a simple polygon from the given list of vertices.
--
-- The points are placed in CCW order if they are not already. Overlapping
-- edges and repeated vertices are /not/ allowed and will trigger an exception.
--
simpleFromPoints :: forall p r. (Ord r, Fractional r) => [Point 2 r :+ p] -> SimplePolygon p r
simpleFromPoints =
  simpleFromCircularVector . CV.unsafeFromList . requireThree "simpleFromPoints"

-- | \( O(n \log n) \) Creates a simple polygon from the given vector of vertices.
--
-- The points are placed in CCW order if they are not already. Overlapping
-- edges and repeated vertices are /not/ allowed and will trigger an exception.
--
simpleFromCircularVector :: forall p r. (Ord r, Fractional r)
  => CircularVector (Point 2 r :+ p) -> SimplePolygon p r
simpleFromCircularVector v =
  let p = fromCircularVector v
      hasInteriorIntersections = not . null . BO.interiorIntersections
  in if hasInteriorIntersections (listEdges p)
      then error "Data.Geometry.Polygon.simpleFromCircularVector: \
                 \Found self-intersections or repeated vertices."
      else p

-- | \( O(n) \) Creates a simple polygon from the given list of vertices.
--
-- pre: the input list constains no repeated vertices.
unsafeFromPoints :: [Point 2 r :+ p] -> SimplePolygon p r
unsafeFromPoints = unsafeFromCircularVector . CV.unsafeFromList

-- | \( O(1) \) Creates a simple polygon from the given vector of vertices.
--
-- pre: the input list constains no repeated vertices.
unsafeFromCircularVector :: CircularVector (Point 2 r :+ p) -> SimplePolygon p r
unsafeFromCircularVector = SimplePolygon . Vertices

-- | \( O(1) \) Creates a simple polygon from the given vector of vertices.
--
-- pre: the input list constains no repeated vertices.
unsafeFromVector :: Vector (Point 2 r :+ p) -> SimplePolygon p r
unsafeFromVector = unsafeFromCircularVector . CV.unsafeFromVector

-- -- | Polygon points, from left to right.
-- toList :: Polygon t p r -> [Point 2 r :+ p]
-- toList (SimplePolygon c)   = F.toList c
-- toList (MultiPolygon s hs) = toList s ++ concatMap toList hs

-- | \( O(n) \)
--   Polygon points, from left to right.
toVector :: Polygon t p r -> Vector (Point 2 r :+ p)
toVector p@SimplePolygon{}   = CV.toVector $ p^.outerBoundaryVector
toVector (MultiPolygon s hs) = foldr (<>) (toVector s) (map toVector hs)

-- | \( O(n) \)
--   Polygon points, from left to right.
toPoints :: Polygon t p r -> [Point 2 r :+ p]
toPoints = V.toList . toVector

-- | \( O(n) \) The edges along the outer boundary of the polygon. The edges are half open.
outerBoundaryEdges :: Polygon t p r -> CircularVector (LineSegment 2 p r)
outerBoundaryEdges = toEdges . (^.outerBoundaryVector)

-- | \( O(n) \) Lists all edges. The edges on the outer boundary are given before the ones
-- on the holes. However, no other guarantees are given on the order.
listEdges    :: Polygon t p r -> [LineSegment 2 p r]
listEdges pg = let f = F.toList . outerBoundaryEdges
               in  f pg <> concatMap f (holeList pg)

-- | Pairs every vertex with its incident edges. The first one is its
-- predecessor edge, the second one its successor edge (in terms of
-- the ordering along the boundary).
--
--
-- >>> mapM_ print . polygonVertices $ withIncidentEdges simplePoly
-- Point2 0 0 :+ V2 (ClosedLineSegment (Point2 1 11 :+ ()) (Point2 0 0 :+ ())) (ClosedLineSegment (Point2 0 0 :+ ()) (Point2 10 0 :+ ()))
-- Point2 10 0 :+ V2 (ClosedLineSegment (Point2 0 0 :+ ()) (Point2 10 0 :+ ())) (ClosedLineSegment (Point2 10 0 :+ ()) (Point2 10 10 :+ ()))
-- Point2 10 10 :+ V2 (ClosedLineSegment (Point2 10 0 :+ ()) (Point2 10 10 :+ ())) (ClosedLineSegment (Point2 10 10 :+ ()) (Point2 5 15 :+ ()))
-- Point2 5 15 :+ V2 (ClosedLineSegment (Point2 10 10 :+ ()) (Point2 5 15 :+ ())) (ClosedLineSegment (Point2 5 15 :+ ()) (Point2 1 11 :+ ()))
-- Point2 1 11 :+ V2 (ClosedLineSegment (Point2 5 15 :+ ()) (Point2 1 11 :+ ())) (ClosedLineSegment (Point2 1 11 :+ ()) (Point2 0 0 :+ ()))
withIncidentEdges                    :: Polygon t p r
                                     -> Polygon t (Two (LineSegment 2 p r)) r
withIncidentEdges poly@SimplePolygon{} =
      unsafeFromCircularVector $ CV.zipWith3 f (CV.rotateLeft 1 vs) vs (CV.rotateRight 1 vs)
  where
    vs = poly ^. outerBoundaryVector
    f p c n = c&extra .~ Two (ClosedLineSegment p c) (ClosedLineSegment c n)
withIncidentEdges (MultiPolygon vs hs) = MultiPolygon vs' hs'
  where
    vs' = withIncidentEdges vs
    hs' = map withIncidentEdges hs

-- -- | Gets the i^th edge on the outer boundary of the polygon, that is the edge
---- with vertices i and i+1 with respect to the current focus. All indices
-- -- modulo n.
-- --

-- FIXME: Test that \poly -> fromEdges (toEdges poly) == poly
-- | Given the vertices of the polygon. Produce a list of edges. The edges are
-- half-open.
toEdges    :: CircularVector (Point 2 r :+ p) -> CircularVector (LineSegment 2 p r)
toEdges vs = CV.zipWith (\p q -> LineSegment (Closed p) (Open q)) vs (CV.rotateRight 1 vs)

-- | Compute the area of a polygon
area                        :: Fractional r => Polygon t p r -> r
area poly@SimplePolygon{} = abs $ signedArea poly
area (MultiPolygon vs hs) = area vs - sum [area h | h <- hs]


-- | Compute the signed area of a simple polygon. The the vertices are in
-- clockwise order, the signed area will be negative, if the verices are given
-- in counter clockwise order, the area will be positive.
signedArea      :: Fractional r => SimplePolygon p r -> r
signedArea poly = signedArea2X poly / 2

-- | Compute the signed area times 2 of a simple polygon. The the vertices are in
-- clockwise order, the signed area will be negative, if the verices are given
-- in counter clockwise order, the area will be positive.
signedArea2X      :: Num r => SimplePolygon p r -> r
signedArea2X poly = x
  where
    x = sum [ p^.core.xCoord * q^.core.yCoord - q^.core.xCoord * p^.core.yCoord
            | LineSegment' p q <- F.toList $ outerBoundaryEdges poly  ]

-- | Compute the centroid of a simple polygon.
centroid      :: Fractional r => SimplePolygon p r -> Point 2 r
centroid poly = Point $ sum' xs ^/ (6 * signedArea poly)
  where
    xs = [ (toVec p ^+^ toVec q) ^* (p^.xCoord * q^.yCoord - q^.xCoord * p^.yCoord)
         | LineSegment' (p :+ _) (q :+ _) <- F.toList $ outerBoundaryEdges poly  ]

    sum' = F.foldl' (^+^) zero


-- | \( O(n) \) Pick a  point that is inside the polygon.
--
-- (note: if the polygon is degenerate; i.e. has <3 vertices, we report a
-- vertex of the polygon instead.)
--
-- pre: the polygon is given in CCW order
pickPoint    :: (Ord r, Fractional r) => Polygon p t r -> Point 2 r
pickPoint pg | isTriangle pg = centroid $ pg^.outerBoundary
             | otherwise     = let LineSegment' (p :+ _) (q :+ _) = findDiagonal pg
                               in p .+^ (0.5 *^ (q .-. p))

-- | \( O(1) \) Test if the polygon is a triangle
isTriangle :: Polygon p t r -> Bool
isTriangle = \case
    p@SimplePolygon{}  -> F.length (p^.outerBoundaryVector) == 3
    MultiPolygon vs [] -> isTriangle vs
    MultiPolygon _  _  -> False

-- | \( O(n) \) Find a diagonal of the polygon.
--
-- pre: the polygon is given in CCW order
findDiagonal    :: (Ord r, Fractional r) => Polygon t p r -> LineSegment 2 p r
findDiagonal pg = List.head . catMaybes . F.toList $ diags
     -- note that a diagonal is guaranteed to exist, so the usage of head is safe.
  where
    vs      = pg^.outerBoundaryVector
    diags   = CV.zipWith3 f (CV.rotateLeft 1 vs) vs (CV.rotateRight 1 vs)
    f u v w = case ccw (u^.core) (v^.core) (w^.core) of
                CCW      -> Just $ findDiag u v w
                            -- v is a convex vertex, so find a diagonal
                            -- (either uw) or from v to a point inside the
                            -- triangle
                CW       -> Nothing -- v is a reflex vertex
                CoLinear -> Nothing -- colinear vertex!?

    -- we test if uw is a diagonal by figuring out if there is a vertex
    -- strictly inside the triangle t. If there is no such vertex then uw must
    -- be a diagonal (i.e. uw intersects the polygon boundary iff there is a
    -- vtx inside t).  If there are vertices inside the triangle, we find the
    -- one z furthest from the line(segment) uw. It then follows that vz is a
    -- diagonal. Indeed this is pretty much the argument used to prove that any
    -- polygon can be triangulated. See BKOS Chapter 3 for details.
    findDiag u v w = let t  = Triangle u v w
                         uw = ClosedLineSegment u w
                     in maybe uw (ClosedLineSegment v)
                      . safeMaximumOn (distTo $ supportingLine uw)
                      . filter (\(z :+ _) -> z `inTriangle` t == Inside)
                      . F.toList . polygonVertices
                      $ pg

    distTo l (z :+ _) = sqDistanceTo z l


safeMaximumOn   :: Ord b => (a -> b) -> [a] -> Maybe a
safeMaximumOn f = \case
  [] -> Nothing
  xs -> Just $ List.maximumBy (comparing f) xs


-- | \( O(n) \) Test if the outer boundary of the polygon is in clockwise or counter
-- clockwise order.
isCounterClockwise :: (Eq r, Num r) => Polygon t p r -> Bool
isCounterClockwise = (\x -> x == abs x) . signedArea2X . view outerBoundary


-- | \( O(n) \) Make sure that every edge has the polygon's interior on its
-- right, by orienting the outer boundary into clockwise order, and
-- the inner borders (i.e. any holes, if they exist) into
-- counter-clockwise order.
toClockwiseOrder   :: (Eq r, Num r) => Polygon t p r -> Polygon t p r
toClockwiseOrder p = toClockwiseOrder' p & polygonHoles'.traverse %~ toCounterClockWiseOrder'

-- | \( O(n) \) Orient the outer boundary into clockwise order. Leaves any holes
-- as they are.
--
toClockwiseOrder'   :: (Eq r, Num r) => Polygon t p r -> Polygon t p r
toClockwiseOrder' pg
      | isCounterClockwise pg = reverseOuterBoundary pg
      | otherwise             = pg

-- | \( O(n) \) Make sure that every edge has the polygon's interior on its left,
-- by orienting the outer boundary into counter-clockwise order, and
-- the inner borders (i.e. any holes, if they exist) into clockwise order.
toCounterClockWiseOrder   :: (Eq r, Num r) => Polygon t p r -> Polygon t p r
toCounterClockWiseOrder p =
  toCounterClockWiseOrder' p & polygonHoles'.traverse %~ toClockwiseOrder'

-- | \( O(n) \) Orient the outer boundary into counter-clockwise order. Leaves
-- any holes as they are.
toCounterClockWiseOrder'   :: (Eq r, Num r) => Polygon t p r -> Polygon t p r
toCounterClockWiseOrder' p
      | not $ isCounterClockwise p = reverseOuterBoundary p
      | otherwise                  = p

-- FIXME: Delete this function.
-- | Reorient the outer boundary from clockwise order to counter-clockwise order or
--   from counter-clockwise order to clockwise order. Leaves
--   any holes as they are.
--
reverseOuterBoundary   :: Polygon t p r -> Polygon t p r
reverseOuterBoundary p = p&unsafeOuterBoundaryVector %~ CV.reverse


-- | assigns unique integer numbers to all vertices. Numbers start from 0, and
-- are increasing along the outer boundary. The vertices of holes
-- will be numbered last, in the same order.
--
-- >>> numberVertices simplePoly
-- SimplePolygon [Point2 0 0 :+ SP 0 (),Point2 10 0 :+ SP 1 (),Point2 10 10 :+ SP 2 (),Point2 5 15 :+ SP 3 (),Point2 1 11 :+ SP 4 ()]
numberVertices :: Polygon t p r -> Polygon t (SP Int p) r
numberVertices = snd . bimapAccumL (\a p -> (a+1,SP a p)) (,) 0
  -- TODO: Make sure that this does not have the same issues as foldl vs foldl'

--------------------------------------------------------------------------------
-- Specialized folds

-- maximum and minimum probably aren't useful. Disabled for now. Lemmih, 2020-12-26.

-- | \( O(n) \) Yield the maximum point of the polygon. Points are compared first by x-coordinate
--   and then by y-coordinate. The maximum point will therefore be the right-most point in
--   the polygon (and top-most if multiple points share the largest x-coordinate).
--
--   Hole vertices are ignored since they cannot be the maximum.
_maximum :: Ord r => Polygon t p r -> Point 2 r :+ p
_maximum = F.maximumBy (comparing _core) . view outerBoundaryVector

-- | \( O(n) \) Yield the maximum point of a polygon according to the given comparison function.
maximumVertexBy :: (Point 2 r :+ p -> Point 2 r :+ p -> Ordering) -> Polygon t p r -> Point 2 r :+ p
maximumVertexBy fn (SimplePolygon vs)  = F.maximumBy fn vs
maximumVertexBy fn (MultiPolygon b hs) = F.maximumBy fn $ map (maximumVertexBy fn) (b:hs)

-- | \( O(n) \) Yield the maximum point of the polygon. Points are compared first by x-coordinate
--   and then by y-coordinate. The minimum point will therefore be the left-most point in
--   the polygon (and bottom-most if multiple points share the smallest x-coordinate).
--
--   Hole vertices are ignored since they cannot be the minimum.
_minimum :: Ord r => Polygon t p r -> Point 2 r :+ p
_minimum = F.minimumBy (comparing _core) . view outerBoundaryVector

-- | \( O(n) \) Yield the maximum point of a polygon according to the given comparison function.
minimumVertexBy :: (Point 2 r :+ p -> Point 2 r :+ p -> Ordering) -> Polygon t p r -> Point 2 r :+ p
minimumVertexBy fn (SimplePolygon vs)  = F.minimumBy fn vs
minimumVertexBy fn (MultiPolygon b hs) = F.minimumBy fn $ map (minimumVertexBy fn) (b:hs)

-- | Rotate to the first point that matches the given condition.
--
-- >>> toVector <$> findRotateTo (== (Point2 1 0 :+ ())) (unsafeFromPoints [Point2 0 0 :+ (), Point2 1 0 :+ (), Point2 1 1 :+ ()])
-- Just [Point2 1 0 :+ (),Point2 1 1 :+ (),Point2 0 0 :+ ()]
-- >>> findRotateTo (== (Point2 7 0 :+ ())) $ unsafeFromPoints [Point2 0 0 :+ (), Point2 1 0 :+ (), Point2 1 1 :+ ()]
-- Nothing
findRotateTo :: (Point 2 r :+ p -> Bool) -> SimplePolygon p r -> Maybe (SimplePolygon p r)
findRotateTo fn = fmap unsafeFromCircularVector . CV.findRotateTo fn . view outerBoundaryVector

--------------------------------------------------------------------------------
-- Rotation

-- | \( O(1) \) Rotate the polygon to the left by n number of points.
rotateLeft :: Int -> SimplePolygon p r -> SimplePolygon p r
rotateLeft n = over unsafeOuterBoundaryVector (CV.rotateLeft n)

-- | \( O(1) \) Rotate the polygon to the right by n number of points.
rotateRight :: Int -> SimplePolygon p r -> SimplePolygon p r
rotateRight n = over unsafeOuterBoundaryVector (CV.rotateRight n)

--------------------------------------------------------------------------------
-- Testing for reflex or convex

-- | Test if a given vertex is a reflex vertex.
--
-- \(O(1)\)
isReflexVertex      :: (Ord r, Num r) => Int -> Polygon Simple p r -> Bool
isReflexVertex i pg = ccw' u  v w == CW
  where
    u = pg^.outerVertex (i-1)
    v = pg^.outerVertex i
    w = pg^.outerVertex (i+1)

-- | Test if a given vertex is a convex vertex (i.e. not a reflex vertex).
--
-- \(O(1)\)
isConvexVertex   :: (Ord r, Num r) => Int -> Polygon Simple p r -> Bool
isConvexVertex i = not . isReflexVertex i

-- | Test if a given vertex is a strictly convex vertex.
--
-- \(O(1)\)
isStrictlyConvexVertex      :: (Ord r, Num r) => Int -> Polygon t p r -> Bool
isStrictlyConvexVertex i pg = ccw' u  v w == CCW
  where
    u = pg^.outerVertex (i-1)
    v = pg^.outerVertex i
    w = pg^.outerVertex (i+1)


-- | Computes all reflex vertices of the polygon.
--
-- \(O(n)\)
reflexVertices  :: (Ord r, Num r) => Polygon t p r -> [Int :+ (Point 2 r :+ p)]
reflexVertices p@(SimplePolygon _)                    = reflexVertices' p
reflexVertices (numberVertices -> MultiPolygon vs hs) =
  map (\(_ :+ (p :+ SP i e)) -> i :+ (p :+ e)) $
    reflexVertices' vs <> concatMap strictlyConvexVertices' hs

-- | Computes all convex (i.e. non-reflex) vertices of the polygon.
--
-- \(O(n)\)
convexVertices :: (Ord r, Num r) => Polygon t p r -> [Int :+ (Point 2 r :+ p)]
convexVertices = \case
  p@(SimplePolygon _)                    -> convexVertices' p
  (numberVertices -> MultiPolygon vs hs) ->
    map (\(_ :+ (p :+ SP i e)) -> i :+ (p :+ e)) $
      convexVertices' vs <> concatMap reflexVertices' hs

-- | Computes all strictly convex vertices of the polygon.
--
-- \(O(n)\)
strictlyConvexVertices :: (Ord r, Num r) => Polygon t p r -> [Int :+ (Point 2 r :+ p)]
strictlyConvexVertices = \case
  p@(SimplePolygon _)                    -> convexVertices' p
  (numberVertices -> MultiPolygon vs hs) ->
    map (\(_ :+ (p :+ SP i e)) -> i :+ (p :+ e)) $
      strictlyConvexVertices' vs <> concatMap reflexVertices' hs

----------------------------------------

-- | Return (the indices of) all reflex vertices, in increasing order
-- along the boundary.
--
-- \(O(n)\)
reflexVertices' :: (Ord r, Num r) => SimplePolygon p r -> [Int :+ (Point 2 r :+ p)]
reflexVertices' = filterReflexConvexWorker asReflex
  where
    asReflex u v w | ccw' (u^.extra) (v^.extra) (w^.extra) == CW = Just v
                   | otherwise                                   = Nothing

-- | Return (the indices of) all strictly convex vertices, in
-- increasing order along the boundary.
--
-- \(O(n)\)
strictlyConvexVertices' :: (Ord r, Num r) => SimplePolygon p r -> [Int :+ (Point 2 r :+ p)]
strictlyConvexVertices' = filterReflexConvexWorker asStrictlyConvex
  where
    asStrictlyConvex u v w | ccw' (u^.extra) (v^.extra) (w^.extra) == CCW = Just v
                           | otherwise                                    = Nothing

-- | Return (the indices of) all convex (= non-reflex) vertices, in increasing order
-- along the boundary.
--
-- \(O(n)\)
convexVertices' :: (Ord r, Num r) => SimplePolygon p r -> [Int :+ (Point 2 r :+ p)]
convexVertices' = filterReflexConvexWorker asConvex
  where
    asConvex u v w | ccw' (u^.extra) (v^.extra) (w^.extra) /= CW = Just v
                   | otherwise                                   = Nothing

-- | Helper function to implement convexVertices, reflexVertices, and
-- strictlyConvexVertices
filterReflexConvexWorker      :: (Ord r, Num r)
                              => (    Int :+ (Point 2 r :+ p)
                                   -> Int :+ (Point 2 r :+ p)
                                   -> Int :+ (Point 2 r :+ p)
                                   -> Maybe (Int :+ (Point 2 r :+ p))
                                 )
                              -> SimplePolygon p r -> [Int :+ (Point 2 r :+ p)]
filterReflexConvexWorker g pg =
    catMaybes $ zip3RWith g (CV.rotateLeft 1 vs) vs (CV.rotateRight 1 vs)
  where
    vs = CV.withIndicesRight $ pg^.outerBoundaryVector
    zip3RWith f us' vs' ws' = zipWith3 f (F.toList us') (F.toList vs') (F.toList ws')

{-# LANGUAGE ScopedTypeVariables  #-}
module Data.Geometry.Polygon where

import           Control.Lens hiding (Simple)
import           Data.Bifoldable
import           Data.Bifunctor
import           Data.Bitraversable
import qualified Data.CircularSeq as C
import           Data.Ext
import qualified Data.Foldable as F
import           Data.Geometry.Boundary
import           Data.Geometry.Box
import           Data.Geometry.Line
import           Data.Geometry.LineSegment
import           Data.Geometry.Point
import           Data.Geometry.Properties
import           Data.Geometry.Transformation
import           Data.Geometry.Vector
import qualified Data.List.NonEmpty as NonEmpty
import           Data.Maybe (mapMaybe)
import           Data.Proxy
import           Data.Semigroup
import           Data.Util
import           Frames.CoRec (asA)


--------------------------------------------------------------------------------
-- * Polygons

{- $setup
>>> :{
let simplePoly :: SimplePolygon () Rational
    simplePoly = SimplePolygon . C.fromList . map ext $ [ point2 0 0
                                                        , point2 10 0
                                                        , point2 10 10
                                                        , point2 5 15
                                                        , point2 1 11
                                                        ]
:} -}

-- | We distinguish between simple polygons (without holes) and Polygons with holes.
data PolygonType = Simple | Multi


data Polygon (t :: PolygonType) p r where
  SimplePolygon :: C.CSeq (Point 2 r :+ p)                         -> Polygon Simple p r
  MultiPolygon  :: C.CSeq (Point 2 r :+ p) -> [Polygon Simple p r] -> Polygon Multi  p r

instance Bifunctor (Polygon t) where
  bimap = bimapDefault

instance Bifoldable (Polygon t) where
  bifoldMap = bifoldMapDefault

instance Bitraversable (Polygon t) where
  bitraverse f g p = case p of
    SimplePolygon vs   -> SimplePolygon <$> bitraverseVertices f g vs
    MultiPolygon vs hs -> MultiPolygon  <$> bitraverseVertices f g vs
                                        <*> traverse (bitraverse f g) hs

bitraverseVertices     :: (Applicative f, Traversable t) => (p -> f q) -> (r -> f s)
                  -> t (Point 2 r :+ p) -> f (t (Point 2 s :+ q))
bitraverseVertices f g = traverse (bitraverse (traverse g) f)

type SimplePolygon = Polygon Simple

type MultiPolygon  = Polygon Multi

-- | Polygons are per definition 2 dimensional
type instance Dimension (Polygon t p r) = 2
type instance NumType   (Polygon t p r) = r

instance (Show p, Show r) => Show (Polygon t p r) where
  show (SimplePolygon vs)   = "SimplePolygon " <> show vs
  show (MultiPolygon vs hs) = "MultiPolygon " <> show vs <> " " <> show hs

instance (Eq p, Eq r) => Eq (Polygon t p r) where
  (SimplePolygon vs)   == (SimplePolygon vs')    = vs == vs'
  (MultiPolygon vs hs) == (MultiPolygon vs' hs') = vs == vs' && hs == hs'

instance PointFunctor (Polygon t p) where
  pmap f (SimplePolygon vs)   = SimplePolygon (fmap (first f) vs)
  pmap f (MultiPolygon vs hs) = MultiPolygon  (fmap (first f) vs) (map (pmap f) hs)

instance Num r => IsTransformable (Polygon t p r) where
  transformBy = transformPointFunctor

instance IsBoxable (Polygon t p r) where
  boundingBox = boundingBoxList' . toListOf (outerBoundary.traverse.core)

-- * Functions on Polygons

outerBoundary :: forall t p r. Lens' (Polygon t p r) (C.CSeq (Point 2 r :+ p))
outerBoundary = lens g s
  where
    g                     :: Polygon t p r -> C.CSeq (Point 2 r :+ p)
    g (SimplePolygon vs)  = vs
    g (MultiPolygon vs _) = vs

    s                           :: Polygon t p r -> C.CSeq (Point 2 r :+ p)
                                -> Polygon t p r
    s (SimplePolygon _)      vs = SimplePolygon vs
    s (MultiPolygon  _   hs) vs = MultiPolygon vs hs

holes :: forall p r. Lens' (Polygon Multi p r) [Polygon Simple p r]
holes = lens g s
  where
    g                     :: Polygon Multi p r -> [Polygon Simple p r]
    g (MultiPolygon _ hs) = hs
    s                     :: Polygon Multi p r -> [Polygon Simple p r]
                          -> Polygon Multi p r
    s (MultiPolygon vs _) = MultiPolygon vs


-- | Access the i^th vertex on the outer boundary
outerVertex   :: Int -> Lens' (Polygon t p r) (Point 2 r :+ p)
outerVertex i = outerBoundary.C.item i

-- running time: \(O(\log i)\)
outerBoundaryEdge     :: Int -> Polygon t p r -> LineSegment 2 p r
outerBoundaryEdge i p = let u = p^.outerVertex i
                            v = p^.outerVertex (i+1)
                        in LineSegment (Closed u) (Open v)


-- | Get all holes in a polygon
holeList                     :: Polygon t p r -> [Polygon Simple p r]
holeList (SimplePolygon _)   = []
holeList (MultiPolygon _ hs) = hs


-- | The vertices in the polygon. No guarantees are given on the order in which
-- they appear!
polygonVertices                      :: Polygon t p r
                                     -> NonEmpty.NonEmpty (Point 2 r :+ p)
polygonVertices (SimplePolygon vs)   = C.toNonEmpty vs
polygonVertices (MultiPolygon vs hs) =
  sconcat $ C.toNonEmpty vs NonEmpty.:| map polygonVertices hs



fromPoints :: [Point 2 r :+ p] -> SimplePolygon p r
fromPoints = SimplePolygon . C.fromList

-- | The edges along the outer boundary of the polygon. The edges are half open.
--
-- running time: \(O(n)\)
outerBoundaryEdges :: Polygon t p r -> C.CSeq (LineSegment 2 p r)
outerBoundaryEdges = toEdges . (^.outerBoundary)

-- | Lists all edges. The edges on the outer boundary are given before the ones
-- on the holes. However, no other guarantees are given on the order.
--
-- running time: \(O(n)\)
listEdges    :: Polygon t p r -> [LineSegment 2 p r]
listEdges pg = let f = F.toList . outerBoundaryEdges
               in  f pg <> concatMap f (holeList pg)

-- | Pairs every vertex with its incident edges. The first one is its
-- predecessor edge, the second one its successor edge.
--
-- >>> mapM_ print . polygonVertices $ withIncidentEdges simplePoly
-- Point2 [0 % 1,0 % 1] :+ SP LineSegment (Closed (Point2 [1 % 1,11 % 1] :+ ())) (Closed (Point2 [0 % 1,0 % 1] :+ ())) LineSegment (Closed (Point2 [0 % 1,0 % 1] :+ ())) (Closed (Point2 [10 % 1,0 % 1] :+ ()))
-- Point2 [10 % 1,0 % 1] :+ SP LineSegment (Closed (Point2 [0 % 1,0 % 1] :+ ())) (Closed (Point2 [10 % 1,0 % 1] :+ ())) LineSegment (Closed (Point2 [10 % 1,0 % 1] :+ ())) (Closed (Point2 [10 % 1,10 % 1] :+ ()))
-- Point2 [10 % 1,10 % 1] :+ SP LineSegment (Closed (Point2 [10 % 1,0 % 1] :+ ())) (Closed (Point2 [10 % 1,10 % 1] :+ ())) LineSegment (Closed (Point2 [10 % 1,10 % 1] :+ ())) (Closed (Point2 [5 % 1,15 % 1] :+ ()))
-- Point2 [5 % 1,15 % 1] :+ SP LineSegment (Closed (Point2 [10 % 1,10 % 1] :+ ())) (Closed (Point2 [5 % 1,15 % 1] :+ ())) LineSegment (Closed (Point2 [5 % 1,15 % 1] :+ ())) (Closed (Point2 [1 % 1,11 % 1] :+ ()))
-- Point2 [1 % 1,11 % 1] :+ SP LineSegment (Closed (Point2 [5 % 1,15 % 1] :+ ())) (Closed (Point2 [1 % 1,11 % 1] :+ ())) LineSegment (Closed (Point2 [1 % 1,11 % 1] :+ ())) (Closed (Point2 [0 % 1,0 % 1] :+ ()))
withIncidentEdges                    :: Polygon t p r
                                     -> Polygon t (Two (LineSegment 2 p r)) r
withIncidentEdges (SimplePolygon vs) =
      SimplePolygon $ C.zip3LWith f (C.rotateL vs) vs (C.rotateR vs)
  where
    f p c n = c&extra .~ SP (ClosedLineSegment p c) (ClosedLineSegment c n)
withIncidentEdges (MultiPolygon vs hs) = MultiPolygon vs' hs'
  where
    (SimplePolygon vs') = withIncidentEdges $ SimplePolygon vs
    hs' = map withIncidentEdges hs

-- -- | Gets the i^th edge on the outer boundary of the polygon, that is the edge
---- with vertices i and i+1 with respect to the current focus. All indices
-- -- modulo n.
-- --

-- | Given the vertices of the polygon. Produce a list of edges. The edges are
-- half-open.
toEdges    :: C.CSeq (Point 2 r :+ p) -> C.CSeq (LineSegment 2 p r)
toEdges vs = C.zipLWith (\p q -> LineSegment (Closed p) (Open q)) vs (C.rotateR vs)
  -- let vs' = F.toList vs in
  -- C.fromList $ zipWith (\p q -> LineSegment (Closed p) (Open q)) vs' (tail vs' ++ vs')


-- | Test if q lies on the boundary of the polygon. Running time: O(n)
--
-- >>> point2 1 1 `onBoundary` simplePoly
-- False
-- >>> point2 0 0 `onBoundary` simplePoly
-- True
-- >>> point2 10 0 `onBoundary` simplePoly
-- True
-- >>> point2 5 13 `onBoundary` simplePoly
-- False
-- >>> point2 5 10 `onBoundary` simplePoly
-- False
-- >>> point2 10 5 `onBoundary` simplePoly
-- True
-- >>> point2 20 5 `onBoundary` simplePoly
-- False
--
-- TODO: testcases multipolygon
onBoundary        :: (Fractional r, Ord r) => Point 2 r -> Polygon t p r -> Bool
q `onBoundary` pg = any (q `onSegment`) es
  where
    out = SimplePolygon $ pg^.outerBoundary
    es = concatMap (F.toList . outerBoundaryEdges) $ out : holeList pg

-- | Check if a point lies inside a polygon, on the boundary, or outside of the polygon.
-- Running time: O(n).
--
-- >>> point2 1 1 `inPolygon` simplePoly
-- Inside
-- >>> point2 0 0 `inPolygon` simplePoly
-- OnBoundary
-- >>> point2 10 0 `inPolygon` simplePoly
-- OnBoundary
-- >>> point2 5 13 `inPolygon` simplePoly
-- Inside
-- >>> point2 5 10 `inPolygon` simplePoly
-- Inside
-- >>> point2 10 5 `inPolygon` simplePoly
-- OnBoundary
-- >>> point2 20 5 `inPolygon` simplePoly
-- Outside
--
-- TODO: Add some testcases with multiPolygons
-- TODO: Add some more onBoundary testcases
inPolygon                                :: forall t p r. (Fractional r, Ord r)
                                         => Point 2 r -> Polygon t p r
                                         -> PointLocationResult
q `inPolygon` pg
    | q `onBoundary` pg                             = OnBoundary
    | odd kl && odd kr && not (any (q `inHole`) hs) = Inside
    | otherwise                                     = Outside
  where
    l = horizontalLine $ q^.yCoord

    -- Given a line segment, compute the intersection point (if a point) with the
    -- line l
    intersectionPoint = asA (Proxy :: Proxy (Point 2 r)) . (`intersect` l)

    -- Count the number of intersections that the horizontal line through q
    -- maxes with the polygon, that are strictly to the left and strictly to
    -- the right of q. If these numbers are both odd the point lies within the polygon.
    --
    --
    -- note that: - by the asA (Point 2 r) we ignore horizontal segments (as desired)
    --            - by the filtering, we effectively limit l to an open-half line, starting
    --               at the (open) point q.
    --            - by using half-open segments as edges we avoid double counting
    --               intersections that coincide with vertices.
    --            - If the point is outside, and on the same height as the
    --              minimum or maximum coordinate of the polygon. The number of
    --              intersections to the left or right may be one. Thus
    --              incorrectly classifying the point as inside. To avoid this,
    --              we count both the points to the left *and* to the right of
    --              p. Only if both are odd the point is inside.  so that if
    --              the point is outside, and on the same y-coordinate as one
    --              of the extermal vertices (one ofth)
    --
    -- See http://geomalgorithms.com/a03-_inclusion.html for more information.
    SP kl kr = count (\p -> (p^.xCoord) `compare` (q^.xCoord))
             . mapMaybe intersectionPoint . F.toList . outerBoundaryEdges $ pg

    -- For multi polygons we have to test if we do not lie in a hole .
    inHole = insidePolygon
    hs     = holeList pg

    count   :: (a -> Ordering) -> [a] -> SP Int Int
    count f = foldr (\x (SP lts gts) -> case f x of
                             LT -> SP (lts + 1) gts
                             EQ -> SP lts       gts
                             GT -> SP lts       (gts + 1)) (SP 0 0)


-- | Test if a point lies strictly inside the polgyon.
insidePolygon        :: (Fractional r, Ord r) => Point 2 r -> Polygon t p r -> Bool
q `insidePolygon` pg = q `inPolygon` pg == Inside


-- testQ = map (`inPolygon` testPoly) [ point2 1 1    -- Inside
--                                    , point2 0 0    -- OnBoundary
--                                    , point2 5 14   -- Inside
--                                    , point2 5 10   -- Inside
--                                    , point2 10 5   -- OnBoundary
--                                    , point2 20 5   -- Outside
--                                    ]

-- testPoly :: SimplePolygon () Rational
-- testPoly = SimplePolygon . C.fromList . map ext $ [ point2 0 0
--                                                   , point2 10 0
--                                                   , point2 10 10
--                                                   , point2 5 15
--                                                   , point2 1 11
--                                                   ]

-- | Compute the area of a polygon
area                        :: Fractional r => Polygon t p r -> r
area poly@(SimplePolygon _) = abs $ signedArea poly
area (MultiPolygon vs hs)   = area (SimplePolygon vs) - sum [area h | h <- hs]


-- | Compute the signed area of a simple polygon. The the vertices are in
-- clockwise order, the signed area will be negative, if the verices are given
-- in counter clockwise order, the area will be positive.
signedArea      :: Fractional r => SimplePolygon p r -> r
signedArea poly = x / 2
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


-- | Test if the outer boundary of the polygon is in clockwise or counter
-- clockwise order.
--
-- running time: \(O(n)\)
--
isCounterClockwise :: (Eq r, Fractional r) => Polygon t p r -> Bool
isCounterClockwise = (\x -> x == abs x) . signedArea
                   . fromPoints . F.toList . (^.outerBoundary)


-- | Orient the outer boundary to clockwise order
toClockwiseOrder         :: (Eq r, Fractional r) => Polygon t p r -> Polygon t p r
toClockwiseOrder p
  | isCounterClockwise p = p&outerBoundary %~ C.reverseDirection
  | otherwise            = p

-- | Orient the outer boundary to counter clockwise order
toCounterClockWiseOrder    :: (Eq r, Fractional r) => Polygon t p r -> Polygon t p r
toCounterClockWiseOrder p
  | not $ isCounterClockwise p = p&outerBoundary %~ C.reverseDirection
  | otherwise                  = p

-- | Convert a Polygon to a simple polygon by forgetting about any holes.
asSimplePolygon                        :: Polygon t p r -> SimplePolygon p r
asSimplePolygon poly@(SimplePolygon _) = poly
asSimplePolygon (MultiPolygon vs _)    = SimplePolygon vs


-- | Comparison that compares which point is 'larger' in the direction given by
-- the vector u.
cmpExtreme       :: (Num r, Ord r)
                 => Vector 2 r -> Point 2 r :+ p -> Point 2 r :+ q -> Ordering
cmpExtreme u p q = u `dot` (p^.core .-. q^.core) `compare` 0


-- | Finds the extreme points, minimum and maximum, in a given direction
--
-- running time: \(O(n)\)
extremesLinear     :: (Ord r, Num r) => Vector 2 r -> Polygon t p r
                   -> (Point 2 r :+ p, Point 2 r :+ p)
extremesLinear u p = let vs = p^.outerBoundary
                         f  = cmpExtreme u
                     in (F.minimumBy f vs, F.maximumBy f vs)


-- | assigns unique integer numbers to all vertices. Numbers start from 0, and
-- are increasing along the outer boundary. The vertices of holes
-- will be numbered last, in the same order.
--
-- >>> numberVertices simplePoly
-- SimplePolygon CSeq [Point2 [0 % 1,0 % 1] :+ SP 0 (),Point2 [10 % 1,0 % 1] :+ SP 1 (),Point2 [10 % 1,10 % 1] :+ SP 2 (),Point2 [5 % 1,15 % 1] :+ SP 3 (),Point2 [1 % 1,11 % 1] :+ SP 4 ()]
numberVertices :: Polygon t p r -> Polygon t (SP Int p) r
numberVertices = snd . bimapAccumL (\a p -> (a+1,SP a p)) (\a r -> (a,r)) 0
  -- TODO: Make sure that this does not have the same issues as foldl vs foldl'

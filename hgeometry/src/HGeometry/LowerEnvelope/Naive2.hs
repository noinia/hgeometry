module HGeometry.LowerEnvelope.Naive2
  ( Plane

  , LowerEnvelope
  , lowerEnvelope

  , Vertex'(..), Vertex, location, location2
  , GHalfEdge(HalfEdge), HalfEdge, origin, destination
  , asHalfEdge
  ) where

import           Control.Lens
import qualified Data.Foldable as F
import           Data.Function (on)
import           Data.Kind (Type)
import qualified Data.Map as Map
import qualified Data.Set as Set
--import qualified Data.Vector as Boxed
import           HGeometry.Combinatorial.Util
import           HGeometry.Ext
--import           HGeometry.Foldable.Sort
import           HGeometry.HyperPlane.Class
import           HGeometry.HyperPlane.NonVertical
import           HGeometry.LineSegment
import           HGeometry.Point hiding (origin)
import           Witherable

--------------------------------------------------------------------------------

-- | Shorthand for non-vertical hyperplanes in R^3
type Plane = NonVerticalHyperPlane 3

-- | A vertex in the lower envelope
data BoundedVertex r = BoundedVertex { _location :: Point 3 r
                                     , _definers :: Set.Set (Plane r)
                                     } deriving (Show,Read,Eq)

data Vertex' vertex = UnboundedVertex
                    | Vertex vertex
                    deriving (Show,Read,Eq,Functor)
type Vertex r = Vertex' (BoundedVertex r)

-- | The location of the vertex
location :: Lens' (BoundedVertex r) (Point 3 r)
location = lens _location (\v l -> v { _location = l })

-- | Projected 2d location of the point
location2 :: Getter (BoundedVertex r) (Point 2 r)
location2 = location . to projectPoint

--------------------------------------------------------------------------------

data GHalfEdge vertex r = HalfEdge { _origin      :: !(Vertex' vertex)
                                   , _destination :: !(Vertex' vertex)
                                   , _leftPlane   :: !(Plane r)
                                   } deriving (Show,Read,Eq)

-- | A Half-edge in the envelope
type HalfEdge r = GHalfEdge (BoundedVertex r) r

origin :: Lens' (GHalfEdge vertex r) (Vertex' vertex)
origin = lens _origin (\he o -> he { _origin = o })

destination :: Lens' (GHalfEdge vertex r) (Vertex' vertex)
destination = lens _destination (\he d -> he { _destination = d })

--------------------------------------------------------------------------------

-- | Intermediate type to use during computation
type LowerEnvelope :: (Type -> Type) -> Type -> Type
data LowerEnvelope g r =
  LowerEnvelope { _boundedVertices :: Map.Map (Point 3 r) (Set.Set (Plane r))
                , _halfEdges       :: g (HalfEdge r)
                -- ^ sorted CCW around their origins
                }


-- | Brute force implementation of the lower envelope.
--
-- pre: we are given at least 3 planes that define at least one vertex
--
-- running time: \(O(n^3)\)
lowerEnvelope    :: (Traversable f, Ord r, Fractional r)
                 => f (Plane r) -> LowerEnvelope [] r
lowerEnvelope hs = LowerEnvelope vertices' halfEdges''
  where
    -- FIXME: This does not report the half-edges in any particular order (yet)

    -- we first compute all half edges. Each half edge still has a somewhat course
    -- idea of what its vertices are though; in particular. These vertices don't know
    -- the full set of definers yet (in particular in case of degeneracies)
    --
    -- note that we generate both (h1,h2) and (h2,h1) since we want each halfedge in
    -- both directions.
    --
    -- note: this is the dominating step in the running time, taking O(n^2*n) = O(n^3 )time
    --
    -- TODO: it is probably slightly faster to generate unique pairs and simply flip
    -- the edges.
    halfEdges' = catMaybes [ asHalfEdge h1 h2 hs
                           | h1 <- F.toList hs, h2 <- F.toList hs
                           ]
    -- once we have the vertices we can annotate every bounded vertex with its complete
    -- set of definers
    halfEdges'' = halfEdges' <&> \(HalfEdge o d h) -> HalfEdge (vtx <$> o) (vtx <$> d) h

    -- we collect the vertex locations, and the corresponding definers by going over all
    -- half edges (and collecting the bounded vertices.)
    vertices' = foldr (\(v :+ defs) -> Map.insertWith (<>) v defs) mempty
              $ foldMap boundedVertices halfEdges'

    -- We use a lookup to find the set of definers. Note that this is safe, since we
    -- constructed the set of vertices from the set of half-edges just before.

    -- vtx          :: Point 3 r :+ a -> BoundedVertex r
    vtx (p :+ _) = BoundedVertex p (vertices' Map.! p)


-- | Collect all bounded vertices of a  halfedge
boundedVertices    :: GHalfEdge vertex r -> [vertex]
boundedVertices he = mapMaybe (\case
                                  UnboundedVertex -> Nothing
                                  Vertex v        -> Just v
                              ) [ he^.origin, he^.destination ]




-- deriving instance (Show r, Show1 g) => Show (LowerEnvelope g r)
-- deriving instance (Read r, Read1 g) => Read (LowerEnvelope g r)
-- deriving instance (Ord r, Eq1 g)    => Eq   (LowerEnvelope g r)

-- data Interval r = UpTo r
--                 | From r
--                 | Interval r r
--                 deriving (Show,Eq)

--------------------------------------------------------------------------------

-- | A line in R^2, specified by its equation
data MyLine r = VerticalLine !r
              | ALine !r -- a       y = ax+ b
                      !r -- b
              deriving (Show,Eq)

-- | Given two planes h1 and h2, compute the line l in which they intersect (if it exists)
intersectingLine  :: (Eq r, Fractional r) => Plane r -> Plane r -> Maybe (MyLine r)
intersectingLine (Plane a1 b1 c1) (Plane a2 b2 c2)
  | b1 == b2  = if a1 == a2 then Nothing -- h1 and h2 are horizontal planes; they don't intersect
                            else Just . VerticalLine $ (c2 - c1) / (a1 - a2)
  | otherwise = let lA = (a2 - a1) / (b1 - b2)
                    lB = (c2 - c1) / (b1 - b2)
                in Just $ ALine lA lB

-- | Side of a halfline
data Direction = Lower | Upper deriving (Show,Eq,Ord)

-- | A half-line with respect to some (not specified line)
data RelativeHalfLine r = RelHalfLine { hlDirection :: {-# UNPACK #-}!Direction
                                      , hlEndPoint  :: !(Point 2 r)
                                      , hlDefiner   :: (Plane r)
                                        -- the plane defining this subline
                                      }
                        deriving (Show,Eq)


data SubLine r = HalfLine !(RelativeHalfLine r)
               | EntireLine                     (Plane r) -- the plane defining this subline
               deriving (Show,Eq)
-- fixme: We are currently not really using the plane passed to entireline.


data SubLine' r = UnboundedSubLine !(RelativeHalfLine r)
                | Segment !(ClosedLineSegment (Point 2 r :+ Plane r))
                deriving (Show,Eq)

-- | given two planes h1 and h2, let l be the the line in which h1 and h2 intersect.
--
-- We compute the (downward projection of) the part of l where l lies
-- below h3.
--
asLowerInterval          :: (Ord r, Fractional r)
                         => Plane r -> Plane r -> Plane r -> Maybe (SubLine r)
asLowerInterval h1 h2 h3 = intersectingLine h1 h2 >>= \l -> asLowerInterval' h1 h2 l h3

-- | Implementation of asLowerInterval in which we are also given the line l = h1 \intersect h2
asLowerInterval' :: (Ord r, Fractional r)
                 => Plane r -> Plane r  -> MyLine r -- ^ line l
                 -> Plane r

                 -> Maybe (SubLine r)
asLowerInterval' h1@(Plane a1 b1 c1) _ l
                 h3@(Plane a3 b3 c3) = case l of
  VerticalLine x -> case b3 `compare` b1 of
                      EQ | belowAt (Point2 x 0) h1 h3 -> Just $ EntireLine h3
                         | otherwise                  -> Nothing
                      LT -> verticalHalfLine x Lower -- sign flips
                      GT -> verticalHalfLine x Upper
      -- if l is a vertical line through x we have to solve: h3(x,y) >= h1(x,y) for y.
      -- we essentially have (b3-b1)*y >= (a1-a3)*x + c1 -c3, and thus
      -- y>= (a1-a3)*x + c1 -c3
      --     ------------------
      --     b3 - b1
      --
      -- we basically test the sign of b3-b1; if it is 0 the lines don't intersect and so
      -- either h1 is everywhere lower than h3. If the sign is negative the >= flips to <=,
      -- and thus the side of the half-line we actually care about flips.
  ALine lA lB    -> let num   = (b1-b3)*lB + c1 - c3
                        denom = a3-a1+(b3-b1)*lA
                        x     = num / denom
                    in
                    case denom `compare` 0 of
                      EQ | belowAt (Point2 0 lB) h1 h3 -> Just $ EntireLine h3
                         | otherwise                   -> Nothing
                      LT -> halfLine lA lB x Lower -- sign flips
                      GT -> halfLine lA lB x Upper
        -- when l is not vertical (and thus of the form y=Ax+B) we again try to solve
        -- h3(x, Ax + B) >= h1(x, Ax + B)
        --
        -- we now get that denom * x >= num
        -- and thus we again compute the sign of denum; if it is zero, we again test which plane
        -- is lowest everywhere (i.e. at any point on the line).
        -- if the sign is negative the >= again flips to <=, and thus the side of the
        -- halfline flips.
  where
    verticalHalfLine x dir = let y = (a1 - a3)*x + c1 - c3
                             in Just $ HalfLine $ RelHalfLine dir (Point2 x y) h3

    halfLine lA lB x dir = Just $ HalfLine $ RelHalfLine dir (Point2 x (lA*x + lB)) h3

    belowAt p hA hB = evalAt p hA <= evalAt p hB

-- | Given planes h1 and h2, let l be the line in which they
-- intersect, compute the interval along l where h1,h2 form an edge of
-- the lower envelope with h1 left of the edge.
--
-- O(n)
asEdgeInterval          :: (Traversable f, Ord r, Fractional r)
                        => Plane r -> Plane r -> f (Plane r) -> Maybe (SubLine' r)
asEdgeInterval h1 h2 hs = do l        <- intersectingLine h1 h2
                             subLines <- traverse (asLowerInterval' h1 h2 l) hs
                             commonIntersection l subLines

-- | Given a Line, and a bunch of sublines, computes the common
-- intersection of those sublines.
--
-- O(n)
commonIntersection            :: (Foldable f, Ord r)
                              => MyLine r -> f (SubLine r) -> Maybe (SubLine' r)
commonIntersection l subLines = case commonIntersection' l subLines of
  Two Nothing  Nothing  -> Nothing -- FIXME: I guess if all are EntireLine's this would be wrong
  Two (Just p) Nothing  -> Just $ UnboundedSubLine p
  Two Nothing  (Just q) -> Just $ UnboundedSubLine q
  Two (Just p) (Just q)
    | cmpAlong l p q == LT -> Nothing
    | otherwise            -> Just $ Segment $
                              ClosedLineSegment (hlEndPoint q :+ hlDefiner q)
                                                (hlEndPoint p :+ hlDefiner p)



-- | compare two points along the given line.
cmpAlong   :: Ord r => MyLine a -> RelativeHalfLine r -> RelativeHalfLine r -> Ordering
cmpAlong l = case l of
               VerticalLine _ -> compare `on` (^.to hlEndPoint.yCoord)
               ALine _ _      -> compare `on` (^.to hlEndPoint.xCoord)

-- compute the intersection of all lower halflines and all upper halfines
-- returns a (strict) pair (l,h) where l is the endpoint of the intersection of all lower halfines
-- and b is the endpoint of the intersection of all upper halflines.
commonIntersection'   :: (Foldable f, Ord r)
                      => MyLine r -> f (SubLine r) -> Two (Maybe (RelativeHalfLine r))
commonIntersection' l = foldr (flip f) (Two Nothing Nothing)
  where
    f acc@(Two low high) = \case
      EntireLine _  -> acc
      HalfLine hl   -> case hlDirection hl of
                         Lower -> Two (minBy (cmpAlong l) low hl) high
                         Upper -> Two low (maxBy (cmpAlong l) high hl)

    minBy cmp mp q = Just $ case mp of
                       Nothing -> q
                       Just p  -> if p `cmp` q /= GT then p else q
    maxBy cmp mp q = Just $ case mp of
                       Nothing -> q
                       Just p  -> if p `cmp` q /= LT then p else q

-- | Given planes h1 and h2, let l be the line in which they
-- intersect, compute the interval along l where h1,h2 form an edge of
-- the lower envelope with h1 left of the edge.
--
-- O(n)
asHalfEdge          :: (Ord r, Fractional r, Traversable f)
                    => Plane r -> Plane r -> f (Plane r)
                    -> Maybe (GHalfEdge (Point 3 r :+ Set.Set (Plane r)) r)
asHalfEdge h1 h2 hs = asEdgeInterval h1 h2 hs <&> \case
                        UnboundedSubLine sl -> let q = hlEndPoint sl
                                                   h3 = hlDefiner sl
                                               in HalfEdge (mkVertex q h3) UnboundedVertex h1
                        Segment seg         -> let (p :+ h3) = seg^.start
                                                   (q :+ h4) = seg^.end
                                               in HalfEdge (mkVertex p h3) (mkVertex q h4) h1
  where
    mkVertex p@(Point2 x y) h3 = let z    = evalAt p h1
                                     defs = Set.fromList [h1,h2,h3]
                                 in Vertex (Point3 x y z :+ defs)
-- FIXME: not sure the h1 is guaranteed to be correct; pick either h1 or h2, depending who is left of the oriented line l.

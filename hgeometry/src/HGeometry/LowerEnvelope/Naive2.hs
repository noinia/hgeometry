module HGeometry.LowerEnvelope.Naive2
  ( Plane

  , LowerEnvelope, _boundedVertices, _halfEdges
  , lowerEnvelope

  , BoundedVertex(BoundedVertex), location, location2
  , belowAll
  , intersectionPoint, asBoundedVertex

  , Vertex'(..), Vertex
  , GHalfEdge(HalfEdge), HalfEdge, origin, destination
  , asHalfEdge

  , asLowerInterval, SubLine(..), hlEndPoint
  ) where

import           Control.Lens
import qualified Data.Foldable as F
import           Data.Function (on)
import           Data.Functor.Classes
import           Data.Kind (Type)
import qualified Data.Map as Map
import qualified Data.Set as Set
import           HGeometry.Combinatorial.Util
import           HGeometry.Ext
--import           HGeometry.Foldable.Sort
import           HGeometry.HyperPlane.Class
import           HGeometry.HyperPlane.NonVertical
import           HGeometry.Point hiding (origin)
import           Witherable

--------------------------------------------------------------------------------

-- | Shorthand for non-vertical hyperplanes in R^3
type Plane = NonVerticalHyperPlane 3

-- | A vertex in the lower envelope
data BoundedVertex r = BoundedVertex (Point 3 r)         -- ^ the location
                                     (Set.Set (Plane r)) -- ^ the definers
                     deriving (Show,Read,Eq)

data Vertex' vertex = UnboundedVertex
                    | Vertex vertex
                    deriving (Show,Read,Eq,Functor)
type Vertex r = Vertex' (BoundedVertex r)

-- | The location of the vertex
location :: Lens' (BoundedVertex r) (Point 3 r)
location = lens (\(BoundedVertex p _) -> p) (\(BoundedVertex _ defs) p -> BoundedVertex p defs)

-- | Projected 2d location of the point
location2 :: Getter (BoundedVertex r) (Point 2 r)
location2 = location . to projectPoint

--------------------------------------------------------------------------------

-- | Test if the given vertex lies on or below all the given planes
--
-- \(O(n)\)
belowAll   :: (Foldable f, Num r, Ord r) => BoundedVertex r -> f (Plane r) -> Bool
belowAll v = all ((v^.location) `notAbove`)

-- | Returns True when the point is not above the plane, in otherwords
-- when the plane actually passes above (or through) the point.
notAbove                  :: (Ord r, Num r) => Point 3 r -> Plane r -> Bool
notAbove (Point3 x y z) h = not $ z > evalAt (Point2 x y) h

-- | Construct the vertex representing the intersection of the three
-- given planes (if such an intersection point exists).
asBoundedVertex                  :: (Fractional r, Ord r)
                                 => Three (Plane r) -> Maybe (BoundedVertex r)
asBoundedVertex (Three h1 h2 h3) = (\l -> BoundedVertex l (Set.fromList [h1, h2, h3]))
                                <$> intersectionPoint h1 h2 h3

-- | Compute the common intersection point of three non-vertical
-- hyperplanes in R^3 (if such a point exists).
intersectionPoint          :: (Fractional r, Ord r)
                           => Plane r -> Plane r -> Plane r -> Maybe (Point 3 r)
intersectionPoint h1 h2 h3 = asLowerInterval h1 h2 h3 >>= \case
  EntireLine  -> Nothing
  HalfLine hl -> let q@(Point2 x y) = hlEndPoint hl
                     z              = evalAt q h1
                 in Just $ Point3 x y z

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

-- | Given the left plane, and a halfegdge create the corresponding
-- halfedge i.e. by flipping origin and destination.
twinWith                    :: Plane r -> GHalfEdge vertex r -> GHalfEdge vertex r
twinWith h (HalfEdge o d _) = HalfEdge d o h

--------------------------------------------------------------------------------

-- | Intermediate type to use during computation
type LowerEnvelope :: (Type -> Type) -> Type -> Type
data LowerEnvelope g r =
  LowerEnvelope { _boundedVertices :: Map.Map (Point 3 r) (Set.Set (Plane r))
                , _halfEdges       :: g (HalfEdge r)
                -- ^ sorted CCW around their origins
                }



deriving instance (Show r, Show1 g)        => Show (LowerEnvelope g r)
deriving instance (Read r, Ord r, Read1 g) => Read (LowerEnvelope g r)
deriving instance (Ord r, Eq1 g)           => Eq   (LowerEnvelope g r)

--------------------------------------------------------------------------------

-- | Somewhat brute force implementation of the lower envelope.
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
    halfEdges' = foldMap (\(Two h1 h2) -> case asHalfEdge h1 h2 hs of
                              Nothing -> mempty
                              Just hl -> [hl, twinWith (other (_leftPlane hl) h1 h2) hl]
                         )
               $ uniquePairs hs

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
               | EntireLine
               deriving (Show,Eq)



data SubLine' r = UnboundedSubLine !(RelativeHalfLine r) !(MyLine r) -- the line

                | Segment !(Point 2 r :+ Plane r) -- startpoint
                          !(Point 2 r :+ Plane r) -- endpoint
                          !(MyLine r) -- the line
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
                      EQ | belowAt (Point2 x 0) h1 h3 -> Just EntireLine
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
                      EQ | belowAt (Point2 0 lB) h1 h3 -> Just EntireLine
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
  Two (Just p) Nothing  -> Just $ UnboundedSubLine p l
  Two Nothing  (Just q) -> Just $ UnboundedSubLine q l
  Two (Just p) (Just q)
    | cmpAlong l p q == LT -> Nothing
    | otherwise            -> Just $ Segment (hlEndPoint q :+ hlDefiner q)
                                             (hlEndPoint p :+ hlDefiner p)
                                             l



-- | compare two points along the given line.
cmpAlong   :: Ord r => MyLine a -> RelativeHalfLine r -> RelativeHalfLine r -> Ordering
cmpAlong l = case l of
               VerticalLine _ -> compare `on` (^.to hlEndPoint.yCoord)
               ALine _ _      -> compare `on` (^.to hlEndPoint.xCoord)

-- | compute the intersection of all lower halflines and all upper
-- halfines returns a (strict) pair (l,h) where l is the endpoint of
-- the intersection of all lower halfines and b is the endpoint of the
-- intersection of all upper halflines.
commonIntersection'   :: (Foldable f, Ord r)
                      => MyLine r -> f (SubLine r) -> Two (Maybe (RelativeHalfLine r))
commonIntersection' l = foldr (flip f) (Two Nothing Nothing)
  where
    f acc@(Two low high) = \case
      EntireLine    -> acc
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
                        UnboundedSubLine sl l         ->
                            let q  = hlEndPoint sl
                                u  = mkVertex q (hlDefiner sl)
                                h  = planeAbove l
                            in HalfEdge u UnboundedVertex h
                        Segment (p :+ h3) (q :+ h4) l ->
                            HalfEdge (mkVertex p h3) (mkVertex q h4) (planeAbove l)
  where
    mkVertex p@(Point2 x y) h3 = let z    = evalAt p h1
                                     defs = Set.fromList [h1,h2,h3]
                                 in Vertex (Point3 x y z :+ defs)

    -- figure out which plane is the lower plane just above l, simply by evaluating
    -- h1 and h2 at some point in the left halfspace of l
    --
    -- we interpret vertical lines as upward, this is consistent with cmpAlong
    planeAbove = minAt . \case
      VerticalLine x -> Point2 (x-1) 0
      ALine _ b      -> Point2 0     (b+1)

    minAt q = if evalAt q h1 <= evalAt q h2 then h1 else h2


--------------------------------------------------------------------------------

-- | given a b c, returns the value from b or c that is not a.
other                     :: Eq p => p -> p -> p -> p
other h h1 h2 | h == h1   = h2
              | otherwise = h1

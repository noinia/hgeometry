module HGeometry.LowerEnvelope.Naive2
  ( Plane

  ) where

import           Control.Lens
import           Data.Function (on)
import           Data.Functor.Classes
import           Data.Kind (Type)
import qualified Data.Map as Map
import           Data.Ord (comparing)
import qualified Data.Set as Set
import qualified Data.Vector as Boxed
import           HGeometry.Combinatorial.Util
import           HGeometry.Foldable.Sort
import           HGeometry.HyperPlane.Class
import           HGeometry.HyperPlane.NonVertical
import           HGeometry.LineSegment
import           HGeometry.Point hiding (origin)
import           HGeometry.Properties
import           HGeometry.Unbounded
import           HGeometry.Vector
import qualified Hiraffe.Graph as Graph

--------------------------------------------------------------------------------

-- | Shorthand for non-vertical hyperplanes in R^3
type Plane = NonVerticalHyperPlane 3

-- | A vertex in the lower envelope
data BoundedVertex r = BoundedVertex { _location :: Point 3 r
                                     , _definers :: Set.Set (Plane r)
                                     } deriving (Show,Read,Eq)

data Vertex r = UnboundedVertex
              | Vertex (BoundedVertex r)
              deriving (Show,Read,Eq)

-- | A Half-edge in the envelope
data HalfEdge r = HalfEdge { _origin      :: !(Vertex r)
                           , _destination :: !(Vertex r)
                           , _leftPlane   :: !(Plane r)
                           } deriving (Show,Read,Eq)

-- | Intermediate type to use during computation
data LowerEnvelope g r =
  LowerEnvelope { _boundedVertices :: Map.Map (Point 3 r) (Set.Set (Plane r))
                , _halfEdges       :: g (HalfEdge r)
                -- ^ sorted CCW around their origins
                }

-- deriving instance (Show r, Show1 g) => Show (LowerEnvelope g r)
-- deriving instance (Read r, Read1 g) => Read (LowerEnvelope g r)
-- deriving instance (Ord r, Eq1 g)    => Eq   (LowerEnvelope g r)

-- data Interval r = UpTo r
--                 | From r
--                 | Interval r r
--                 deriving (Show,Eq)

-- intersect                   :: Ord r => Interval r -> Interval r -> Maybe (Interval r)
-- intersect (UpTo l) (UpTo r) = Just  $UpTo (min l r)
-- intersect (UpTo l) (From r)
--   | r <= l                  = Just $ Interval r l
--   | otherwise               = Nothing
-- intersect (UpTo l) (Interval rl rr)
--   | rl

--   UpTo (min l r)


-- type Subline r = ClosedLineSegment (Point 2 (Unbounded r))

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

data Direction = Lower | Upper deriving (Show,Eq,Ord)

data SubLine r = HalfLine   !(MyLine r) {-# UNPACK #-}!Direction !(Point 2 r)
               | EntireLine !(MyLine r)
               deriving (Show,Eq)


data SubLine' r = UnboundedSubLine !(SubLine r)
                | Segment !(ClosedLineSegment (Point 2 r))
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
                      EQ | belowAt (Point2 x 0) h1 h3 -> Just $ EntireLine l
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
                      EQ | belowAt (Point2 0 lB) h1 h3 -> Just $ EntireLine l
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
                               in Just $ HalfLine l dir $ (Point2 x y)

    halfLine lA lB x dir = Just $ HalfLine l dir (Point2 x (lA*x + lB))

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

commonIntersection            :: (Foldable f, Ord r)
                              => MyLine r -> f (SubLine r) -> Maybe (SubLine' r)
commonIntersection l subLines = case commonIntersection' subLines of
  Two Nothing  Nothing  -> Nothing
  Two (Just p) Nothing  -> Just $ UnboundedSubLine $ HalfLine l Lower p
  Two Nothing  (Just q) -> Just $ UnboundedSubLine $ HalfLine l Upper q
  Two (Just p) (Just q)
    | cmpAlong l p q == LT -> Nothing
    | otherwise            -> Just $ Segment $ ClosedLineSegment q p

-- | compare two points along the given line.
cmpAlong   :: Ord r => MyLine a -> Point 2 r -> Point 2 r -> Ordering
cmpAlong l = case l of
               VerticalLine _ -> compare `on` (^.yCoord)
               ALine _ _      -> compare `on` (^.xCoord)


-- compute the intersection of all lower halflines and all upper halfines
-- returns a (strict) pair (l,h) where l is the endpoint of the intersection of all lower halfines
-- and b is the endpoint of the intersection of all upper halflines.
commonIntersection' :: (Foldable f, Ord r)
                    => f (SubLine r) -> Two (Maybe (Point 2 r))
commonIntersection' = foldr (flip f) (Two Nothing Nothing)
  where
    f acc@(Two low high) = \case
      EntireLine _     -> acc
      HalfLine l dir p -> case dir of
                            Lower -> Two (minBy (cmpAlong l) low p) high
                            Upper -> Two low (maxBy (cmpAlong l) high p)

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
                    => Plane r -> Plane r -> f (Plane r) -> Maybe (HalfEdge r)
asHalfEdge h1 h2 hs = asEdgeInterval h1 h2 hs <&> \case
                        UnboundedSubLine sl -> HalfEdge undefined UnboundedVertex h1
                        Segment seg         -> HalfEdge undefined undefined       h1

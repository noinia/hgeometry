{-# LANGUAGE TemplateHaskell  #-}
--------------------------------------------------------------------------------
-- |
-- Module      :  Data.Geometry.Arrangement.Internal
-- Copyright   :  (C) Frank Staals
-- License     :  see the LICENSE file
-- Maintainer  :  Frank Staals
--
-- Data type for representing an Arrangement of lines in \(\mathbb{R}^2\).
--
--------------------------------------------------------------------------------
module Data.Geometry.Arrangement.Internal where

import           Algorithms.BinarySearch
import           Control.Lens
import qualified Data.CircularSeq as CSeq
import           Data.Ext
import qualified Data.Foldable as F
import           Data.Geometry.Boundary
import           Data.Geometry.Box
import           Data.Geometry.Line
import           Data.Geometry.LineSegment
import           Data.Geometry.PlanarSubdivision
import           Data.Geometry.Point
import           Data.Geometry.Properties
import qualified Data.List as List
import           Data.Maybe
import           Data.Ord (Down(..))
import qualified Data.Vector as V
import           Data.Vinyl.CoRec

--------------------------------------------------------------------------------

type ArrangementBoundary s e r = V.Vector (Point 2 r, VertexId' s, Maybe (Line 2 r :+ e))

-- | Data type representing a two dimensional planar arrangement
data Arrangement s l v e f r = Arrangement {
    _inputLines             :: V.Vector (Line 2 r :+ l)
  , _subdivision            :: PlanarSubdivision s v e f r
  , _boundedArea            :: Rectangle () r
  , _unboundedIntersections :: ArrangementBoundary s l r
  } deriving (Show,Eq)
  -- unboundedIntersections also stores the corners of the box. They are not
  -- associated with any line
makeLenses ''Arrangement

type instance NumType   (Arrangement s l v e f r) = r
type instance Dimension (Arrangement s l v e f r) = 2

--------------------------------------------------------------------------------

-- | Builds an arrangement of \(n\) lines
--
-- running time: \(O(n^2\log n\)
constructArrangement       :: (Ord r, Fractional r)
                           => proxy s
                           -> [Line 2 r :+ l]
                           -> Arrangement s l () (Maybe l) () r
constructArrangement px ls = let b  = makeBoundingBox ls
                             in constructArrangementInBox' px b ls

-- | Constructs the arrangemnet inside the box.  note that the resulting box
-- may be larger than the given box to make sure that all vertices of the
-- arrangement actually fit.
--
-- running time: \(O(n^2\log n\)
constructArrangementInBox            :: (Ord r, Fractional r)
                                     => proxy s
                                     -> Rectangle () r
                                     -> [Line 2 r :+ l]
                                     -> Arrangement s l () (Maybe l) () r
constructArrangementInBox px rect ls = let b  = makeBoundingBox ls
                                       in constructArrangementInBox' px (b <> rect) ls


-- | Constructs the arrangemnet inside the box. (for parts to be useful, it is
-- assumed this boxfits at least the boundingbox of the intersections in the
-- Arrangement)
constructArrangementInBox'            :: (Ord r, Fractional r)
                                      => proxy s
                                      -> Rectangle () r
                                      -> [Line 2 r :+ l]
                                      -> Arrangement s l () (Maybe l) () r
constructArrangementInBox' px rect ls =
    Arrangement (V.fromList ls) subdiv rect (link parts' subdiv)
  where
    subdiv = fromConnectedSegments px segs
                & rawVertexData.traverse.dataVal .~ ()
    (segs,parts') = computeSegsAndParts rect ls

computeSegsAndParts         :: forall r l. (Ord r, Fractional r)
                            => Rectangle () r
                            -> [Line 2 r :+ l]
                            -> ( [LineSegment 2 () r :+ Maybe l]
                               , [(Point 2 r, Maybe (Line 2 r :+ l))]
                               )
computeSegsAndParts rect ls = ( segs <> boundarySegs, parts')
  where
    segs         = map (&extra %~ Just)
                 . concatMap (\(l,ls') -> perLine rect l ls') $ makePairs ls
    boundarySegs = map (:+ Nothing) . toSegments . dupFirst $ map fst parts'
    dupFirst = \case []       -> []
                     xs@(x:_) -> xs ++ [x]
    parts'       = unBoundedParts rect ls


perLine       :: forall r l. (Ord r, Fractional r)
              => Rectangle () r -> Line 2 r :+ l -> [Line 2 r :+ l]
              -> [LineSegment 2 () r :+ l]
perLine b m ls = map (:+ m^.extra) . toSegments . rmDuplicates . List.sort $ vs <> vs'
  where
    rmDuplicates = map head . List.group
    vs  = mapMaybe (m `intersectionPoint`) ls
    vs' = maybe [] (\(p,q) -> [p,q]) . asA @(Point 2 r, Point 2 r)
        $ (m^.core) `intersect` (Boundary b)


intersectionPoint                   :: forall r l. (Ord r, Fractional r)
                                    => Line 2 r :+ l -> Line 2 r :+ l -> Maybe (Point 2 r)
intersectionPoint (l :+ _) (m :+ _) = asA @(Point 2 r) $ l `intersect` m


toSegments      :: Ord r => [Point 2 r] -> [LineSegment 2 () r]
toSegments ps = let pts = map ext $ ps in
  zipWith ClosedLineSegment pts (tail pts)


-- | Constructs a boundingbox containing all intersections
--
-- running time: \(O(n^2)\), where \(n\) is the number of input lines
makeBoundingBox :: (Ord r, Fractional r) => [Line 2 r :+ l] -> Rectangle () r
makeBoundingBox = grow 1 . boundingBoxList' . intersections

-- | Computes all intersections
intersections :: (Ord r, Fractional r) => [Line 2 r :+ l] -> [Point 2 r]
intersections = mapMaybe (uncurry intersectionPoint) . allPairs


-- intersections :: forall p r. (Ord r, Fractional r)
--               => [Line 2 r :+ p] -> Map.Map (Point 2 r) (NonEmpty (Line 2 r :+ p))
-- intersections = Map.map sortNub . collect
--               . mapMaybe (\(l,m) -> (l, m,) <$> f l m) . allPairs
--   where
--     f (l :+ _) (m :+ _) = asA (Proxy :: Proxy (Point 2 r)) $ l `intersect` m



-- collect :: Ord k => [(v,v,k)] -> Map.Map k (NonEmpty v)
-- collect = foldr f mempty
--   where
--     f (l,m,p) = Map.insertWith (<>) p (NonEmpty.fromList [l,m])

-- sortNub :: Ord r => NonEmpty (Line 2 r :+ p) -> NonEmpty (Line 2 r :+ p)
-- sortNub = fmap (NonEmpty.head) .  groupLines

-- groupLines :: Ord r => NonEmpty (Line 2 r :+ p)
--            -> NonEmpty (NonEmpty (Line 2 r :+ p))
-- groupLines = NonEmpty.groupWith1 L2 . NonEmpty.sortWith L2


-- -- | Newtype wrapper that allows us to sort lines
-- newtype L2 r p = L2 (Line 2 r :+ p) deriving (Show)

-- instance Eq r => Eq (L2 r p) where
--   (L2 (Line p u :+ _)) == (L2 (Line q v :+ _)) = (p,u) == (q,v)
-- instance Ord r => Ord (L2 r p) where
--   (L2 (Line p u :+ _)) `compare` (L2 (Line q v :+ _)) = p `compare` q <> u `compare` v

-- -- | Collect the intersection points per line
-- byLine :: Ord r
--        => Map.Map (Point 2 r) (NonEmpty (Line 2 r :+ p))
--        -> Map.Map (L2 r p)    (NonEmpty (Point 2 r))
-- byLine = foldr f mempty . flatten . Map.assocs
--   where
--     flatten = concatMap (\(p,ls) -> map (\l -> (L2 l,p)) $ NonEmpty.toList ls)
--     f (l,p) = Map.insertWith (<>) l $ NonEmpty.fromList [p]


-- | Computes the intersections with a particular side
sideIntersections      :: (Ord r, Fractional r)
                       => [Line 2 r :+ l] -> LineSegment 2 q r
                       -> [(Point 2 r, Line 2 r :+ l)]
sideIntersections ls s = let l   = supportingLine s :+ undefined
                         in List.sortOn fst . filter (flip onSegment s . fst)
                          . mapMaybe (\m -> (,m) <$> l `intersectionPoint` m) $ ls

-- | Constructs the unbounded intersections. Reported in clockwise direction.
unBoundedParts         :: (Ord r, Fractional r)
                       => Rectangle () r
                       -> [Line 2 r :+ l]
                       -> [(Point 2 r, Maybe (Line 2 r :+ l))]
unBoundedParts rect ls = [tl] <> t <> [tr] <> reverse r <> [br] <> reverse b <> [bl] <> l
  where
    sideIntersections' = over (traverse._2) Just . sideIntersections ls
    Sides t r b l       = fmap sideIntersections'      $ sides   rect
    Corners tl tr br bl = fmap ((,Nothing) . (^.core)) $ corners rect


-- | Links the vertices  of the outer boundary with those in the subdivision
link       :: Eq r => [(Point 2 r, a)] -> PlanarSubdivision s v (Maybe e) f r
           -> V.Vector (Point 2 r, VertexId' s, a)
link vs ps = V.fromList . map (\((p,x),(_,y)) -> (p,y,x)) . F.toList
           . fromJust' $ alignWith (\(p,_) (q,_) -> p == q) (CSeq.fromList vs) vs'
  where
    vs' = CSeq.fromList . map (\v -> (ps^.locationOf v,v) ) . V.toList
        $ boundaryVertices (outerFaceId ps) ps
    fromJust' = fromMaybe (error "Data.Geometry.Arrangement.link: fromJust")

--------------------------------------------------------------------------------

makePairs :: [a] -> [(a,[a])]
makePairs = go
  where
    go []     = []
    go (x:xs) = (x,xs) : map (\(y,ys) -> (y,x:ys)) (go xs)

allPairs    :: [a] -> [(a,a)]
allPairs ys = go ys
  where
    go []     = []
    go (x:xs) = map (x,) xs ++ go xs

-- | Given a predicate that tests if two elements of a CSeq match, find a
-- rotation of the seqs such at they match.
--
-- Running time: \(O(n)\)
alignWith         :: (a -> b -> Bool) -> CSeq.CSeq a -> CSeq.CSeq b
                  -> Maybe (CSeq.CSeq (a,b))
alignWith p xs ys = CSeq.zipL xs <$> CSeq.findRotateTo (p (CSeq.focus xs)) ys

--------------------------------------------------------------------------------

-- | Given an Arrangement and a line in the arrangement, follow the line
-- through he arrangement.
--
traverseLine       :: (Ord r, Fractional r)
                   => Line 2 r -> Arrangement s l v (Maybe e) f r -> [Dart s]
traverseLine l arr = let md    = findStart l arr
                         dup x = (x,x)
                     in maybe [] (List.unfoldr (fmap dup . follow arr)) md

-- | Find the starting point of the line  the arrangement
findStart       :: forall s l v e f r. (Ord r, Fractional r)
                => Line 2 r -> Arrangement s l v (Maybe e) f r -> Maybe (Dart s)
findStart l arr = do
    (p,_)   <- asA @(Point 2 r, Point 2 r) $
                 l `intersect` (Boundary $ arr^.boundedArea)
    (_,v,_) <- findStartVertex p arr
    findStartDart (arr^.subdivision) v



-- | Given a point on the boundary of the boundedArea box; find the vertex
--  this point corresponds to.
--
-- running time: \(O(\log n)\)
--
-- basically; maps every point to a tuple of the point and the side the
-- point occurs on. We then binary search to find the point we are looking
-- for.
findStartVertex       :: (Ord r, Fractional r)
                      => Point 2 r
                      -> Arrangement s l v e f r
                      -> Maybe (Point 2 r, VertexId' s, Maybe (Line 2 r :+ l))
findStartVertex p arr = do
    ss <- findSide p
    i  <- binarySearchVec (pred' ss) (arr^.unboundedIntersections)
    pure $ arr^.unboundedIntersections.singular (ix i)
  where
    Sides t r b l = sides'' $ arr^.boundedArea
    sides''       = fmap (\(ClosedLineSegment a c) -> LineSegment (Closed a) (Open c)) . sides

    findSide q = fmap fst . List.find (onSegment q . snd) $ zip [1..] [t,r,b,l]

    pred' ss (q,_,_) = let Just j = findSide q
                           x      = before (ss,p) (j,q)
                       in  x == LT || x == EQ

    before (i,p') (j,q') = case i `compare` j of
                                LT -> LT
                                GT -> GT
                                EQ | i == 2 || i == 3 -> Down p' `compare` Down q'
                                   | otherwise        -> p' `compare` q'


-- | Find the starting dart of the given vertex v. Reports a dart s.t.
-- tailOf d = v
--
-- running me: \(O(k)\) where \(k\) is the degree of the vertex
findStartDart      :: PlanarSubdivision s v (Maybe e) f r -> VertexId' s -> Maybe (Dart s)
findStartDart ps v = V.find (\d -> isJust $ ps^.dataOf d) $ incidentEdges v ps
    -- the "real" dart is the one that has ata associated to it.


-- | Given a dart d that incoming to v (headOf d == v), find the outgoing dart
-- colinear with the incoming one. Again reports dart d' s.t. tailOf d' = v
--
-- running time: \(O(k)\), where k is the degree of the vertex d points to
follow       :: (Ord r, Num r) => Arrangement s l v e f r -> Dart s -> Maybe (Dart s)
follow arr d = V.find extends $ incidentEdges v ps
  where
    ps = arr^.subdivision
    v  = headOf d ps
    (up,vp) = over both (^.location) $ endPointData d ps

    extends d' = let wp = ps^.locationOf (headOf d' ps)
                 in d' /= twin d && ccw up vp wp == CoLinear

--------------------------------------------------------------------------------

-- TODO: we can skip the findStart by just traversing from all boundary points

-- computeFaceData :: (Arrangement s v e f r -> Dart s -> f')
--                -> Arrangement s v e f r -> V.Vertex f'
-- computeFaceData arr f = fmap fromJust . V.create $ do
--                           v <- MV.replicate (arr^.subdivision.to numFaces) Nothing
--                           mapM_ (computeFaceData' arr f v) $ arr^.inputLines
--                           pure v


-- computeFaceData' arr f v l = mapM_ (assign ) traverseLine arr l

--------------------------------------------------------------------------------

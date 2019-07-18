{-# LANGUAGE ScopedTypeVariables #-}
--------------------------------------------------------------------------------
-- |
-- Module      :  Algorithms.Geometry.HiddenSurfaceRemoval.HiddenSurfaceRemoval
-- Copyright   :  (C) Frank Staals
-- License     :  see the LICENSE file
-- Maintainer  :  Frank Staals
--
-- Implementation of an algorithm to do Hidden surface removal, i.e. given a
-- set of triangles in \(\mathbb{R}^3\) and a viewpoint, compute which parts of
-- the triangles are visible from the viewpoint.
--
--------------------------------------------------------------------------------
module Algorithms.Geometry.HiddenSurfaceRemoval.HiddenSurfaceRemoval where

import           Control.Applicative
import           Control.Lens
import           Data.Ext
import qualified Data.Foldable as F
import           Data.Functor.Classes
import           Data.Geometry.Arrangement
import           Data.Geometry.Box
import           Data.Geometry.HyperPlane
import           Data.Geometry.Line
import           Data.Geometry.PlanarSubdivision
import           Data.Geometry.Point
import           Data.Geometry.Polygon (centroid)
import           Data.Geometry.Properties
import           Data.Geometry.Triangle
import qualified Data.List as List
import           Data.List.NonEmpty (NonEmpty(..))
import qualified Data.List.NonEmpty as NonEmpty
import           Data.UnBounded
import qualified Data.Vector as V
import qualified Data.Vector.Mutable as MV
import           Data.Vinyl.CoRec

import           Debug.Trace

--------------------------------------------------------------------------------

type PriQ a = [a]

-- | group the elements that have the same core value.
--
-- running time: \(O(n^2)\)
--
-- >>> groupLines [1 :+ "a", 1 :+ "b", 2 :+ "c", 1 :+ "d", 2 :+ "e", 3 :+ "f"]
-- [1 :+ "a" :| ["b","d"],2 :+ "c" :| ["e"],3 :+ "f" :| []]
groupLines :: forall l e. Eq l => [l :+ e] -> [l :+ NonEmpty e]
groupLines = foldr (\(l:+e) -> insert' l e) []
  where
    insert'       :: l -> e -> [l :+ NonEmpty e] -> [l :+ NonEmpty e]
    insert' l e r = case foldr ins (False,[]) r of
                       (False,r') -> (l :+ (e :| [])) : r'
                       (True,r')  -> r'
      where
        ins t@(m :+ ys) (b,acc) | m == l    = (True, (m :+ (e NonEmpty.<| ys)) : acc)
                                | otherwise = (b,    t:acc)


-- | removes degenerate (zero area) triangles
dropDegenerate :: (Eq r, Num r) => [Triangle 2 p r :+ a] -> [Triangle 2 p r :+ a]
dropDegenerate = filter (not . isDegenerateTriangle . (^.core))


-- drawVisible     :: IpeOut (Arrangement s (NonEmpty (Tri v q f r))
--                                          ()
--                                          (Maybe EdgeSide)
--                                          (Maybe (Tri v q f r))
--                                          r) Group r
-- drawVisible arr = drawColoredArrangement $ arr
--   where
--     ps = arr^.subdivision

-- | Given a viewpoint, a screen, and a bunch of triangles with their
-- projections compute the 2D arrangement formed by all supporting lines of the
-- edges. The associated data is:
--
-- lines: the list of triangles who have a side for which this line is a supporting line
-- vertices: () (nothing)
-- edges: which face (either the left or the right; orientation with respect to the direction of the line defining this edge) occurs closer to the viewpoint.
-- faces: the triangle defining this face (if any).
--
render           :: forall proxy s r v q f. (Ord r, Fractional r
                                           , Show r, Show v, Show q, Show f
                                           )
                 => proxy s
                 -> Point 3 r -- ^ the viewpoint
                 -> Rectangle () r  -- ^ The screen
                 -> [Tri v q f r]
                 -> Arrangement s (NonEmpty (Tri v q f r))
                                  ()
                                  (Maybe EdgeSide)
                                  (Maybe (Tri v q f r))
                                  r
render px vp sc ts' | traceShow (vp,sc,ts') False = undefined
render px vp sc ts' = arr''&subdivision.dartData .~ ds
  where
    -- remove degeneate triangles
    ts    = dropDegenerate ts'
    arr   :: Arrangement s (NonEmpty (Tri v q f r)) () (Maybe (NonEmpty (Tri v q f r))) () r
    arr   = constructArrangementInBox px sc $ collectLines ts
    arr'' = arr `seq` arr'
    arr'  = arr&subdivision.faceData .~ fas

    fas = naiveAssignment vp arr ts
    ds  = dartAssignments arr'

-- | Given a list of triangles, collect all supporting lines
collectLines :: (Eq r, Fractional r) => [Tri v q f r] -> [Line 2 r :+ NonEmpty (Tri v q f r)]
collectLines = groupLines . concatMap f
  where
    f t = map (\s -> supportingLine s :+ t) . sideSegments $ t^.core


-- type Tri v q f r = Triangle 2 (v :+ (Point 3 r :+ q) r :+ f


type Tri v q f r = Triangle 2 v r :+ (Triangle 3 q r :+ f)


-- -- | Given a point on the boundary of a projected triangle T, get the point in
-- -- R^3 on the boundary of the triangle (in R^3)
-- liftToR3              :: (Ord r, Fractional r) => Point 2 r -> T v q f r -> Point 3 r
-- liftToR3 q (SP seg t) = Point $ lambda *^ (h s) ^+^ (1-lambda) *^ (h e)
--   where
--     LineSegment' (s :+ _) (e :+ _) = seg
--     lambda = scalarMultiple (q .-. s) (e .-. s)

--     h p = fromMaybe (error "liftToR3: fromJust.") $ lookup q vs'
--     vs' = zip (vs $ t^.core) (vs $ t^.extra.core)

--     vs                  :: Triangle d q r -> [Point d r]
--     vs (Triangle a b c) = map (^.core) [a,b,c]

-- | Given a point on the projected triangle tw, get the point in
-- R^3 on the corresponding triangle in R^3
liftToR3              :: (Ord r, Fractional r) => Point 2 r -> Tri v q f r -> Point 3 r
liftToR3 q (t2 :+ t3) = fromBarricentric (toBarricentric q t2) (t3^.core)

data EdgeSide = L | R deriving (Show,Eq)



-- | Given a view point vp and a point q, order the two triangles based on their
-- first intersection with the line through vp and q.
compareDepthOrder          :: (Ord r, Fractional r)
                           => Point 3 r -> Point 3 r
                           -> Tri v q f r -> Tri v q f r -> Ordering
compareDepthOrder vp q a b = distOf a `compare` distOf b
  where
    l        = lineThrough vp q
    distOf t = maybe err (squaredEuclideanDist vp) $ intersectionPoint l (t^.extra.core)
    err      = error "HiddenSurfaceremoval. depthOrder: no intersection"


-- | Given a view point vp and a projected point q, order the two triangles
-- based on their first intersection with the line through vp and q.
--
-- pre: the projected point occurs in the projection of the first triangle
compareDepthOrder'         :: (Ord r, Fractional r)
                           => Point 3 r -> Point 2 r
                           -> Tri v q f r -> Tri v q f r -> Ordering
compareDepthOrder' vp q a = compareDepthOrder vp (liftToR3 q a) a



-- | computes the intersection point between l and the supporting plane of t
intersectionPoint     :: forall p r. (Ord r, Fractional r)
                      => Line 3 r -> Triangle 3 p r -> Maybe (Point 3 r)
intersectionPoint l t = asA @(Point 3 r) $ l `intersect` supportingPlane t

-- | Given a mapping function, pairs of keys and values, and a length n,
-- construct a vector that such that for every pair (k,v) the vector stores
-- Just v at the index (idxOf k).
--
-- pre: assumes the indexOf function is injective
--
-- >>> fromAssignments id 5 [0 :+ 0, 1 :+ 1, 3 :+ 3, 4 :+ 4]
-- [Just 0,Just 1,Nothing,Just 3,Just 4]
fromAssignments            :: (k -> Int) -> Int -> [k :+ v] -> V.Vector (Maybe v)
fromAssignments idxOf n as = V.create $ do
                               vec <- MV.replicate n Nothing
                               mapM_ (\(k :+ v) -> MV.modify vec (<|> Just v) (idxOf k)) as
                               pure vec

-- | Computes which darts should be drawn
dartAssignments     :: (Ord r, Fractional r)
                    => Arrangement s l () e (Maybe (Tri v q f r)) r
                    -> V.Vector (Dart s,Maybe EdgeSide)
dartAssignments arr = (\d -> (d,f d)) <$> darts' ps
  where
    -- if p,q both occur in the trinagle, return Just x, otherwise ret. nothing
    inTri (p,q) x t | all (`onTriangle` (t^.core)) [p,q] = Just x
                    | otherwise                  = Nothing

    ps  = arr^.subdivision
          -- if the endpoints of this dart both occur in the left triangle
          -- then the dart forms the left boundary, otherwise the right boundary
          --
          -- FIXME: We should test if left occurs before right or vice versa
          -- rather than just picking left.
    f d = let pts = bimap (^.location) (^.location) $ ps^.endPointsOf d
              g i x = ps^.dataOf i >>= inTri pts x
          in (g (leftFace d ps) L <|> g (rightFace d ps) R)




-- | Computes a face assignment for all faces by just naively computing the first
-- visible triangle for every face. We do so by just sampling a point in the face
--
-- running time: \(O(n^3)\), where \(n\) is the number of input triangles.
naiveAssignment           :: (Ord r, Fractional r
                             , Show r, Show v, Show q, Show f
                             )
                          => Point 3 r
                          -> Arrangement s l () g () r
                          -> [Tri v q f r]
                          -> V.Vector (Maybe (Tri v q f r))
naiveAssignment vp arr ts = f <$> faces' ps
  where
    f i = let (pg :+ _) = rawFaceBoundary i ps
              c         = centroid pg
          in minimumBy' (compareDepthOrder' vp c) $ filter (\t -> c `onTriangle` (t^.core)) ts
    ps = arr^.subdivision
  -- observe that all faces in the arrangement are convex, so the centroid
  -- lies inside the face
  --
  -- observe that since we explicitly filter the triangles to contain c, we can indeed
  -- safely use compareDepthOrder'

onTriangle' :: (Show r, Show p, Ord r, Fractional r) => Point 2 r -> Triangle 2 p r -> Bool
onTriangle' q t = traceShow ("onTriangle': ",q,t) $ q `onTriangle` t
--
-- >>> minimumBy' compare []
-- Nothing
-- >>> minimumBy' compare [1,0,5]
-- Just 0
minimumBy'     :: Foldable f => (a -> a -> Ordering) -> f a -> Maybe a
minimumBy' cmp = topToMaybe  . List.minimumBy (liftCompare cmp)
               . (Top :) . fmap ValT . F.toList


--------------------------------------------------------------------------------

-- scanAll     :: (Ord r, Fractional r)
--             => Arrangement s () (T v q f r) () r
--             -> [FaceId' s :+ Maybe (T v q f r)]
-- scanAll arr = concatMap (scanLine arr) . V.toList $ arr^.inputLines


-- -- | Sweeps along line l in the arrangement, maintaining the first/topmost
-- -- visible triangle during the sweep. While doing this we thus figure out
-- -- 1) which triangle is the top-triangle just left of the line
-- -- 2) which triangle is the top-triangle just right of the line.
-- --
-- -- We collect this information in the form of assignments: i.e. for a
-- -- faceId we simply remember what data to assign it to.
-- -- (we do this so that we can process all these assignments in a batch.)
-- scanLine              :: (Ord r, Fractional r)
--                       => Arrangement s () (T v q f r) () r
--                       -> Line 2 r :+ T v q f r
--                       -> [FaceId' s :+ Maybe (T v q f r)]
-- scanLine arr (l :+ t) = view _1
--                       . List.foldl' f (STR [] mempty mempty) $ traverseLine l arr
--   where
--     f (STR fs lst rst) d = STR fs lst rst
--       -- | isActive d = STR (fa <> fs) st
--       --                  | otherwise  = STR ds fs st





-- data Changes a = Changes { _enterLeft  :: !a
--                          , _enterRight :: !a
--                          , _exitLeft   :: !a
--                          , _exitRight  :: !a
--                          } deriving (Show,Eq, Functor, Foldable, Traversable)

-- stateChanges          :: Arrangement s l () (T v q f r) () r -> Dart s -> Dart s
--                       -> Changes [T v q f r]
-- stateChanges arr pd d = Changes lEnters rEnters lLeaves rLeaves
--   where
--     lEnters = []
--     rEnters = []
--     lLeaves = []
--     rLeaves = []



--------------------------------------------------------------------------------

data Test

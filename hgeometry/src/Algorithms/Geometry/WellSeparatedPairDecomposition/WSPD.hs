--------------------------------------------------------------------------------
-- |
-- Module      :  Algorithms.Geometry.WellSeparatedPairDecomposition.WSPD
-- Copyright   :  (C) Frank Staals
-- License     :  see the LICENSE file
-- Maintainer  :  Frank Staals
--
-- Algorithm to construct a well separated pair decomposition (wspd).
--
--------------------------------------------------------------------------------
module Algorithms.Geometry.WellSeparatedPairDecomposition.WSPD where

import           Algorithms.Geometry.WellSeparatedPairDecomposition.Types
import           Control.Lens hiding (Level, levels)
import           Control.Monad.Reader
import           Control.Monad.ST (ST,runST)
import           Data.BinaryTree
import           Data.Ext
import qualified Data.Foldable as F
import           Data.Geometry.Box
import           Data.Geometry.Point
import           Data.Geometry.Properties
import           Data.Geometry.Transformation
import           Data.Geometry.Vector
import qualified Data.Geometry.Vector as GV
import qualified Data.IntMap.Strict as IntMap
import qualified Data.LSeq as LSeq
import           Data.LSeq (LSeq,toSeq,ViewL(..),ViewR(..),pattern (:<|))
import qualified Data.List as L
import qualified Data.List.NonEmpty as NonEmpty
import           Data.Maybe
import           Data.Ord (comparing)
import           Data.Range
import qualified Data.Range as Range
import qualified Data.Sequence as S
import qualified Data.Vector as V
import qualified Data.Vector.Mutable as MV
import           GHC.TypeLits

import           Debug.Trace

--------------------------------------------------------------------------------

-- | Construct a split tree
--
-- running time: \(O(n \log n)\)
fairSplitTree     :: (Fractional r, Ord r, Arity d, 1 <= d
                     , Show r, Show p
                     )
                  => NonEmpty.NonEmpty (Point d r :+ p) -> SplitTree d p r ()
fairSplitTree pts = foldUp node' Leaf $ fairSplitTree' n pts'
  where
    pts' = GV.imap sortOn . pure . g $ pts
    n    = length $ pts'^.GV.element (C :: C 0)

    sortOn' i = NonEmpty.sortWith (^.core.unsafeCoord i)
    sortOn  i = LSeq.fromNonEmpty . sortOn' (i + 1)
    -- sorts the points on the first coordinate, and then associates each point
    -- with an index,; its rank in terms of this first coordinate.
    g = NonEmpty.zipWith (\i (p :+ e) -> p :+ (i :+ e)) (NonEmpty.fromList [0..])
      . sortOn' 1

    -- node' :: b -> a -> b -> b
    -- node'       :: SplitTree d p r () -> Int -> SplitTree d p r () -> SplitTree d p r ()
    node' l j r = Node l (NodeData j (bbOf l <> bbOf r) ()) r


-- | Given a split tree, generate the Well separated pairs
--
-- running time: \(O(s^d n)\)
wellSeparatedPairs   :: (Floating r, Ord r, Arity d, Arity (d + 1))
                     => r -> SplitTree d p r a -> [WSP d p r a]
wellSeparatedPairs s = f
  where
    f (Leaf _)     = []
    f (Node l _ r) = findPairs s l r ++ f l ++ f r



-- -- | Given a split tree, generate the well separated pairs such that one set is
-- -- a singleton.
-- -- running time: \(O(s^d n\log n)\)
-- wellSeparatedPairSingletons   :: (Fractional r, Ord r, AlwaysTrueWSPD d)
--                               => r -> SplitTree d p r a -> [(Point d r :+ p, PointSet d p r (Sized a))]
-- wellSeparatedPairSingletons s t = concatMap split $ wellSeparatedPairs s t'
--   where
--     split (l,r) = undefined
--       -- | measure l <= measure r = map (,r) $ F.toList l
--       -- | otherwise              = map (,l) $ F.toList r
--     t' = foldUpData (\l nd r -> )

--     t


--------------------------------------------------------------------------------
-- * Building the split tree

-- | Given the points, sorted in every dimension, recursively build a split tree
--
-- The algorithm works in rounds. Each round takes O(n) time, and halves the
-- number of points. Thus, the total running time is O(n log n).
--
-- The algorithm essentially builds a path in the split tree; at every node on
-- the path that we construct, we split the point set into two sets (L,R)
-- according to the longest side of the bounding box.
--
-- The smaller set is "assigned" to the current node and set asside. We
-- continue to build the path with the larger set until the total number of
-- items remaining is less than n/2.
--
-- To start the next round, each node on the path needs to have the points
-- assigned to that node, sorted in each dimension (i.e. the Vector
-- (PointSeq))'s. Since we have the level assignment, we can compute these
-- lists by traversing each original input list (i.e. one for every dimension)
-- once, and partition the points based on their level assignment.
fairSplitTree'       :: (Fractional r, Ord r, Arity d, 1 <= d
                        , Show r, Show p
                        )
                     => Int -> GV.Vector d (PointSeq d (Idx :+ p) r)
                     -> BinLeafTree Int (Point d r :+ p)
fairSplitTree' n pts
    | n <= 1    = let p = LSeq.head $ pts^.GV.element (C :: C 0) in Leaf (dropIdx p)
    | otherwise = foldr node' (V.last path) $ V.zip nodeLevels (V.init path)
  where
    -- note that points may also be assigned level 'Nothing'.
    (levels, nodeLevels'@(maxLvl NonEmpty.:| _)) = runST $ do
        lvls  <- MV.replicate n Nothing
        ls    <- runReaderT (assignLevels (n `div` 2) 0 pts (Level 0 Nothing) []) lvls
        lvls' <- V.unsafeFreeze lvls
        pure (lvls',ls)

    -- TODO: We also need to report the levels in the order in which they are
    -- assigned to nodes

    nodeLevels = V.fromList . L.reverse . NonEmpty.toList $ nodeLevels'

    -- levels = traceShow ("Levels",levels',maxLvl) levels'

    -- path = traceShow ("path", path',nodeLevels) path'
    distrPts = distributePoints (1 + maxLvl^.unLevel) levels pts

    path = recurse <$> distrPts -- (traceShow ("distributed pts",distrPts) distrPts)

    -- node' (lvl,lc) rc | traceShow ("node' ",lvl,lc,rc) False = undefined
    node' (lvl,lc) rc = case lvl^?widestDim._Just of
                          Nothing -> error "Unknown widest dimension"
                          Just j  -> Node lc j rc
    recurse pts' = fairSplitTree' (length $ pts'^.GV.element (C :: C 0))
                                  (reIndexPoints pts')

-- | Assign the points to their the correct class. The 'Nothing' class is
-- considered the last class
distributePoints          :: (Arity d , Show r, Show p)
                          => Int -> V.Vector (Maybe Level)
                          -> GV.Vector d (PointSeq d (Idx :+ p) r)
                          -> V.Vector (GV.Vector d (PointSeq d (Idx :+ p) r))
distributePoints k levels = transpose . fmap (distributePoints' k levels)

transpose :: Arity d => GV.Vector d (V.Vector a) -> V.Vector (GV.Vector d a)
transpose = V.fromList . map GV.vectorFromListUnsafe . L.transpose
          . map V.toList . F.toList

-- | Assign the points to their the correct class. The 'Nothing' class is
-- considered the last class
distributePoints'              :: Int                      -- ^ number of classes
                               -> V.Vector (Maybe Level)   -- ^ level assignment
                               -> PointSeq d (Idx :+ p) r  -- ^ input points
                               -> V.Vector (PointSeq d (Idx :+ p) r)
distributePoints' k levels pts
  | otherwise
  = fmap fromSeqUnsafe $ V.create $ do
    v <- MV.replicate k mempty
    forM_ pts $ \p ->
      append v (level p) p
    pure v
  where
    level p = maybe (k-1) _unLevel $ levels V.! (p^.extra.core)
    append v i p = MV.read v i >>= MV.write v i . (S.|> p)

fromSeqUnsafe = LSeq.promise . LSeq.fromSeq

-- | Given a sequence of points, whose index is increasing in the first
-- dimension, i.e. if idx p < idx q, then p[0] < q[0].
-- Reindex the points so that they again have an index
-- in the range [0,..,n'], where n' is the new number of points.
--
-- running time: O(n' * d) (more or less; we are actually using an intmap for
-- the lookups)
--
-- alternatively: I can unsafe freeze and thaw an existing vector to pass it
-- along to use as mapping. Except then I would have to force the evaluation
-- order, i.e. we cannot be in 'reIndexPoints' for two of the nodes at the same
-- time.
--
-- so, basically, run reIndex points in ST as well.
reIndexPoints      :: (Arity d, 1 <= d)
                   => GV.Vector d (PointSeq d (Idx :+ p) r)
                   -> GV.Vector d (PointSeq d (Idx :+ p) r)
reIndexPoints ptsV = fmap reIndex ptsV
  where
    pts = ptsV^.GV.element (C :: C 0)

    reIndex = fmap (\p -> p&extra.core %~ fromJust . flip IntMap.lookup mapping')
    mapping' = IntMap.fromAscList $ zip (map (^.extra.core) . F.toList $ pts) [0..]

-- | ST monad with access to the vector storign the level of the points.
type RST s = ReaderT (MV.MVector s (Maybe Level)) (ST s)

-- | Assigns the points to a level. Returns the list of levels used. The first
-- level in the list is the level assigned to the rest of the nodes. Their
-- level is actually still set to Nothing in the underlying array.
assignLevels                  :: (Fractional r, Ord r, Arity d
                                 , Show r, Show p
                                 )
                              => Int -- ^ Number of items we need to collect
                              -> Int -- ^ Number of items we collected so far
                              -> GV.Vector d (PointSeq d (Idx :+ p) r)
                              -> Level -- ^ next level to use
                              -> [Level] -- ^ Levels used so far
                              -> RST s (NonEmpty.NonEmpty Level)
assignLevels h m pts l prevLvls
  | m >= h    = pure (l NonEmpty.:| prevLvls)
  | otherwise = do
    pts' <- compactEnds pts
    -- find the widest dimension j = i+1
    let j    = widestDimension pts'
        i    = j - 1 -- traceShow  ("i",j,pts') j - 1
        extJ = (extends pts')^.ix' i
        mid  = midPoint extJ

    -- find the set of points that we have to delete, by looking at the sorted
    -- list L_j. As a side effect, this will remove previously assigned points
    -- from L_j.
    (lvlJPts,deletePts) <- findAndCompact j (pts'^.ix' i) mid
    let pts''     = pts'&ix' i .~ lvlJPts
        l'        = l&widestDim .~ Just j
    forM_ deletePts $ \p ->
      assignLevel p l'
    assignLevels h (m + length deletePts) pts'' (nextLevel l) (l' : prevLvls)

-- | Remove already assigned pts from the ends of all vectors.
compactEnds        :: Arity d
                   => GV.Vector d (PointSeq d (Idx :+ p) r)
                   -> RST s (GV.Vector d (PointSeq d (Idx :+ p) r))
compactEnds = traverse compactEnds'

-- | Assign level l to point p
assignLevel     :: (c :+ (Idx :+ p)) -> Level -> RST s ()
assignLevel p l = ask >>= \levels -> lift $ MV.write levels (p^.extra.core) (Just l)

-- | Get the level of a point
levelOf   :: (c :+ (Idx :+ p)) -> RST s (Maybe Level)
levelOf p = ask >>= \levels -> lift $ MV.read levels (p^.extra.core)

-- | Test if the point already has a level assigned to it.
hasLevel :: c :+ (Idx :+ p) -> RST s Bool
hasLevel = fmap isJust . levelOf

-- | Remove allready assigned points from the sequence
--
-- pre: there are points remaining
compactEnds'              :: PointSeq d (Idx :+ p) r
                          -> RST s (PointSeq d (Idx :+ p) r)
compactEnds' (l0 :<| s0) = fmap fromSeqUnsafe . goL $ l0 S.<| toSeq s0
  where
    goL s@(S.viewl -> l S.:< s') = hasLevel l >>= \case
                                     False -> goR s
                                     True  -> goL s'
    goR s@(S.viewr -> s' S.:> r) = hasLevel r >>= \case
                                     False -> pure s
                                     True  -> goR s'


-- | Given the points, ordered by their j^th coordinate, split the point set
-- into a "left" and a "right" half, i.e. the points whose j^th coordinate is
-- at most the given mid point m, and the points whose j^th coordinate is
-- larger than m.
--
-- We return a pair (Largest set, Smallest set)
--
--
--fi ndAndCompact works by simultaneously traversing the points from left to
-- right, and from right to left. As soon as we find a point crossing the mid
-- point we stop and return. Thus, in principle this takes only O(|Smallest
-- set|) time.
--
-- running time: O(|Smallest set|) + R, where R is the number of *old* points
-- (i.e. points that should have been removed) in the list.
findAndCompact                   :: (Ord r, Arity d
                                    , Show r, Show p
                                    )
                                 => Int
                                    -- ^ the dimension we are in, i.e. so that we know
                                    -- which coordinate of the point to compare
                                 -> PointSeq d (Idx :+ p) r
                                 -> r -- ^ the mid point
                                 -> RST s ( PointSeq d (Idx :+ p) r
                                          , PointSeq d (Idx :+ p) r
                                          )
findAndCompact j (l0 :<| s0) m = fmap select . stepL $ l0 S.<| toSeq s0
  where
    -- stepL and stepR together build a data structure (FAC l r S) that
    -- contains the left part of the list, i.e. the points before midpoint, and
    -- the right part of the list., and a value S that indicates which part is
    -- the short side.

    -- stepL takes a step on the left side of the list; if the left point l
    -- already has been assigned, we continue waling along (and "ignore" the
    -- point). If it has not been assigned, and is before the mid point, we
    -- take a step from the right, and add l onto the left part. If it is
    -- larger than the mid point, we have found our split.
    -- stepL :: S.Seq (Point d r :+ (Idx :+ p)) -> ST s (FindAndCompact d r (Idx :+ p))
    stepL s = case S.viewl s of
      S.EmptyL  -> pure $ FAC mempty mempty L
      l S.:< s' -> hasLevel l >>= \case
                     False -> if l^.core.unsafeCoord j <= m
                                 then addL l <$> stepR s'
                                 else pure $ FAC mempty s L
                     True  -> stepL s' -- delete, continue left

    -- stepR :: S.Seq (Point d r :+ (Idx :+ p)) -> ST s (FindAndCompact d r (Idx :+ p))
    stepR s = case S.viewr s of
      S.EmptyR  -> pure $ FAC mempty mempty R
      s' S.:> r -> hasLevel r >>= \case
                     False -> if r^.core.unsafeCoord j >= m
                                 then addR r <$> stepL s'
                                 else pure $ FAC s mempty R
                     True  -> stepR s'


    addL l x = x&leftPart  %~ (l S.<|)
    addR r x = x&rightPart %~ (S.|> r)

    select = over both fromSeqUnsafe . select'

    -- select' f | traceShow ("select'", f) False = undefined
    select' (FAC l r L) = (r, l)
    select' (FAC l r R) = (l, r)


-- | Find the widest dimension of the point set
--
-- pre: points are sorted according to their dimension
widestDimension :: (Num r, Ord r, Arity d) => GV.Vector d (PointSeq d p r) -> Int
widestDimension = fst . L.maximumBy (comparing snd) . zip [1..] . F.toList . widths

widths :: (Num r, Arity d) => GV.Vector d (PointSeq d p r) -> GV.Vector d r
widths = fmap Range.width . extends



-- | get the extends of the set of points in every dimension, i.e. the left and
-- right boundaries.
--
-- pre: points are sorted according to their dimension
extends :: Arity d => GV.Vector d (PointSeq d p r) -> GV.Vector d (Range r)
extends = GV.imap (\i pts ->
                     ClosedRange ((LSeq.head pts)^.core.unsafeCoord (i + 1))
                                 ((LSeq.last pts)^.core.unsafeCoord (i + 1)))


--------------------------------------------------------------------------------
-- * Finding Well Separated Pairs

findPairs                     :: (Floating r, Ord r, Arity d, Arity (d + 1))
                              => r -> SplitTree d p r a -> SplitTree d p r a
                              -> [WSP d p r a]
findPairs s l r
  | areWellSeparated' s l r   = [(l,r)]
  | maxWidth l <=  maxWidth r = concatMap (findPairs s l) $ children' r
  | otherwise                 = concatMap (findPairs s r) $ children' l


-- | Test if the two sets are well separated with param s
areWellSeparated                     :: (Arity d, Arity (d + 1), Fractional r, Ord r)
                                     => r -- ^ separation factor
                                     -> SplitTree d p r a
                                     -> SplitTree d p r a -> Bool
areWellSeparated _ (Leaf _) (Leaf _) = True
areWellSeparated s l        r        = boxBox s (bbOf l)   (bbOf r)


-- areWellSeparated s (Leaf p)      (Node _ nd _) = pointBox s (p^.core) (nd^.bBox)
-- areWellSeparated s (Node _ nd _) (Leaf p)      = pointBox s (p^.core) (nd^.bBox)
-- areWellSeparated s (Node _ ld _) (Node _ rd _) = boxBox   s (ld^.bBox) (rd^.bBox)


-- -- | Test if the point and the box are far enough appart
-- pointBox       :: (Fractional r, Ord r, AlwaysTruePFT d, AlwaysTrueTransformation d)
--                => r -> Point d r -> Box d p r -> Bool
-- pointBox s p b = not $ p `inBox` b'
--   where
--     v  = (centerPoint b)^.vector
--     b' = translateBy v . scaleUniformlyBy s . translateBy ((-1) *^ v) $ b

-- | Test if the two boxes are sufficiently far appart
boxBox         :: (Fractional r, Ord r, Arity d, Arity (d + 1))
               => r -> Box d p r -> Box d p r -> Bool
boxBox s lb rb = boxBox' lb rb && boxBox' rb lb
  where
    boxBox' b' b = not $ b' `intersects` bOut
      where
        v    = (centerPoint b)^.vector
        bOut = translateBy v . scaleUniformlyBy s . translateBy ((-1) *^ v) $ b

--------------------------------------------------------------------------------
-- * Alternative def if wellSeparated that uses fractional


areWellSeparated'                     :: (Floating r, Ord r, Arity d)
                                      => r
                                      -> SplitTree d p r a
                                      -> SplitTree d p r a
                                      -> Bool
areWellSeparated' _ (Leaf _) (Leaf _) = True
areWellSeparated' s l        r        = boxBox1 s (bbOf l) (bbOf r)

-- (Leaf p)      (Node _ nd _) = pointBox' s (p^.core) (nd^.bBox)
-- areWellSeparated' s (Node _ nd _) (Leaf p)      = pointBox' s (p^.core) (nd^.bBox)
-- areWellSeparated' s (Node _ ld _) (Node _ rd _) = boxBox'   s (ld^.bBox) (rd^.bBox)

boxBox1         :: (Floating r, Ord r, Arity d) => r -> Box d p r -> Box d p r -> Bool
boxBox1 s lb rb = euclideanDist (centerPoint lb) (centerPoint rb) >= (s+1)*d
  where
    diam b = euclideanDist (b^.minP.core.cwMin) (b^.maxP.core.cwMax)
    d      = max (diam lb) (diam rb)




--------------------------------------------------------------------------------
-- * Helper stuff


-- | Computes the maximum width of a splitTree
maxWidth                             :: (Arity d, Num r)
                                     => SplitTree d p r a -> r
maxWidth (Leaf _)                    = 0
maxWidth (Node _ (NodeData i b _) _) = fromJust $ widthIn' i b

-- | 'Computes' the bounding box of a split tree
bbOf                             :: Ord r => SplitTree d p r a -> Box d () r
bbOf (Leaf p)                    = boundingBox $ p^.core
bbOf (Node _ (NodeData _ b _) _) = b


children'              :: BinLeafTree v a -> [BinLeafTree v a]
children' (Leaf _)     = []
children' (Node l _ r) = [l,r]


-- | Turn a traversal into lens
ix'   :: (Arity d, KnownNat d) => Int -> Lens' (GV.Vector d a) a
ix' i = singular (GV.element' i)


dropIdx                 :: core :+ (t :+ extra) -> core :+ extra
dropIdx (p :+ (_ :+ e)) = p :+ e

--------------------------------------------------------------------------------

{-# LANGUAGE TemplateHaskell  #-}
{-# LANGUAGE LambdaCase  #-}
module Algorithms.Geometry.WellSeparatedPairDecomposition.WSPD where

import Control.Monad.Reader

import Data.Semigroup
import Data.Ord(comparing)
import Control.Applicative
import Control.Monad(forM_)
import Control.Monad.ST(ST,runST)
import Data.Ext
import Control.Lens hiding (Level, levels)
import Data.Geometry.Point
import Data.Geometry.Vector
import Data.Geometry.Box
import Data.BinaryTree
import Data.Maybe
import GHC.TypeLits
import qualified Data.Geometry.Vector as GV



import qualified Data.Vector as V
import qualified Data.Vector.Mutable as MV

import Data.Range
import qualified Data.Range as Range

import qualified Data.Seq2 as S2
import qualified Data.Sequence as S
import qualified Data.List as L
import qualified Data.List.NonEmpty as NonEmpty


import qualified Data.Foldable as F
import qualified Data.Traversable as Tr

import Algorithms.Geometry.WellSeparatedPairDecomposition.Types

--------------------------------------------------------------------------------

-- | Construct a split tree
--
--
fairSplitTree     :: (Fractional r, Ord r, Arity d, Index' 0 d, KnownNat d)
                  => NonEmpty.NonEmpty (Point d r :+ p) -> SplitTree d p r ()
fairSplitTree pts = fairSplitTree' (NonEmpty.length pts) . GV.imap f $ pure (g pts)
  where
    f i  = S2.viewL1FromNonEmpty . NonEmpty.sortOn (^.core.unsafeCoord i)
    g = NonEmpty.zipWith (\i (p :+ e) -> p :+ (i :+ e)) (NonEmpty.fromList [0..])

-- | Given a split tree, generate the Well separated pairs
--
--
wellSeparatedPairs   :: (Num r, Ord r, Arity d, KnownNat d)
                     => r -> SplitTree d p r a -> [WSP d p r a]
wellSeparatedPairs s = f
  where
    f (Leaf _)     = []
    f (Node l _ r) = findPairs s l r ++ f l ++ f r

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
fairSplitTree'       :: (Fractional r, Ord r, Arity d, Index' 0 d, KnownNat d)
                     => Int -> GV.Vector d (PointSeq d (Idx :+ p) r)
                     -> SplitTree d p r ()
fairSplitTree' n pts
    | n <= 1    = let (p S2.:< _) = pts^.GV.element (C :: C 0) in Leaf (dropIdx p)
    | otherwise = undefined
  where
    -- note that points may also be assigned level 'Nothing'.
    (levels, maxLvl) = runST $ do
        lvls  <- MV.replicate n Nothing
        l     <- runReaderT (assignLevels (n `div` 2) 0 pts (Level 0)) lvls
        lvls' <- V.unsafeFreeze lvls
        pure (lvls',l)


    pts' = distributePoints (unLevel maxLvl) levels pts

    -- FIXME: We cannot really afford to run reIndexPoints on all nodes!!!!!


-- TODO, check if this works as I expect.
distributePoints          :: Arity d
                          => Int -> V.Vector (Maybe Level)
                          -> GV.Vector d (PointSeq d (Idx :+ p) r)
                          -> V.Vector (GV.Vector d (PointSeq d (Idx :+ p) r))
distributePoints k levels = Tr.traverse (distributePoints' k levels)

-- | Assign the points to their the correct class. The 'Nothing' class is
-- considered the last class
distributePoints'              :: Int                      -- ^ number of classes
                               -> V.Vector (Maybe Level)   -- ^ level assignment
                               -> PointSeq d (Idx :+ p) r  -- ^ input points
                               -> V.Vector (PointSeq d (Idx :+ p) r)
distributePoints' k levels pts = fmap fromSeqUnsafe $ V.create $ do
    v <- MV.replicate k mempty
    forM_ pts $ \p ->
      append v (level p) p
    pure v
  where
    level p = maybe (k-1) unLevel $ levels V.! (p^.extra.core)
    append v i p = MV.read v i >>= MV.write v i . (S.|> p)


-- | Reindex the points so that they again have an index in the range
-- [0,..,n'], where n' is the new number of points.
--
-- running time: O(oldN * d)
--
-- FIXME: We should get this down to newN * d, then we are fine.
--
-- one copout would be to use an IntMap to implement mapping'.
--
-- alternatively: I can unsafe freeze and thaw an existing vector to pass it
-- along to use as mapping. Except then I would have to force the evaluation
-- order, i.e. we cannot be in 'reIndexPoints' for two of the nodes at the same
-- time.
--
-- so, basically, run reIndex points in ST as well.
reIndexPoints           :: (Arity d, Index' 0 d)
                        => Int -- ^ oldN
                        -> GV.Vector d (PointSeq d (Idx :+ p) r)
                        -> GV.Vector d (PointSeq d (Idx :+ p) r)
reIndexPoints oldN ptsV = fmap reIndex ptsV
  where
    pts = ptsV^.GV.element (C :: C 0)

    reIndex = fmap (\p -> p&extra.core %~ (mapping' V.!))

    mapping' = V.create $ do
        v <- MV.new oldN
        forM_ (zip [0..] (F.toList pts)) $ \(i,p) ->
          MV.write v (p^.extra.core) i
        pure v



-- | ST monad with access to the vector storign the level of the points.
type RST s = ReaderT (MV.MVector s (Maybe Level)) (ST s)

-- | Assigns the points a level. Returns the number l of levels used. (note
-- that this means the maximum assigned level is l-1; the level of the
-- remaining points is Nothing).
assignLevels                  :: (Fractional r, Ord r, Arity d, KnownNat d)
                              => Int -- ^ Number of items we need to collect
                              -> Int -- ^ Number of items we collected so far
                              -> GV.Vector d (PointSeq d (Idx :+ p) r)
                              -> Level -- ^ current level
                              -> RST s Level
assignLevels h m pts l
  | m > h     = pure l
  | otherwise = do
    pts' <- compactEnds pts
    -- find the widest dimension j = i+1
    let i    = -1 + widestDimension pts'
        extJ = (extends pts')^.ix' i
        mid  = midPoint extJ
    -- find the set of points that we have to delete, by looking at the sorted
    -- list L_j. As a side effect, this will remove previously assigned points
    -- from L_j.
    (lvlJPts,deletePts) <- findAndCompact i (pts'^.ix' i) mid
    let pts'' = pts'&ix' i .~ lvlJPts
    forM_ deletePts $ \p ->
      assignLevel p l
    assignLevels h (m + F.length deletePts) pts'' (succ l)

-- | Remove already assigned pts from the ends of all vectors.
compactEnds        :: Arity d
                   => GV.Vector d (PointSeq d (Idx :+ p) r)
                   -> RST s (GV.Vector d (PointSeq d (Idx :+ p) r))
compactEnds = Tr.traverse compactEnds'

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
compactEnds'               :: PointSeq d (Idx :+ p) r
                           -> RST s (PointSeq d (Idx :+ p) r)
compactEnds' (l0 S2.:< s0) = fmap fromSeqUnsafe . goL $ l0 S.<| s0
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
-- findAndCompact works by simultaneously traversing the points from left to
-- right, and from right to left. As soon as we find a point crossing the mid
-- point we stop and return. Thus, in principle this takes only O(|Smallest
-- set|) time.
--
-- running time: O(|Smallest set|) + R, where R is the number of *old* points
-- (i.e. points that should have been removed) in the list.
findAndCompact                   :: (Ord r, Arity d)
                                 => Int
                                    -- ^ the dimension we are in, i.e. so that we know
                                    -- which coordinate of the point to compare
                                 -> PointSeq d (Idx :+ p) r
                                 -> r -- ^ the mid point
                                 -> RST s ( PointSeq d (Idx :+ p) r
                                          , PointSeq d (Idx :+ p) r
                                          )
findAndCompact j (l0 S2.:< s0) m = fmap select . stepL $ l0 S.<| s0
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
    stepL s@(S.viewl -> l S.:< s') = hasLevel l >>= \case
                                       False -> if l^.core.unsafeCoord j <= m
                                                     then addL l <$> stepR s'
                                                     else pure $ FAC mempty s L
                                       True  -> stepL s' -- delete, continue left

    -- stepR :: S.Seq (Point d r :+ (Idx :+ p)) -> ST s (FindAndCompact d r (Idx :+ p))
    stepR s@(S.viewr -> s' S.:> r) = hasLevel r >>= \case
                                       False -> if (r^.core.unsafeCoord j) >= m
                                                     then addR r <$> stepL s'
                                                     else pure $ FAC s mempty R
                                       True  -> stepR s'

    addL l x = x&leftPart  %~ (l S.<|)
    addR r x = x&rightPart %~ (S.|> r)

    select = over both fromSeqUnsafe . select'

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
extends = GV.imap (\i pts@(l S2.:< _) ->
                     let (_ S2.:> r) = S2.viewL1toR1 pts
                     in ClosedRange (l^.core.unsafeCoord i) (r^.core.unsafeCoord i))


--------------------------------------------------------------------------------
-- * Finding Well Separated Pairs

findPairs                     :: (Num r, Ord r,Arity d, KnownNat d)
                              => r -> SplitTree d p r a -> SplitTree d p r a
                              -> [WSP d p r a]
findPairs s l r
  | areWellSeparated s l r    = [(l,r)]
  | maxWidth l <=  maxWidth r = concatMap (findPairs s l) $ children' r
  | otherwise                 = concatMap (findPairs s r) $ children' l


-- | Test if the two sets are well separated with param s
areWellSeparated       :: r -> SplitTree d p r a -> SplitTree d p r a -> Bool
areWellSeparated s l r = undefined -- TODO!!!


maxWidth                             :: (Arity d, KnownNat d, Num r)
                                     => SplitTree d p r a -> r
maxWidth (Leaf _)                    = 0
maxWidth (Node _ (NodeData i b _) _) = fromJust $ widthIn' i b

--------------------------------------------------------------------------------
-- * Helper stuff

children'              :: BinLeafTree v a -> [BinLeafTree v a]
children' (Leaf _)     = []
children' (Node l _ r) = [l,r]


fromSeqUnsafe                         :: S.Seq a -> S2.ViewL1 a
fromSeqUnsafe (S.viewl -> (l S.:< s)) = l S2.:< s
fromSeqUnsafe _                       = error "fromSeqUnsafe: Empty seq"


-- | Turn a traversal into lens
ix'   :: (Arity d, KnownNat d) => Int -> Lens' (GV.Vector d a) a
ix' i = singular (GV.element' i)


dropIdx                 :: core :+ (t :+ extra) -> core :+ extra
dropIdx (p :+ (_ :+ e)) = p :+ e

--------------------------------------------------------------------------------

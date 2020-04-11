{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE ScopedTypeVariables #-}
module Demo.ExpectedPairwiseDistance where

import           Algorithms.Geometry.Diameter.Naive
import           Algorithms.Geometry.WellSeparatedPairDecomposition.Types
import           Algorithms.Geometry.WellSeparatedPairDecomposition.WSPD
import           Control.Lens
import           Control.Monad ((<=<))
import           Data.BinaryTree
import qualified Data.ByteString as B
import qualified Data.ByteString.Char8 as C
import           Data.Char (isSpace)
import           Data.Data
import           Data.Ext
import qualified Data.Foldable as F
import           Data.Geometry
import qualified Data.List.NonEmpty as NonEmpty
import           Data.Maybe (mapMaybe)
import           Data.Measured.Class
import           Data.Measured.Size
import           Data.Proxy
import           Data.Semigroup
import qualified Data.Set as Set
import           GHC.TypeLits
import           Options.Applicative hiding ((<>))


import           Debug.Trace
--------------------------------------------------------------------------------


data Options = Options { _inPath    :: FilePath }
               deriving Data

options :: ParserInfo Options
options = info (helper <*> parser)
               (  progDesc "Compute expected pairwise distance of the points in the input file."
               <> header   "Expected Pairwise Distance"
               )
  where
    parser = Options
          <$> strOption (help "Input file"
                         <> short 'i'
                        )

--------------------------------------------------------------------------------

-- | Evaluates the formula: $\sum_{p,q \in pts} \|pq\|*Prb[sample has size k
-- and contains p and q]$ which solves to $\frac{{n-2 \choose k-2}}{{n \choose
-- k}} \sum_{p,q} \|pq\|$
--
-- running time: $O(n^2)$, where $n$ is the number of points
expectedPairwiseDistance       :: (Floating r, Arity d) => Int -> [Point d r :+ p] -> r
expectedPairwiseDistance k pts = makeExpected k pts pairwiseDist

-- | A $(1+\varepsilon)$-approximation of expectedPairwiseDistance
--
-- running time: $O(n(1/eps)^d + n\log n)$, where $n$ is the number of points
approxExpectedPairwiseDistance          :: (Floating r, Ord r
                                           , Arity d, Arity (d+1), 1 <= d
                                         , Show r, Show p)
                                         => r -> Int -> [Point d r :+ p] -> r
approxExpectedPairwiseDistance eps k pts =
  makeExpected k pts (approxPairwiseDistance eps)

--------------------------------------------------------------------------------
-- * Computing Distances

-- | Sum of the pairwise distances
pairwiseDist     :: (Floating r, Arity d) => [Point d r :+ p] -> r
pairwiseDist pts = sum [ euclideanDist (p^.core) (q^.core) | p <- pts, q <- pts] / 2


-- | $(1+\eps)$-approximation of the sum of the pairwise distances.
--
-- running time: $O(n(1/eps)^d + n\log n)$, where $n$ is the number of points
approxPairwiseDistance         :: (Floating r, Ord r, Arity d, Arity (d+1), 1 <= d
                                  , Show r, Show p)
                               => r -> [Point d r :+ p] -> r
approxPairwiseDistance _   []  = 0
approxPairwiseDistance eps pts =
    sum [ (size as)*(size bs)*euclideanDist (repr as) (repr bs) | (as,bs) <- pairs ]
  where
    t     = withSizes . fairSplitTree . NonEmpty.fromList $ pts
    pairs = wellSeparatedPairs (4 / eps) t

    size (access' -> (Sized (Size i) _))  = fromIntegral i
    repr (access' -> (Sized _ (First p))) = p^.core


-- wspPairs = fairSplitTree . NonEmpty.fromList

--------------------------------------------------------------------------------
-- * Helper stuff

-- | Helper to turn the result of 'f k' into 'the expected 'f k', assuming that
-- we select a set of k points.
makeExpected         :: (Fractional r, Foldable t) => Int -> t a -> (t a -> r) -> r
makeExpected k pts f = prb * f pts
  where
    n   = length pts
    prb = ((n - 2) `choose` (k - 2)) / (n `choose` k)


choose       :: (Integral a, Num b) => a -> a -> b
n `choose` k = fromIntegral $ fac n' `div` (fac (n'-k') * fac k')
  where
    n' :: Integer
    n' = fromIntegral n
    k' :: Integer
    k' = fromIntegral k

    fac z = product [1..z]

-- newtype WSPDMeasured a = WSPDMeasured a

-- instance Measured (Sized (First a)) (WSPDMeasured a) where
--   measure (WSPDMeasured p) = Sized 1 (First p)

-- instance Measured v (WSPDMeasured (Point d r :+ p))
--            => Measured v (SplitTree d p r v) where
--   measure (Leaf p)      = measure $ WSPDMeasured p
--   measure (Node _ nd _) = nd^.nodeData

-- | Annotate the split tree with sizes
withSizes :: SplitTree d p r a -> SplitTree d p r (Sized (First (Point d r :+ p)))
withSizes = foldUp f Leaf
  where
    f l (NodeData j b _) r = let nd = (access' l) <> (access' r)
                             in Node l (NodeData j b nd) r

-- | Get the measurement for a given splittree
access'               :: BinLeafTree (NodeData d r (Sized (First a))) a -> Sized (First a)
access' (Leaf x)      = Sized 1 (First x)
access' (Node _ nd _) = nd^.nodeData


--
-- | CVS file, in which every line consists of a name, followed by exactly d coordinates
parseInput :: forall d r. (Arity d, KnownNat d, Read r)
           => B.ByteString -> [Point d r :+ B.ByteString]
parseInput = mapMaybe toPoint . drop 1 . C.lines
  where
    trim      = fst . C.spanEnd isSpace . C.dropWhile isSpace
    fromList' = vectorFromList . take (fromInteger . natVal $ (Proxy :: Proxy d))

    toPoint bs = let (n:rs) = map trim . C.split ',' $ bs
                     p      = fmap Point . fromList' . map (read . C.unpack) $ rs
                 in (:+ n) <$> p


readInput :: (Arity d, KnownNat d, Read r) => FilePath -> IO [Point d r :+ C.ByteString]
readInput = fmap parseInput . B.readFile

test :: FilePath -> IO [Point 2 Double :+ C.ByteString]
test = readInput

testTree = fmap f .test
  where
    f pts = uncovered pts (4 / 0.05) (fairSplitTree $ NonEmpty.fromList pts)

-- compareBoth     :: r -> FilePath -> IO (r, r, Bool)
compareBoth eps = fmap f . test
  where
    f pts = let exact  = pairwiseDist pts
                approx = approxPairwiseDistance eps pts
            in (exact, approx, (1-eps)*exact <= approx && approx <= (1+eps)*exact)


compareBoth1 eps pts = let exact  = pairwiseDist pts
                           approx = approxPairwiseDistance eps pts
                       in (exact, approx, (1-eps)*exact <= approx && approx <= (1+eps)*exact)


mainWith (Options f) = compareBoth 0.05 f >>= print


--------------------------------------------------------------------------------
-- testing stuff


-- | Computes all pairs of points that are uncovered by the WSPD with separation s
uncovered         :: (Floating r, Ord r, Arity d, Arity (d+1), Ord p)
                  => [Point d r :+ p] -> r -> SplitTree d p r a -> [(Point d r :+ p, Point d r :+ p)]
uncovered pts s t = Set.toList $ allPairs `Set.difference` covered
  where
    allPairs = Set.fromList [ (p,q) | p <- pts, q <- pts, p < q ]
    covered  = Set.unions [ mkSet as bs | (as,bs) <- wellSeparatedPairs s t]

mkSet as bs = Set.fromList [ (min a b,max a b) | a <- F.toList as, b <- F.toList bs]

-- | Naively check if a WSP pair is actually well separated with respect to
-- separation s. i.e. computes the maximum diameter of as and bs, and then
-- tests by brute force if all pairs (a,b) from different sets are at distance
-- at least s times the maximum diameter.
isWellSeparated           :: (Floating r, Ord r, Arity d) => r -> WSP d p r a -> Bool
isWellSeparated s (as,bs) =
    and [ euclideanDist (a^.core) (b^.core) >= s*d | a <- F.toList as, b <- F.toList bs ]
  where
    d = (/2) . maximum . map (diameter . F.toList) $ [as,bs]


nonWellSeparated s = map (\(a,b,c) -> (a,b))
                   . filter (\(a,b,c) -> not c)
                   . map (\p@(a,b) -> (a,b,isWellSeparated s p))
                   . wellSeparatedPairs s . fairSplitTree . NonEmpty.fromList


points1 :: [Point 2 Double :+ ()]
points1 = ext <$> [Point2 0 0, Point2 1 1, Point2 2 10, Point2 3 11, Point2 5 5, Point2 10 0]

{-# LANGUAGE TemplateHaskell  #-}
{-# LANGUAGE ScopedTypeVariables  #-}
module Data.Geometry.Arrangement where

import Data.Geometry.Boundary
import Data.Semigroup
import Data.Util
import Control.Lens
import Data.Proxy
import Data.Maybe
import Data.Ext
import Data.Geometry.Point
import Data.Geometry.Vector
import Data.Geometry.Line
import Data.Geometry.Box
import Data.Geometry.Properties
import Data.Geometry.PlanarSubdivision
import Data.Geometry.LineSegment
import Data.Vinyl.CoRec
import qualified Data.Vector as V
import qualified Data.Map as Map
import qualified Data.Set as Set
import qualified Data.List as List
import qualified Data.List.NonEmpty as NonEmpty
import Data.List.NonEmpty(NonEmpty)

import Data.Geometry.Ipe
import Data.Geometry.PlanarSubdivision.Draw

--------------------------------------------------------------------------------

-- | Data type representing a two dimensional planar arrangement
data Arrangement s v e f r = Arrangement {
    _subdivision            :: PlanarSubdivision s v (Maybe e) f r
  , _boundedArea            :: Box 2 () r
  , _unboundedIntersections :: V.Vector (Point 2 r, Maybe (Line 2 r :+ e))
  } deriving (Show,Eq)
  -- unboundedIntersections also stores the corners of the box. They are not
  -- associated with any line
makeLenses ''Arrangement


--------------------------------------------------------------------------------

-- | Builds an arrangement of \(n\) lines
--
-- running time: \(O(n^2\log n\)
constructArrangement       :: (Ord r, Fractional r)
                           => proxy s
                           -> [Line 2 r :+ p]
                           -> Arrangement s () p () r
constructArrangement px ls = let b  = makeBoundingBox ls
                             in constructArrangementInBox px b ls

-- | Constructs the arrangemnet inside the box. (for parts to be useful, it is
-- assumed this box is the boundingbox of the intersections in the Arrangement)
constructArrangementInBox          :: (Ord r, Fractional r)
                                   => proxy s
                                   -> Box 2 () r
                                   -> [Line 2 r :+ p]
                                   -> Arrangement s () p () r
constructArrangementInBox px rect ls = Arrangement subdiv rect (V.fromList parts')
  where
    subdiv = fromConnectedSegments px segs
                & rawVertexData.traverse.dataVal .~ ()
    (segs,parts') = computeSegsAndParts rect ls

computeSegsAndParts         :: (Ord r, Fractional r)
                            => Box 2 () r
                            -> [Line 2 r :+ p]
                            -> ( [LineSegment 2 () r :+ Maybe p]
                               , [(Point 2 r, Maybe (Line 2 r :+ p))]
                               )
computeSegsAndParts rect ls = ( segs <> boundarySegs, parts')
  where
    segs         = map (&extra %~ Just)
                 . concatMap (\(l,ls') -> perLine rect l ls') $ makePairs ls
    boundarySegs = map (:+ Nothing) . toSegments . dupFirst $ map fst parts'
    dupFirst = \case []       -> []
                     xs@(x:_) -> xs ++ [x]
    parts'       = unBoundedParts rect ls



perLine       :: forall r p. (Ord r, Fractional r)
              => Box 2 () r -> Line 2 r :+ p -> [Line 2 r :+ p]
              -> [LineSegment 2 () r :+ p]
perLine b m ls = map (:+ m^.extra) . toSegments . List.sort $ vs <> vs'
  where
    vs  = mapMaybe (m `intersectionPoint`) ls
    vs' = maybe [] (\(p,q) -> [p,q]) . asA (Proxy :: Proxy (Point 2 r, Point 2 r))
        $ (m^.core) `intersect` (Boundary b)


intersectionPoint                   :: (Ord r, Fractional r)
                                    => Line 2 r :+ p -> Line 2 r :+ p -> Maybe (Point 2 r)
intersectionPoint (l :+ _) (m :+ _) = asA (Proxy :: Proxy (Point 2 r)) $ l `intersect` m


toSegments      :: Ord r => [Point 2 r] -> [LineSegment 2 () r]
toSegments ps = let pts = map ext $ ps in
  zipWith ClosedLineSegment pts (tail pts)


-- | Constructs a boundingbox containing all intersections
--
-- running time: \(O(n^2)\), where \(n\) is the number of input lines
makeBoundingBox :: (Ord r, Fractional r) => [Line 2 r :+ p] -> Box 2 () r
makeBoundingBox = grow 1 . boundingBoxList' . intersections

-- | Computes all intersections
intersections :: (Ord r, Fractional r) => [Line 2 r :+ p] -> [Point 2 r]
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
                       => [Line 2 r :+ p] -> LineSegment 2 q r
                       -> [(Point 2 r, Line 2 r :+ p)]
sideIntersections ls s = let l   = supportingLine s :+ undefined
                         in List.sortOn fst . filter (flip onSegment s . fst)
                          . mapMaybe (\m -> (,m) <$> l `intersectionPoint` m) $ ls

-- | Constructs the unbounded intersections
unBoundedParts         :: (Ord r, Fractional r)
                       => Box 2 () r
                       -> [Line 2 r :+ p]
                       -> [(Point 2 r, Maybe (Line 2 r :+ p))]
unBoundedParts rect ls = [tl] <> t <> [tr] <> reverse r <> [br] <> reverse b <> [bl] <> l
  where
    sideIntersections' = over (traverse._2) Just . sideIntersections ls
    (t,r,b,l)     = map' sideIntersections'      $ sides   rect
    (tl,tr,br,bl) = map' ((,Nothing) . (^.core)) $ corners rect

    map' f (a,b',c,d) = (f a, f b', f c, f d)

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


--------------------------------------------------------------------------------

data Test = Test

test = do
         let fp = "test/Data/Geometry/arrangement.ipe"
             outFile = "/tmp/out.ipe"
         Right ls <- fmap f <$> readSinglePageFile fp
         print ls
         let arr = constructArrangement (Identity Test) ls
             (segs,parts') = computeSegsAndParts (arr^.boundedArea) ls
             out = [ asIpe drawPlanarSubdivision (arr^.subdivision) ]
         mapM_ print segs
         writeIpeFile outFile . singlePageFromContent $ out
  where
    f     :: IpePage Rational -> [Line 2 Rational :+ ()]
    f page = [ ext $ supportingLine s
             | (s :+ ats) <- segs
             ]
      where
        segs = page^..content.traverse._withAttrs _IpePath _asLineSegment

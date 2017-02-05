{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE ScopedTypeVariables #-}
module Algorithms.Geometry.PolygonTriangulation.MakeMonotone where

import qualified Algorithms.Geometry.LineSegmentIntersection.BentleyOttmann as BO
import           Control.Lens
import           Control.Monad (forM_, when)
import           Control.Monad.Reader
import           Control.Monad.State.Strict
import           Control.Monad.Writer(WriterT, execWriterT,tell)
import qualified Data.BalBST as SS
import           Data.Bifunctor
import           Data.CircularSeq (rotateL, rotateR, zip3LWith)
import           Data.Ext
import qualified Data.Foldable as F
import           Data.Functor.Contravariant
import           Data.Geometry.LineSegment
import           Data.Geometry.PlanarSubdivision
import           Data.Geometry.Point
import           Data.Geometry.Polygon
import qualified Data.IntMap as IntMap
import qualified Data.List.NonEmpty as NonEmpty
import           Data.Maybe (catMaybes)
import           Data.Ord (comparing, Down(..))
import           Data.Semigroup
import           Data.Util
import qualified Data.Vector as V
import qualified Data.Vector.Mutable as MV
import qualified Data.DList as DList

import Debug.Trace

--------------------------------------------------------------------------------

data VertexType = Start | Merge | Split | End | Regular deriving (Show,Read,Eq)


-- | assigns a vertex type to each vertex
--
-- pre: the polygon is given in CCW order
--
-- running time: \(O(n)\).
classifyVertices                   :: (Num r, Ord r)
                                    => SimplePolygon p r
                                    -> SimplePolygon (p :+ VertexType) r
classifyVertices (SimplePolygon vs) =
    SimplePolygon $ zip3LWith f (rotateL vs) vs (rotateR vs)
  where
    -- is the angle larger than > 180 degrees
    largeInteriorAngle p c n = case ccw (p^.core) (c^.core) (n^.core) of
           CCW -> False
           CW  -> True
           _   -> error "classifyVertices -> largeInteriorAngle: colinear points"

    f p c n = c&extra %~ (:+ vt)
      where
        vt = case (p `cmpSweep` c, n `cmpSweep` c, largeInteriorAngle p c n) of
               (LT, LT, False) -> Start
               (LT, LT, True)  -> Split
               (GT, GT, False) -> End
               (GT, GT, True)  -> Merge
               _               -> Regular

-- | p < q = p.y < q.y || p.y == q.y && p.x > q.y
cmpSweep :: Ord r => Point 2 r :+ e -> Point 2 r :+ e -> Ordering
p `cmpSweep` q =
  comparing (^.core.yCoord) p q <> comparing (Down . (^.core.xCoord)) p q


--------------------------------------------------------------------------------

-- fromAdjacencyList

makeMonotone      :: SimplePolygon p r -> proxy s -> PlanarSubdivision s p () Bool r
makeMonotone pg _ = undefined

ordAtNav :: (Fractional r, Ord r, Show r) => r -> SS.TreeNavigator r (LineSegment 2 p r)
ordAtNav r = traceShow ("ordAtNav",r) $ BO.ordAtNav r

-- | The
withIncidentEdges                    :: SimplePolygon p r
                                     -> SimplePolygon (Two (LineSegment 2 p r)) r
withIncidentEdges (SimplePolygon vs) =
    SimplePolygon $ zip3LWith f (rotateL vs) vs (rotateR vs)
  where
    f p c n = c&extra .~ SP (ClosedLineSegment p c) (ClosedLineSegment c n)


type Event r = Point 2 r :+ (Two (LineSegment 2 Int r))

data StatusStruct r = SS { _statusStruct :: !(SS.BalBST r (LineSegment 2 Int r))
                         , _helper       :: !(IntMap.IntMap Int)
                         -- ^ for every e_i, the id of the helper vertex
                         } deriving (Show)
makeLenses ''StatusStruct


ix' i = traceShow i $ singular (ix i)

findDiagonals    :: forall r p. (Fractional r, Ord r,          Show r, Show p)
                 => SimplePolygon p r -> [LineSegment 2 p r]
findDiagonals p' = map f . sweep
                 . NonEmpty.sortBy (flip cmpSweep)
                 . polygonVertices . withIncidentEdges
                 . first (^._1) $ p
  where
    -- remaps to get the p value rather than the vertexId
    f = first (\i -> vertexInfo^.ix' i._2)

    p :: SimplePolygon (SP Int (p :+ VertexType)) r
    p = numberVertices . classifyVertices $ p'
    vertexInfo :: V.Vector (STR (Point 2 r) p VertexType)
    vertexInfo = let vs = polygonVertices p
                     n  = F.length vs
                 in V.create $ do
                   v <- MV.new n
                   forM_ vs $ \(pt :+ SP i (p :+ vt)) ->
                     MV.write v i (STR pt p vt)
                   return v

    initialSS = SS (SS.empty $ BO.ordAtNav undefined) mempty

    -- sweep :: NonEmpty.NonEmpty (Event r) -> SP [LineSegment 2 Int r] (StatusStruct r)
    -- sweep = foldr (handle vertexInfo) (SP [] initialSS)

    sweep  xs = flip runReader vertexInfo $ evalStateT (sweep'' xs) initialSS
    sweep'' xs = DList.toList <$> execWriterT (sweep' xs)

    sweep' :: NonEmpty.NonEmpty (Event r) -> Sweep p r ()
    sweep' = mapM_ handle

type Sweep p r = WriterT (DList.DList (LineSegment 2 Int r))
                   (StateT (StatusStruct r)
                     (Reader (V.Vector (VertexInfo p r))))

type VertexInfo p r = STR (Point 2 r) p VertexType


-- type Event r = Point 2 r :+ (Two (LineSegment 2 Int r))

tell' :: LineSegment 2 Int r -> Sweep p r ()
tell' = tell . DList.singleton

getIdx :: Event r -> Int
getIdx = view (extra._1.end.extra)

getVertexType   :: Int -> Sweep p r VertexType
getVertexType v = asks (^.ix' v._3)

getEventType :: Event r -> Sweep p r VertexType
getEventType = getVertexType . getIdx

handle   :: (Fractional r, Ord r, Show r, Show p) => Event r -> Sweep p r ()
handle e | traceShow (getIdx e, e) False = undefined
handle e = let i = getIdx e in getEventType e >>= \case
    Start   -> handleStart   i e
    End     -> handleEnd     i e
    Split   -> handleSplit   i e
    Merge   -> handleMerge   i e
    Regular | isLeftVertex i e -> handleRegularL i e
            | otherwise        -> handleRegularR i e


insertAt       :: (Ord r, Fractional r, Show r, Show q) => Point 2 r -> LineSegment 2 q r
               -> SS.BalBST r (LineSegment 2 q r) -> SS.BalBST r (LineSegment 2 q r)
insertAt v e t | traceShow ("InsertAt", v, e) False = undefined
               | otherwise  = SS.insert e $ t { SS.nav = ordAtNav (v^.yCoord) }

deleteAt v e t = SS.delete e $ t { SS.nav = ordAtNav (v^.yCoord) }


-- handleStart   :: (Fractional r, Ord r, Show r) => Int -> Event r -> Sweep p r ()
handleStart i (v :+ adj) = modify $ \(SS t h) ->
                                SS (insertAt v (adj^._2) t)
                                   (IntMap.insert i i h)

-- handleEnd   :: (Fractional r, Ord r, Show r) => Int -> Event r -> Sweep p r ()
handleEnd i (v :+ adj) = do let iPred = adj^._1.start.extra  -- i-1
                            -- lookup p's helper; if it is a merge vertex
                            -- we insert a new segment
                            tellIfMerge i v iPred
                            -- delete e_{i-1} from the status struct
                            modify $ \ss -> ss&statusStruct %~ deleteAt v (adj^._1)

-- | Adds edge (i,j) if e_j's helper is a merge vertex
tellIfMerge i v j = do SP u ut' <- getHelper j
                       let ut = traceShow ("tellIfMerge", ut') ut'
                       when (ut == Merge) (tell' $ ClosedLineSegment (v :+ i) u)

-- | Get the helper of edge i, and its vertex type
getHelper   :: Int -> Sweep p r (SP (Point 2 r :+ Int) VertexType)
getHelper i | traceShow ("HelperOf ", i) False = undefined
getHelper i = do Just ui    <- gets (^.helper.at (traceShowId i))
                 STR u _ ut <- asks (^.ix' (traceShowId ui ))
                 pure $ SP (u :+ ui) ut



lookupLE     :: (Ord r, Fractional r , Show r) => Point 2 r
             -> SS.BalBST r (LineSegment 2 Int r)
             -> Maybe (LineSegment 2 Int r)
lookupLE v t = SS.lookupLE (v^.yCoord) (t { SS.nav = ordAtNav (v^.yCoord) })


handleSplit i (v :+ adj) = do Just ej <- gets $ \ss -> ss^.statusStruct.to (lookupLE v)
                              let j = ej^.start.extra
                              SP u _ <- getHelper j
                              -- update the status struct:
                              -- insert the new edge into the status Struct and
                              -- set the helper of e_j to be v_i
                              modify $ \(SS t h) ->
                                SS (insertAt v (adj^._2) t)
                                   (IntMap.insert i i . IntMap.insert j i $ h)
                              -- return the diagonal
                              tell' $ ClosedLineSegment (v :+ i) u

handleMerge i e | traceShow ("Merge", i, e) False = undefined
handleMerge i (v :+ adj) = do let ePred' = adj^._1.start.extra -- i-1
                              st <- get
                              let ePred = traceShow ("ST") ePred'
                              tellIfMerge i v ePred
                              -- delete e_{i-1} from the status struct
                              modify $ \ss -> ss&statusStruct %~ deleteAt v (adj^._1)
                              connectToLeft i v

-- | finds the edge j to the left of v_i, and connect v_i to it if the helper
-- of j is a merge vertex
connectToLeft i v = do Just ej <- gets $ \ss -> ss^.statusStruct.to (lookupLE v)
                       let j = ej^.start.extra
                       tellIfMerge i v j
                       modify $ \ss -> ss&helper %~ IntMap.insert j i


-- | returns True if v the interior of the polygon is to the right of v
isLeftVertex i (v :+ adj) = case (adj^._1.start) `cmpSweep` (v :+ i) of
                              GT -> True
                              _  -> False
  -- if the predecessor occurs before the sweep, this must be a left vertex

handleRegularL i (v :+ adj) = do let ePred = adj^._1.start.extra -- i-1
                                 tellIfMerge i v ePred
                                 -- delete e_{i-1} from the status struct
                                 modify $ \ss -> ss&statusStruct %~ deleteAt v (adj^._1)
                                 -- insert a e_i in the status struct, and set its helper
                                 -- to be v_i
                                 modify $ \(SS t h) ->
                                     SS (insertAt v (adj^._2) t)
                                        (IntMap.insert i i h)

handleRegularR i (v :+ _) = connectToLeft i v




--------------------------------------------------------------------------------


testPoly = fromPoints . map ext $ [ origin
                                  , point2 0 (-1)
                                  , point2 0 (-5)
                                  , point2 1 (-5)
                                  , point2 5 (-1)
                                  , point2 7 (-4)
                                  , point2 9 1
                                  , point2 6 4
                                  , point2 3 2
                                  , point2 1 5
                                  ]
testAnswer = [Regular, Regular, Regular, End, Split, End, Regular, Start, Merge, Start]


testP = point2 7 2
testQ = point2 3 2

-- (Point2 px py :+ _) `pntOrd` (Point2 qx qy :+ ) =
--  comparing


--     case (py `compare` qy, px `compare` qx) of
--       (LT,_)  -> LT
--       (EQ,GT) -> LT
--       _       -> False





-- (Point2 px py :+ _) `isBelow` (Point2 qx qy :+ ) =
--     case (py `compare` qy, px `compare` qx) of
--       (LT,_)  -> True
--       (EQ,GT) -> True
--       _       -> False


-- (Point2 px py :+ ) `isAbove` (Point2 qx qy :+ ) =
--     case (py `compare` qy, px `compare` qx) of
--       (GT,_)  -> True
--       (EQ,LT) -> True
--       _       -> False


testPolygon :: SimplePolygon Int Rational
testPolygon = fromPoints [ point2 20 20 :+ 1
                         , point2 18 19 :+ 2
                         , point2 16 25 :+ 3
                         , point2 13 23 :+ 4
                         , point2 10 24 :+ 5
                         , point2 6  22 :+ 6
                         , point2 8  21 :+ 7
                         , point2 7  18 :+ 8
                         , point2 2  19 :+ 9
                         , point2 1  10 :+ 10
                         , point2 3  5  :+ 11
                         , point2 11 7  :+ 12
                         , point2 15 1  :+ 13
                         , point2 12 15 :+ 14
                         , point2 15 12 :+ 15
                         ]

vertexTypes = [Start,Merge,Start,Merge,Start,Regular,Regular,Merge,Start,Regular,End,Split,End,Split,End]

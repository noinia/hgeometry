{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE ScopedTypeVariables #-}
module Algorithms.Geometry.PolygonTriangulation.MakeMonotoneOld where

import           Algorithms.Geometry.LineSegmentIntersection.BentleyOttmann ( xCoordAt
                                                                            , ordAt)
import           Algorithms.Geometry.PolygonTriangulation.Types
import           Control.Lens
import           Control.Monad (forM_, when)
import           Control.Monad.Reader
import           Control.Monad.State.Strict
import           Control.Monad.Writer (WriterT, execWriterT,tell)
import           Data.Bifunctor
import           Data.CircularSeq (rotateL, rotateR, zip3LWith)
import qualified Data.DList as DList
import           Data.Ext
import qualified Data.Foldable as F
import           Data.Geometry.LineSegment
import           Data.Geometry.PlanarSubdivision.Basic
import           Data.Geometry.Point
import           Data.Geometry.Polygon
import qualified Data.IntMap as IntMap
import qualified Data.List.NonEmpty as NonEmpty
import           Data.Ord (comparing, Down(..))
import           Data.OrdSeq (OrdSeq)
import qualified Data.OrdSeq as SS
import           Data.Util
import qualified Data.Vector as V
import qualified Data.Vector.Mutable as MV


----------------------------------------------------------------------------------

data VertexType = Start | Merge | Split | End | Regular deriving (Show,Read,Eq)


-- How about the hole vertices?

-- | assigns a vertex type to each vertex
--
-- pre: the polygon is given in CCW order
--
-- running time: \(O(n)\).
classifyVertices                     :: (Num r, Ord r)
                                     => Polygon t p r
                                     -> Polygon t (p :+ VertexType) r
classifyVertices p@(SimplePolygon _) = classifyVertices' p
classifyVertices (MultiPolygon vs h) = MultiPolygon vs' h'
  where
    (SimplePolygon vs') = classifyVertices' $ SimplePolygon vs
    h' = map (first (&extra %~ onHole) . classifyVertices') h

    -- the roles on hole vertices are slightly different
    onHole Start   = Split
    onHole Merge   = End
    onHole Split   = Start
    onHole End     = Merge
    onHole Regular = Regular

-- | assigns a vertex type to each vertex
--
-- pre: the polygon is given in CCW order
--
-- running time: \(O(n)\).
classifyVertices'                    :: (Num r, Ord r)
                                     => SimplePolygon p r
                                     -> SimplePolygon (p :+ VertexType) r
classifyVertices' (SimplePolygon vs) =
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

type Event r = Point 2 r :+ (Two (LineSegment 2 Int r))

data StatusStruct r = SS { _statusStruct :: !(SS.OrdSeq (LineSegment 2 Int r))
                         , _helper       :: !(IntMap.IntMap Int)
                         -- ^ for every e_i, the id of the helper vertex
                         } deriving (Show)
makeLenses ''StatusStruct

ix'   :: Int -> Lens' (V.Vector a) a
ix' i = singular (ix i)

-- | Given a polygon, find a set of non-intersecting diagonals that partition
-- the polygon into y-monotone pieces.
--
-- running time: \(O(n\log n)\)
computeDiagonals    :: forall t r p. (Fractional r, Ord r)
                    => Polygon t p r -> [LineSegment 2 p r]
computeDiagonals p' = map f . sweep
                    . NonEmpty.sortBy (flip cmpSweep)
                    . polygonVertices . withIncidentEdges
                    . first (^._1) $ pg
  where
    -- remaps to get the p value rather than the vertexId
    f = first (\i -> vertexInfo^.ix' i._2)

    pg :: Polygon t (SP Int (p :+ VertexType)) r
    pg = numberVertices . classifyVertices . toCounterClockWiseOrder $ p'
    vertexInfo :: V.Vector (STR (Point 2 r) p VertexType)
    vertexInfo = let vs = polygonVertices pg
                     n  = F.length vs
                 in V.create $ do
                   v <- MV.new n
                   forM_ vs $ \(pt :+ SP i (p :+ vt)) ->
                     MV.write v i (STR pt p vt)
                   return v

    initialSS = SS mempty mempty

    sweep  es = flip runReader vertexInfo $ evalStateT (sweep' es) initialSS
    sweep' es = DList.toList <$> execWriterT (sweep'' es)

    sweep'' :: NonEmpty.NonEmpty (Event r) -> Sweep p r ()
    sweep'' = mapM_ handle

-- | Computes a set of diagionals that decompose the polygon into y-monotone
-- pieces.
--
-- running time: \(O(n\log n)\)
makeMonotone      :: (Fractional r, Ord r)
                  => proxy s -> Polygon t p r
                  -> PlanarSubdivision s p PolygonEdgeType PolygonFaceData r
makeMonotone px pg = let (e:es) = listEdges pg
                     in constructSubdivision px e es (computeDiagonals pg)

type Sweep p r = WriterT (DList.DList (LineSegment 2 Int r))
                   (StateT (StatusStruct r)
                     (Reader (V.Vector (VertexInfo p r))))

type VertexInfo p r = STR (Point 2 r) p VertexType


tell' :: LineSegment 2 Int r -> Sweep p r ()
tell' = tell . DList.singleton

getIdx :: Event r -> Int
getIdx = view (extra._1.end.extra)

getVertexType   :: Int -> Sweep p r VertexType
getVertexType v = asks (^.ix' v._3)

getEventType :: Event r -> Sweep p r VertexType
getEventType = getVertexType . getIdx

handle   :: (Fractional r, Ord r) => Event r -> Sweep p r ()
handle e = let i = getIdx e in getEventType e >>= \case
    Start   -> handleStart   i e
    End     -> handleEnd     i e
    Split   -> handleSplit   i e
    Merge   -> handleMerge   i e
    Regular | isLeftVertex i e -> handleRegularL i e
            | otherwise        -> handleRegularR i e


insertAt   :: (Ord r, Fractional r) => Point 2 r -> LineSegment 2 q r
           -> OrdSeq (LineSegment 2 q r) -> OrdSeq (LineSegment 2 q r)
insertAt v = SS.insertBy (ordAt $ v^.yCoord)

deleteAt   :: (Fractional r, Ord r) => Point 2 r -> LineSegment 2 p r
           -> OrdSeq (LineSegment 2 p r) -> OrdSeq (LineSegment 2 p r)
deleteAt v = SS.deleteAllBy (ordAt $ v^.yCoord)


handleStart              :: (Fractional r, Ord r)
                         => Int -> Event r -> Sweep p r ()
handleStart i (v :+ adj) = modify $ \(SS t h) ->
                                SS (insertAt v (adj^._2) t)
                                   (IntMap.insert i i h)

handleEnd              :: (Fractional r, Ord r)
                       => Int -> Event r -> Sweep p r ()
handleEnd i (v :+ adj) = do let iPred = adj^._1.start.extra  -- i-1
                            -- lookup p's helper; if it is a merge vertex
                            -- we insert a new segment
                            tellIfMerge i v iPred
                            -- delete e_{i-1} from the status struct
                            modify $ \ss ->
                              ss&statusStruct %~ deleteAt v (adj^._1)

-- | Adds edge (i,j) if e_j's helper is a merge vertex
tellIfMerge       :: Int -> Point 2 r -> Int -> Sweep p r ()
tellIfMerge i v j = do SP u ut <- getHelper j
                       when (ut == Merge) (tell' $ ClosedLineSegment (v :+ i) u)

-- | Get the helper of edge i, and its vertex type
getHelper   :: Int -> Sweep p r (SP (Point 2 r :+ Int) VertexType)
getHelper i = do ui         <- gets (^?!helper.ix i)
                 STR u _ ut <- asks (^.ix' ui)
                 pure $ SP (u :+ ui) ut


lookupLE     :: (Ord r, Fractional r)
             => Point 2 r -> OrdSeq (LineSegment 2 Int r)
             -> Maybe (LineSegment 2 Int r)
lookupLE v s = let (l,m,_) = SS.splitOn (xCoordAt $ v^.yCoord) (v^.xCoord) s
               in SS.lookupMax (l <> m)


handleSplit              :: (Fractional r, Ord r) => Int -> Event r -> Sweep p r ()
handleSplit i (v :+ adj) = do ej <- gets $ \ss -> ss^?!statusStruct.to (lookupLE v)._Just
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

handleMerge              :: (Fractional r, Ord r) => Int -> Event r -> Sweep p r ()
handleMerge i (v :+ adj) = do let ePred = adj^._1.start.extra -- i-1
                              tellIfMerge i v ePred
                              -- delete e_{i-1} from the status struct
                              modify $ \ss -> ss&statusStruct %~ deleteAt v (adj^._1)
                              connectToLeft i v

-- | finds the edge j to the left of v_i, and connect v_i to it if the helper
-- of j is a merge vertex
connectToLeft     :: (Fractional r, Ord r) => Int -> Point 2 r -> Sweep p r ()
connectToLeft i v = do ej <- gets $ \ss -> ss^?!statusStruct.to (lookupLE v)._Just
                       let j = ej^.start.extra
                       tellIfMerge i v j
                       modify $ \ss -> ss&helper %~ IntMap.insert j i

-- | returns True if v the interior of the polygon is to the right of v
isLeftVertex              :: Ord r => Int -> Event r -> Bool
isLeftVertex i (v :+ adj) = case (adj^._1.start) `cmpSweep` (v :+ i) of
                              GT -> True
                              _  -> False
  -- if the predecessor occurs before the sweep, this must be a left vertex

handleRegularL              :: (Fractional r, Ord r) => Int -> Event r -> Sweep p r ()
handleRegularL i (v :+ adj) = do let ePred = adj^._1.start.extra -- i-1
                                 tellIfMerge i v ePred
                                 -- delete e_{i-1} from the status struct
                                 modify $ \ss ->
                                   ss&statusStruct %~ deleteAt v (adj^._1)
                                 -- insert a e_i in the status struct, and set its helper
                                 -- to be v_i
                                 modify $ \(SS t h) ->
                                     SS (insertAt v (adj^._2) t)
                                        (IntMap.insert i i h)

handleRegularR            :: (Fractional r, Ord r) => Int -> Event r -> Sweep p r ()
handleRegularR i (v :+ _) = connectToLeft i v




--------------------------------------------------------------------------------


-- testPolygon :: SimplePolygon Int Rational
-- testPolygon = fromPoints [ Point2 20 20 :+ 1
--                          , Point2 18 19 :+ 2
--                          , Point2 16 25 :+ 3
--                          , Point2 13 23 :+ 4
--                          , Point2 10 24 :+ 5
--                          , Point2 6  22 :+ 6
--                          , Point2 8  21 :+ 7
--                          , Point2 7  18 :+ 8
--                          , Point2 2  19 :+ 9
--                          , Point2 1  10 :+ 10
--                          , Point2 3  5  :+ 11
--                          , Point2 11 7  :+ 12
--                          , Point2 15 1  :+ 13
--                          , Point2 12 15 :+ 14
--                          , Point2 15 12 :+ 15
--                          ]

-- vertexTypes = [Start,Merge,Start,Merge,Start,Regular,Regular,Merge,Start,Regular,End,Split,End,Split,End]


-- loadT = do pgs <- readAllFrom "/Users/frank/tmp/testPoly.ipe"
--                         :: IO [SimplePolygon () Rational :+ IpeAttributes Path Rational]
--            mapM_ print pgs
--            let diags = map (computeDiagonals . (^.core)) pgs
--                f = asIpeGroup . map (asIpeObject' mempty)
--                out = [ asIpeGroup $ map (\(pg :+ a) -> asIpeObject pg a) pgs
--                      , asIpeGroup $ map f diags
--                      ]
--                outFile = "/Users/frank/tmp/out.ipe"
--            writeIpeFile outFile . singlePageFromContent $ out

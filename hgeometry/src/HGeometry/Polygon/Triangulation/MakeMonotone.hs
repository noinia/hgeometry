{-# LANGUAGE ScopedTypeVariables #-}
--------------------------------------------------------------------------------
-- |
-- Module      :  HGeometry.Polygon.Triangulation.MakeMonotone
-- Copyright   :  (C) Frank Staals
-- License     :  see the LICENSE file
-- Maintainer  :  Frank Staals
--------------------------------------------------------------------------------
module HGeometry.Polygon.Triangulation.MakeMonotone
  (

    -- makeMonotone
    computeDiagonals


  , VertexType(..)
  , classifyVertices'
  ) where

import           Control.Lens
import           Control.Monad (forM_, when)
import           Control.Monad.Reader
import           Control.Monad.State.Strict
import           Control.Monad.Writer (WriterT, execWriterT, tell)
import           Data.Bifunctor
import qualified Data.DList as DList
import qualified Data.Foldable as F
import qualified Data.IntMap as IntMap
import qualified Data.List.NonEmpty as NonEmpty
import           Data.Ord (Down (..), comparing)
import qualified Data.Set as SS
import qualified Data.Vector as V
import           HGeometry.Combinatorial.Util
import qualified HGeometry.Set.Util as SS
-- import qualified Data.Vector.Circular as CV
import qualified Data.Vector.Mutable as MV
import           HGeometry.Ext
import           HGeometry.LineSegment
-- import           HGeometry.PlanarSubdivision.Basic
import           HGeometry.Point
import           HGeometry.Polygon.Class
import           HGeometry.Polygon.Simple.Class
import           HGeometry.Polygon.Triangulation.Types

----------------------------------------------------------------------------------

data VertexType = Start | Merge | Split | End | Regular deriving (Show,Read,Eq)

{-

-- | assigns a vertex type to each vertex
--
-- pre: Both the outer boundary and the inner boundary of the polygon are given in CCW order.
--
-- running time: \(O(n)\).
classifyVertices                     :: ( Polygon_ polygon point r
                                        , Polygon_ polygon' (point :+ VertexType) r
                                        , Num r, Ord r)
                                     => polygon
                                     -> polygon'
classifyVertices p@SimplePolygon{}   = classifyVertices' p
classifyVertices (MultiPolygon vs h) = MultiPolygon vs' h'
  where
    vs' = classifyVertices' vs
    h' = map (first (&extra %~ onHole) . classifyVertices') h

    -- the roles on hole vertices are slightly different
    onHole Start   = Split
    onHole Merge   = End
    onHole Split   = Start
    onHole End     = Merge
    onHole Regular = Regular

-}

-- pre: the polygon is given in CCW order

-- | assigns a vertex type to each vertex
--
-- running time: \(O(n)\).
classifyVertices'      :: ( SimplePolygon_ simplePolygon point r
                          , SimplePolygon_ simplePolygon' (point :+ VertexType) r
                          , Num r, Ord r
                          )
                       => simplePolygon -> simplePolygon'
classifyVertices' poly =
    uncheckedFromCCWPoints . map f $ poly^..outerBoundaryWithNeighbours
    -- unsafeFromCircularVector $ CV.zipWith3 f (CV.rotateLeft 1 vs) vs (CV.rotateRight 1 vs)
  where
    -- vs = poly ^. outerBoundaryVector
    -- is the angle larger than > 180 degrees
    largeInteriorAngle p c n = case ccw p c n of
           CCW -> False
           CW  -> True
           _   -> error "classifyVertices -> largeInteriorAngle: colinear points"

    f (c,(p,n)) = c :+ vt
      where
        vt = case (p `cmpSweep` c, n `cmpSweep` c, largeInteriorAngle p c n) of
               (LT, LT, False) -> Start
               (LT, LT, True)  -> Split
               (GT, GT, False) -> End
               (GT, GT, True)  -> Merge
               _               -> Regular

-- | p < q = p.y < q.y || p.y == q.y && p.x > q.y
cmpSweep :: (Point_ point 2 r, Ord r) => point :+ e -> point :+ e -> Ordering
p `cmpSweep` q = comparing (^.core.yCoord) p q <> comparing (Down . (^.core.xCoord)) p q


--------------------------------------------------------------------------------

type Event point = point :+ Two (ClosedLineSegment (point :+ Int))

data StatusStruct point =
  SS { _statusStruct :: !(SS.Set (ClosedLineSegment (point :+ Int)))
     , _helper       :: !(IntMap.IntMap Int)
                        -- ^ for every e_i, the id of the helper vertex
     } deriving (Show)

-- | Lens to access te statusStructure
statusStruct :: Lens (StatusStruct point) (StatusStruct point')
                     (SS.Set (ClosedLineSegment (point :+ Int)))
                     (SS.Set (ClosedLineSegment (point' :+ Int)))
statusStruct = lens _statusStruct (\ss s -> ss { _statusStruct = s})

-- | Lens to access the helper
helper :: Lens' (StatusStruct point) (IntMap.IntMap Int)
helper = lens _helper (\ss h -> ss { _helper = h })

-- | helper to access the i^th element of a vector.
ix'   :: Int -> Lens' (V.Vector a) a
ix' i = singular (ix i)

{- HLINT ignore computeDiagonals -}
-- | Given a polygon, find a set of non-intersecting diagonals that partition
-- the polygon into y-monotone pieces.
--
-- running time: \(O(n\log n)\)
computeDiagonals    :: (SimplePolygon_ polygon point r, VertexIx polygon ~ Int, Ord r, Num r)
                    => polygon -> [ClosedLineSegment (point :+ Int)]
computeDiagonals p' = map f . sweep
                    . NonEmpty.sortBy (flip cmpSweep)
                    . polygonVertices . withIncidentEdges
                    . first (^._1) $ pg
  where
    -- remaps to get the p value rather than the vertexId
    f = first (\i -> vertexInfo^.ix' i._2)

    -- pg :: Polygon t (SP Int (p :+ VertexType)) r
    -- pg = numberVertices . holesToCW . classifyVertices . toCCW $ p'

    pg = classifyVertices' $ p'

    vertexInfo :: V.Vector (point :+ VertexType)
    vertexInfo = vertices pg
      -- let n  = numVertices pg
      --            in V.create $ do
      --              v <- MV.new n
      --              iforM_ vertices pg $ \i (pt :+ SP i (p :+ vt)) ->
      --                MV.write v i (pt :+ vt)
      --              return v

    initialSS = SS SS.empty mempty

    sweep  es = flip runReader vertexInfo $ evalStateT (sweep' es) initialSS
    sweep' es = DList.toList <$> execWriterT (sweep'' es)

--    sweep'' :: NonEmpty.NonEmpty (Event point) -> Sweep point ()
    sweep'' = mapM_ handle

    -- holesToCW p = p&polygonHoles'.traverse %~ toClockwiseOrder'

{-

-- | Computes a set of diagionals that decompose the polygon into y-monotone
-- pieces.
--
-- pre: the polygon boundary is given in counterClockwise order.
--
-- running time: \(O(n\log n)\)
makeMonotone    :: forall s t p r. (Fractional r, Ord r)
                => Polygon t p r
                -> PlanarSubdivision s p PolygonEdgeType PolygonFaceData r
makeMonotone pg = let (e:es) = listEdges pg
                  in constructSubdivision e es (computeDiagonals pg)

-}

type Sweep point =
  WriterT (DList.DList (ClosedLineSegment (point :+ Int)))
  (StateT (StatusStruct point)
                  (Reader (V.Vector (VertexInfo point))))

type VertexInfo point = point :+ VertexType

  -- STR (Point 2 r) p VertexType


tell' :: ClosedLineSegment (point :+ Int) -> Sweep point ()
tell' = tell . DList.singleton

getIdx :: Event point -> Int
getIdx = view (extra._1.end.extra)

getVertexType   :: Int -> Sweep point VertexType
getVertexType v = asks (^.ix' v.extra)

getEventType :: Event point -> Sweep point VertexType
getEventType = getVertexType . getIdx

handle   :: (Point_ point 2 r, Num r, Ord r) => Event point -> Sweep point ()
handle e = let i = getIdx e in getEventType e >>= \case
    Start   -> handleStart   i e
    End     -> handleEnd     i e
    Split   -> handleSplit   i e
    Merge   -> handleMerge   i e
    Regular | isLeftVertex i e -> handleRegularL i e
            | otherwise        -> handleRegularR i e

--FIXME: I think we can drop the fractional constraints :), and replace them by Num instead, since ordAtY now only uses a Num constraint :)
insertAt   :: (Point_ point 2 r, Ord r, Num r) => point -> ClosedLineSegment point
           -> SS.Set (ClosedLineSegment point) -> SS.Set (ClosedLineSegment point)
insertAt v = SS.insertBy (ordAtY $ v^.yCoord)

deleteAt   :: (Point_ point 2 r, Num r, Ord r) => point -> ClosedLineSegment point
           -> SS.Set (ClosedLineSegment point) -> SS.Set (ClosedLineSegment point)
deleteAt v = SS.deleteAllBy (ordAtY $ v^.yCoord)


handleStart              :: (Point_ point 2 r, Num r, Ord r)
                         => Int -> Event point -> Sweep point ()
handleStart i (v :+ adj) = modify $ \(SS t h) ->
                                SS (insertAt v (adj^._2) t)
                                   (IntMap.insert i i h)

handleEnd              :: (Point_ point 2 r, Num r, Ord r)
                       => Int -> Event point -> Sweep point ()
handleEnd i (v :+ adj) = do let iPred = adj^._1.start.extra  -- i-1
                            -- lookup p's helper; if it is a merge vertex
                            -- we insert a new segment
                            tellIfMerge i v iPred
                            -- delete e_{i-1} from the status struct
                            modify $ \ss ->
                              ss&statusStruct %~ deleteAt v (adj^._1)

-- | Adds edge (i,j) if e_j's helper is a merge vertex
tellIfMerge       :: (Point_ point 2 r) => Int -> point -> Int -> Sweep point ()
tellIfMerge i v j = do SP u ut <- getHelper j
                       when (ut == Merge) (tell' $ ClosedLineSegment (v :+ i) u)

-- | Get the helper of edge i, and its vertex type
getHelper   :: (Point_ point 2 r) => Int -> Sweep point (SP (point  :+ Int) VertexType)
getHelper i = do ui      <- gets (^?!helper.ix i)
                 u :+ ut <- asks (^.ix' ui)
                 pure $ SP (u :+ ui) ut


lookupLE     :: (Point_ point 2 r, Ord r, Num r)
             => point -> SS.Set (ClosedLineSegment (point :+ Int))
             -> Maybe (ClosedLineSegment (point :+ Int))
lookupLE v s = let (l,m,_) = SS.splitOn (xCoordAt $ v^.yCoord) (v^.xCoord) s
               in SS.lookupMax (l `SS.join` m)


handleSplit              :: (Point_ point 2 r, Num r, Ord r) => Int -> Event point -> Sweep point ()
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

handleMerge              :: (Point_ point 2 r, Num r, Ord r) => Int -> Event point -> Sweep point ()
handleMerge i (v :+ adj) = do let ePred = adj^._1.start.extra -- i-1
                              tellIfMerge i v ePred
                              -- delete e_{i-1} from the status struct
                              modify $ \ss -> ss&statusStruct %~ deleteAt v (adj^._1)
                              connectToLeft i v

-- | finds the edge j to the left of v_i, and connect v_i to it if the helper
-- of j is a merge vertex
connectToLeft     :: (Point_ point 2 r, Num r, Ord r) => Int -> point -> Sweep point ()
connectToLeft i v = do ej <- gets $ \ss -> ss^?!statusStruct.to (lookupLE v)._Just
                       let j = ej^.start.extra
                       tellIfMerge i v j
                       modify $ \ss -> ss&helper %~ IntMap.insert j i

-- | returns True if v the interior of the polygon is to the right of v
isLeftVertex              :: (Point_ point 2 r, Ord r) => Int -> Event point -> Bool
isLeftVertex i (v :+ adj) = case (adj^._1.start) `cmpSweep` (v :+ i) of
                              GT -> True
                              _  -> False
  -- if the predecessor occurs before the sweep, this must be a left vertex

handleRegularL              :: (Point_ point 2 r, Num r, Ord r)
                            => Int -> Event point -> Sweep point ()
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

handleRegularR            :: (Point_ point 2 r, Num r, Ord r)
                          => Int -> Event point -> Sweep point ()
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


-- myPoly :: Polygon Multi () Rational
-- myPoly = MultiPolygon (CC.fromList $ read "[Point2 [16 % 1,80 % 1] :+ (),Point2 [16 % 1,16 % 1] :+ (),Point2 [144 % 1,16 % 1] :+ (),Point2 [144 % 1,80 % 1] :+ ()]"
--                       )
--   [ fromPoints $ read "[Point2 [88 % 1,48 % 1] :+ (),Point2 [112 % 1,40 % 1] :+ (),Point2 [112 % 1,48 % 1] :+ (),Point2 [80 % 1,56 % 1] :+ ()]"
  --   , fromPoints $ read "[Point2 [32 % 1,64 % 1] :+ (),Point2 [32 % 1,32 % 1] :+ (),Point2 [64 % 1,32 % 1] :+ (),Point2 [64 % 1,64 %1] :+ ()]"
--   ]

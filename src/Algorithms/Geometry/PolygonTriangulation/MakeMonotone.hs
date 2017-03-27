{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE ScopedTypeVariables #-}
module Algorithms.Geometry.PolygonTriangulation.MakeMonotone where

import qualified Algorithms.Geometry.LineSegmentIntersection.BentleyOttmann as BO
import           Control.Lens
import           Control.Monad (forM_, when)
import           Control.Monad.Reader
import           Control.Monad.State.Strict
import           Control.Monad.Writer(WriterT, execWriterT,tell)
import qualified Data.OrdSeq as SS
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
import qualified Data.OrdSeq as OS
import Data.OrdSeq(Compare, OrdSeq)


import Data.Geometry.Ipe
import Debug.Trace

-- import Unsafe.Coerce
-- import Data.Coerce
-- import Data.Proxy
-- import Data.Reflection
-- import qualified Data.Map as Map
-- --------------------------------------------------------------------------------

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

-- fromAdjacencyList

makeMonotone      :: Polygon t p r -> proxy s -> PlanarSubdivision s p () Bool r
makeMonotone pg _ = undefined


ordAt :: (Fractional r, Ord r, Show r) => r -> Compare (LineSegment 2 p r)
ordAt r = traceShow ("R ",r) $ BO.ordAt r

type Event r = Point 2 r :+ (Two (LineSegment 2 Int r))

data StatusStruct r = SS { _statusStruct :: !(SS.OrdSeq (LineSegment 2 Int r))
                         , _helper       :: !(IntMap.IntMap Int)
                         -- ^ for every e_i, the id of the helper vertex
                         } deriving (Show)
makeLenses ''StatusStruct


ix' i = singular (ix i)

findDiagonals    :: forall t r p. (Fractional r, Ord r,          Show r, Show p)
                 => Polygon t p r -> [LineSegment 2 p r]
findDiagonals p' = map f . sweep
                 . NonEmpty.sortBy (flip cmpSweep)
                 . polygonVertices . withIncidentEdges
                 . first (^._1) $ p
  where
    -- remaps to get the p value rather than the vertexId
    f = first (\i -> vertexInfo^.ix' i._2)

    p :: Polygon t (SP Int (p :+ VertexType)) r
    p = numberVertices . classifyVertices . toCounterClockWiseOrder $ p'
    vertexInfo :: V.Vector (STR (Point 2 r) p VertexType)
    vertexInfo = let vs = polygonVertices p
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
-- handle e | traceShow (getIdx e, e) False = undefined
handle e = let i = getIdx e in getEventType e >>= \case
    Start   -> handleStart   i e
    End     -> handleEnd     i e
    Split   -> handleSplit   i e
    Merge   -> handleMerge   i e
    Regular | isLeftVertex i e -> handleRegularL i e
            | otherwise        -> handleRegularR i e


insertAt       :: (Ord r, Fractional r, Show r, Show q) => Point 2 r -> LineSegment 2 q r
               -> OrdSeq (LineSegment 2 q r) -> OrdSeq (LineSegment 2 q r)
insertAt v = SS.insertBy (ordAt $ v^.yCoord)
-- e $ t { SS.nav =  }

deleteAt v = SS.deleteAllBy (ordAt $ v^.yCoord)

  -- SS.delete e $ t { SS.nav = ordAtNav (v^.yCoord) }


-- handleStart   :: (Fractional r, Ord r, Show r) => Int -> Event r -> Sweep p r ()
handleStart i (v :+ adj) = modify $ \(SS t h) ->
                                SS (insertAt v (adj^._2) t)
                                   (IntMap.insert i i h)

-- handleEnd   :: (Fractional r, Ord r, Show r) => Int -> Event r -> Sweep p r ()
handleEnd i (v :+ adj) | traceShow ("end",i,v,adj) False = undefined
handleEnd i (v :+ adj) = do let iPred = adj^._1.start.extra  -- i-1
                            -- lookup p's helper; if it is a merge vertex
                            -- we insert a new segment
                            tellIfMerge i v iPred
                            -- delete e_{i-1} from the status struct
                            modify $ \ss ->
                              traceShow ("Deleting ", v, adj^._1,ss^.statusStruct)$
                              ss&statusStruct %~ deleteAt v (adj^._1)



-- | Adds edge (i,j) if e_j's helper is a merge vertex
tellIfMerge i v j = do SP u ut' <- getHelper j
                       let ut = traceShow ("TellIfMerge ",i,v,j,u,ut')  ut'
                       when (ut == Merge) (tell' $ ClosedLineSegment (v :+ i) u)

-- | Get the helper of edge i, and its vertex type
getHelper   :: Int -> Sweep p r (SP (Point 2 r :+ Int) VertexType)
getHelper i = do Just ui    <- gets (^.helper.at i)
                 STR u _ ut <- asks (^.ix' ui)
                 pure $ SP (u :+ ui) ut



lookupLE   :: (Ord r, Fractional r , Show r) => Point 2 r
           -> OrdSeq (LineSegment 2 Int r)
           -> Maybe (LineSegment 2 Int r)
lookupLE v s = let (l,m,_) = SS.splitOn (BO.xCoordAt $ v^.yCoord) (v^.xCoord) s
               in SS.lookupMax (l <> m)


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

handleMerge i (v :+ adj) = do let ePred = adj^._1.start.extra -- i-1
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

handleRegularL i (v :+ adj) | traceShow ("regularL ", (i,v,adj)) False = undefined
handleRegularL i (v :+ adj) = do let ePred = adj^._1.start.extra -- i-1
                                 tellIfMerge i v ePred
                                 -- delete e_{i-1} from the status struct
                                 modify $ \ss ->
                                   traceShow ("beforeDelete", ss) $
                                   ss&statusStruct %~ deleteAt v (adj^._1)
                                 -- insert a e_i in the status struct, and set its helper
                                 -- to be v_i
                                 modify $ \(SS t h) ->
                                     SS (traceShow ("BeforeInsert",v,t,h,adj^._2) $  (insertAt v (adj^._2) t))
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


loadT = do pgs <- readAllFrom "/Users/frank/tmp/testPoly.ipe"
                        :: IO [SimplePolygon () Rational :+ IpeAttributes Path Rational]
           mapM_ print pgs
           let diags = map (findDiagonals . (^.core)) pgs
               f = asIpeGroup . map (asIpeObject' mempty)
               out = [ asIpeGroup $ map (\(pg :+ a) -> asIpeObject pg a) pgs
                     , asIpeGroup $ map f diags
                     ]
               outFile = "/Users/frank/tmp/out.ipe"
           writeIpeFile outFile . singlePageFromContent $ out


testPg :: SimplePolygon Int Rational
testPg = fromPoints [ point2 10 0 :+ 1
                   , point2 20 1 :+ 2
                   , point2 12 20 :+ 3
                   , point2 1 19:+ 4
                   , point2 2 15:+ 5
                   , point2 4 17:+ 6
                   , point2 11 12 :+ 7
                   ]


-- newtype Key s a b = Key { getKey :: a -> b }

-- instance (Eq b, Reifies s a) => Eq (Key s a b) where
--   (Key f) == (Key g) = let x = reflect (Proxy :: Proxy s)
--                        in f x == g x

-- instance (Ord b, Reifies s a) => Ord (Key s a b) where
--   Key f `compare` Key g = let x = reflect (Proxy :: Proxy s)
--                           in f x `compare` g x


-- at 5 Map.insert k v m

-- at' :: a -> (Ord k => Map.Map k v -> r) -> Map.Map k v -> r
-- at' = undefined




-- at' :: a -> (Ord Map.Map ())

-- at' :: Ord b => a -> (Map.Map (Key s a b) v -> r) -> Map.Map (a -> b) v -> r
-- at' x f m = reify x (\p -> let m'  = coerceKeys p m
                        -- in f m')


-- inserAt  :: r -> v -> Map.Map (r -> b) v ->
--insertAt v e t

-- at'       :: a -> (forall s. Reifies s a => Map.Map (Key s a b) v -> res) -> Map.Map (a -> b) v -> res
-- at' x f m = reify x (\p -> f $ coerceKeys p m)

-- at'       :: a -> (forall (s :: *). Reifies s a => Map.Map (Key s a b) v -> res)
--           -> Map.Map (a -> b) v -> res
-- at' x f m = reify x (\p -> f . coerceKeys p $ m)

-- updateAt      :: a
--               -> (forall (s :: *). Reifies s a =>
--                    Map.Map (Key s a b) v -> Map.Map (Key s a b) v')
--               -> Map.Map (a -> b) v
--               -> Map.Map (a -> b) v'
-- updateAt x f m = reify x (\p -> uncoerceKeys . f . coerceKeys p $ m)

-- testl x k m = at' x (Map.lookup (Key k)) m

-- testr x k v m = updateAt x (Map.insert (Key k) v) m


-- lookupAt' :: Ord b => a -> (a -> b) -> Map.Map (a -> b) v -> Maybe v
-- lookupAt' x k m = reify x (\p -> Map.lookup (Key k) $ coerceKeys p m)

-- insertAt' :: Ord b => a -> (a -> b) -> v -> Map.Map (a -> b) v -> Map.Map (a -> b) v
-- insertAt' x f v m = reify x (\p -> let m'  = coerceKeys p m
--                                        m'' = Map.insert (Key f) v m'
--                                    in uncoerceKeys m''
--                             )

-- test x = let m= insertAt' 1 bar 5 Map.empty
--        in lookupAt' x foo m

-- coerceKeys   :: proxy s -> Map.Map (a -> b) v -> Map.Map (Key s a b) v
-- coerceKeys _ = unsafeCoerce

-- uncoerceKeys :: Map.Map (Key s a b) v -> Map.Map (a -> b) v
-- uncoerceKeys = unsafeCoerce

-- with     :: forall a v. a -> (forall s. Reifies s a => v) -> v
-- with x f = reify x (\p -> f)

-- lookup'       :: (Reifies s a, Ord b) => a -> Key s a b
--               -> Map.Map (Key s a b) v -> Maybe v
-- lookup' x k m = (\p -> Map.lookup k m)


-- lookup' :: (Ord b, Reifies s a) => Key s a b -> Map.Map (Key s a b) v -> Maybe v
-- lookup' = Map.lookup

-- lookup'' :: forall a b s v. (Ord b, Reifies s a)
--             => (a -> b) -> Map.Map (Key s a b) v -> Maybe v
-- lookup'' f = Map.lookup (Key f)

-- foo   :: Int -> String
-- foo 1 = "a"
-- foo 2 = "b"

-- bar   :: Int -> String
-- bar 1 = "b"
-- bar 2 = "b"

-- -- testz     :: (Reifies s a, Ord b) => (a -> b) -> v -> Map.Map (Key s a b) v
-- -- testz f v = Map.singleton (Key f) v

-- convert' :: proxy s' -> Key s a b -> Key s' a b
-- convert' _ = coerce

-- convert   :: proxy s' -> Map.Map (Key s a b) v -> Map.Map (Key s' a b) v
-- convert _ = unsafeCoerce

-- test x1 = reify x1 (\p1 ->
--                          testz bar 5
--                         )

-- foox x2 = reify x2 (\p2 -> )

-- ______
-- test   :: Int -> Maybe Int
-- test x = reify x (lookup'' foo . makeMap)
--   where
--     makeKey   :: Proxy s -> (a -> b) -> Key s a b
--     makeKey _ = Key

--     -- makeMap :: (Ord b, Reifies s a) => Proxy s -> Map.Map (Key s a b) v
--     makeMap   :: (Reifies s Int) => Proxy s -> Map.Map (Key s Int String) Int
--     makeMap p = convert p (testz bar 5)



-- lookupWith   :: Ord b => a -> Map (Key s a b) v -> Maybe v
-- lookupWith x = reify (\p -> Lookup )

-- lookup' :: (forall s. Reifies s a => )
-- lookup'

-- with :: Ord b => a -> (c -> Map (Key s )) Map (Key s a b) v ->



-- data EvalAt r a =  EvalAt (First r) (r -> a)

-- data Key a b  = Has a
--               | Needs (a -> b)



-- instance Ord b => Ord (Key a b) where
--   (Needs)

-- instance Show a => Show (Key a b) where



-- instance (Eq a) => Eq (EvalAt r a)


-- instance (Ord a) => Ord (EvalAt r a) where
--   (EvalAt r f) `compare` (EvalAt s g) = let x = getFirst $ r <> s
--                                         in f x `compare` g x

--------------------------------------------------------------------------------
-- |
-- Module      :  HGeometry.Polygon.Simple.ShortestPath.Tree
-- Copyright   :  (C) Frank Staals
-- License     :  see the LICENSE file
-- Maintainer  :  Frank Staals
--
-- Computing a shortest path tree in a simple polygon
--
--------------------------------------------------------------------------------
module HGeometry.Polygon.Simple.ShortestPath.Tree
  -- ( computeShortestPaths
  -- , Parent
  -- -- , shortestPathTree
  -- )
  where

import           Control.Lens
import           Data.Bifoldable
import           Data.Bifunctor (first)
import           Data.Coerce
import qualified Data.FingerTree as FT
import qualified Data.Foldable as F
import qualified Data.Sequence as Seq
import           HGeometry.Boundary
import qualified HGeometry.Boundary as Boundary
import           HGeometry.Ext
import           HGeometry.Intersection
import           HGeometry.Interval
import           HGeometry.PlaneGraph.Connected
import           HGeometry.Point
import           HGeometry.Polygon.Class
import           HGeometry.Polygon.Simple
import           HGeometry.Polygon.Simple.Class
import           HGeometry.Polygon.Simple.InPolygon
import           HGeometry.Polygon.Triangulation
import           HGeometry.Sequence.KV
import           HGeometry.Sequence.NonEmpty (ViewR1(..))
import qualified HGeometry.Sequence.NonEmpty as NESeq
import           HGeometry.Tree.Util
import           HGeometry.Trie
import           HGeometry.Unbounded
import           HGeometry.Vector
import           Hiraffe.DFS
import           Hiraffe.PlanarGraph.Connected


import           HGeometry.Number.Real.Rational

import           Debug.Trace
--------------------------------------------------------------------------------

-- class TriangulatedSimplePolygon_ polygon point r where
--   dualTree :: polygon -> BinaryTree (VertexIx polygon, Vertex polygon)


{-


-- | Labels each vertex ith its parent (if it has one).
computeShortestPaths        :: ( SimplePolygon_ simplePolygon  point r
                               , SimplePolygon_ poly (point :+ Either source (VertexIx poly)) r
                               , Point_ source 2 r
                               , Ord r, Num r
                               ) => source -> simplePolygon -> poly
computeShortestPaths s poly = computeShortestPaths' s poly (triangulate poly)
-}




-- computeShortestPaths'        :: ()
--                              => source
--                              -> CPlaneGraph s point e f
--                              --  ^ the triangulated polygon
--                              -> Maybe ()
-- computeShortestPaths' s poly =

--   do
--     (root,_) <- findOf (interiorFacePolygons.withIndex) sourceTriang poly
--     dualTree <- toBinaryTree $ dfs (dualGraph poly) root
--     let toDualTreesFrom



-- data AtMostThree a b = One   a b
--                      | Two   a b b
--                      | Three a b b b






-- | A dualTree of a polygon; essentially a binary tree (with the exception) that the root
-- may actually have three children.
data DualTree a e b = RootZero  a
                    | RootOne   a (e, BinaryTrie e b)
                    | RootTwo   a (e, BinaryTrie e b) (e, BinaryTrie e b)
                    | RootThree a (e, BinaryTrie e b) (e, BinaryTrie e b) (e, BinaryTrie e b)
                    deriving (Show,Eq,Ord,Functor,Foldable,Traversable)

-- | Access the root of the dual tree
rootVertex :: Lens (DualTree a e b) (DualTree a' e b) a a'
rootVertex = lens g (flip s)
  where
    g = \case
      RootZero r        -> r
      RootOne r _       -> r
      RootTwo r _ _     -> r
      RootThree r _ _ _ -> r
    s r = \case
      RootZero _         -> RootZero r
      RootOne _ a        -> RootOne r a
      RootTwo _ a b      -> RootTwo r a b
      RootThree _ a b c  -> RootThree r a b c



instance Bifunctor (DualTree a) where
  bimap f g = let go = bimap f g in \case
    RootZero r                     -> RootZero r
    RootOne r (e, t)               -> RootOne r (f e, go t)
    RootTwo r (e, a) (e',b)        -> RootTwo r (f e, go a) (f e', go  b)
    RootThree r (d, a) (e,b) (h,c) -> RootThree r (f d, go a) (f e, go  b) (f h, go c)

instance Bifoldable (DualTree a) where
  bifoldMap f g = let go = bifoldMap f g in \case
    RootZero _                     -> mempty
    RootOne _ (e, t)               -> f e <> go t
    RootTwo _ (e, a) (e',b)        -> f e <> go a <> f e' <> go  b
    RootThree _ (d, a) (e,b) (h,c) -> f d <> go a <> f e  <> go  b <> f h <> go c

-- | Computes the dual tree of the polygon, starting with the triangle containing
-- the source point.
--
-- returns a Maybe in case the polygon actually lies outside the polygon
dualTreeFrom             :: ( Point_ source 2 r
                            , Point_ point 2 r
                            , Ord r, Num r

                            , Show r, Show point, Show source
                            )
                         => source
                         -> CPlaneGraph s point PolygonEdgeType  f
                         --  ^ the triangulated polygon
                         -> Maybe (DualTree (FaceIx (CPlaneGraph s point PolygonEdgeType f))
                                            (DartIx (CPlaneGraph s point PolygonEdgeType f))
                                            (FaceIx (CPlaneGraph s point PolygonEdgeType f))
                                  )
dualTreeFrom source poly = do
    let gr                 = dualGraph poly
        inTriangle (i, pg) = case traceShowWith (source,pg,i,) $ source `inPolygon` pg of
                               Boundary.StrictlyOutside -> False
                               _                        -> True
    (root',_) <- traceShowWith ("root",) $ findOf (interiorFacePolygons.withIndex) inTriangle poly
    toDualTree $ toFaceIds $ dfs' (\u -> filter (\(e,_) -> gr^?!edgeAt e == Diagonal)
                                                (gr^..neighboursOfByEdge u.asIndex)
                                  ) (numVertices gr) (toDualVertexIx root')


-- | Coerce into the right type
toFaceIds    :: TrieF (KV []) d (VertexIx (DualGraphOf (CPlaneGraph s point e f)))
             -> TrieF (KV []) d (FaceIx   (CPlaneGraph s point e f))
toFaceIds    = fmap coerce
  -- TODO: figure out why this can't just be coerce?

toDualVertexIx :: FaceIx (CPlaneGraph s point e f)
               -> VertexIx (DualGraphOf (CPlaneGraph s point e f))
toDualVertexIx = coerce

-- | Tries to convert the RoseTree into a DualTree
toDualTree                   :: (Show e, Show v) =>
  TrieF (KV []) e v -> Maybe (DualTree v e v)
toDualTree (Node root' chs) | traceShow ("toDualTree",root',fmap asBinaryTrie chs

                                        ) False = undefined

toDualTree (Node root' chs)  = traverse asBinaryTrie chs >>= \res -> case traceShowWith ("chs",) $ F.toList (assocs res) of
  []      -> Just $ RootZero  root'
  [c]     -> Just $ RootOne   root' c
  [l,r]   -> Just $ RootTwo   root' l r
  [a,b,c] -> Just $ RootThree root' a b c
  _       -> Nothing


-- | Transform the dual tree into a format we can use to run the shortest path procedure on
toDualTreeFrom      :: ( PlanarGraph_ gr
                       , Eq (VertexIx gr)
                       )
                    => gr
                    -> source
                    -> DualTree (FaceIx gr) (DartIx gr) (FaceIx gr)
                    -> DualTree source
                                (Vector 2 (VertexIx gr, Vertex gr))
                                (VertexIx gr, Vertex gr)
toDualTreeFrom gr s dt = case first endPoints' dt of
    RootZero _        -> RootZero s
    RootOne _ a       -> RootOne s (mapEdge a)
    RootTwo _ a b     -> RootTwo s (mapEdge a) (mapEdge b)
    RootThree _ a b c -> RootThree s (mapEdge a) (mapEdge b) (mapEdge c)
  where
    mapEdge (e,a) = (e, mapWithEdgeLabels (thirdVertex e) thirdVertex a)

    endPoints' d = let ((u,v),(ux,vx)) = gr^.endPointsOf d.withIndex in Vector2 (u,ux) (v,vx)
    -- TODO: make sure the oreitnation is indeed right

    thirdVertex (Vector2 (l,_) (r,_)) f =
      case ifindOf (boundaryVerticesOf f.withIndex) (\v _ -> v /= l && v /= r) gr of
        Nothing -> error "absurd, third vertex not found"
        Just v  -> v


-- toDualTreeFrom'     :: gr
--                     -> Vector 2 (VertexIx gr, Vertex gr)
--                     -> BinaryTrie (DartIx gr) (FaceIx gr)
--                     -> BinaryTrie (Vector 2 (VertexIx gr, Vertex gr)) (VertexIx gr, Vertex gr)
-- toDualTreeFrom' gr = mapWithEdgeLabels _ thirdVertex
--   -- go
--   -- where
--   --   go diag = \case
--   --     Leaf f          -> Leaf $ thirdVertex diag f
--   --     OneNode f (d,c) -> OneNode (thirdVertex diag f) (d, go )



--------------------------------------------------------------------------------

-- toDualTreesFrom    :: gr
--                    -> DualTree d (FaceIx gr) (FaceIx gr)

--                    -> DualTree d (Vector 3 (Vector 2 (VertexIx gr, Vertex gr)))
--                                  (VertexIx gr, Vertex gr)
-- toDualTreesFrom gr = \case
--     RootZero root'    -> RootZero undefined
--     RootOne root' a   -> RootOne

--     undefined undefined -- go root' a
--     RootTwo root' a b -> let a' = go root' a
--                              b' = go root' b
--                          in RootTwo undefined a' b'
--     RootThree root' a b c -> let a' = go root' a
--                                  b' = go root' b
--                                  c' = go root' c
--                              in RootThree undefined a' b' c'
--   where
--




--     go0 parent = undefined

--     go = undefined
    -- go l r = \case
    --   Nil              -> Nil
    --   Internal l' t r' -> let otherVertex (i,_) = i /= fst l && i /= fst r
    --                       in case findOf (boundaryVerticesOf t.withIndex) otherVertex gr of
    --     Nothing -> error "toDualTreesFrom: absurd"
    --     Just w  -> Internal (go l w l') w (go r w r')



-- data DualTree v = DualTree { _leftVtx  :: v
--                            , _tree     :: BinaryTree v
--                            , _rightVtx :: v
--                            }
--                   deriving (Show)



--------------------------------------------------------------------------------

-- (Tree.Node root chs) = undefined



type Vertex' poly point = VertexIx poly :+ point


-- toDualTree      :: CPlaneGraph s point e f
--                 -> Tree (FaceIx (CPlaneGraph s point e f))
--                 -> BinaryTree (VertexIx (CPlaneGraph s point e f) :+ point)
-- toDualTree poly = fmap toTriangle


-- toDualTree'     :: CPlaneGraph s point e f
--                 -> Tree (FaceIx (CPlaneGraph s point e f))
--                 -> BinaryTree (FaceIx (CPlaneGraph s point e f))
-- toDualTree' poly = fmap toTriangle


      -- \case
      -- Node i





--------------------------------------------------------------------------------

{-

data Range r = EmptyR
             | Range { _min :: !r
                     , _max :: !r
                     }
             deriving (Show,Eq)

instance Semigroup (Range r) where
  EmptyR      <> r            = r
  l           <> EmptyR       = l
  (Range l _) <> (Range _ u') = Range l u'

instance Monoid (Range r) where
  mempty = EmptyR


newtype Elem a = Elem a
  deriving newtype (Show,Eq,Ord)


type OrdSeq a = FT.FingerTree (Range a) (Elem a)

instance Measured (Range a) (Elem a) where
  measure (Elem x) = Range (ValB x) (ValT x)


-- | The cusp of the funnel
data Cusp a v = Cusp { _leftMost   :: v
                     , _leftChain  :: OrdSeq v  -- ^ from the diagonal towards the apex
                     , _apex       :: a
                     , _rightChain :: OrdSeq v -- ^ from the diagonal towards the apex
                     , _rightMost  :: v
                     }
              deriving (Show,Eq)

-- | Lens to access the apex of the Cusp
apex :: Lens (Cusp a v) (Cusp a' v) a a'
apex = lens _apex { \(Cusp l _ r) a -> Cusp l a r}





data Split a v = ApexLeft  (Cusp a v) (Cusp v v)
               | AtApex    (Cusp a v) (Cusp a v)
               | ApexRight (Cusp v v) (Cusp a v)
               deriving (Show,Eq)


-- -- | Given a predicate p x y, find the first element x_i so that p x_i x_{i+1} is true
-- -- i.e. returns the tuple (x_1,..,x_{i-1}, x_i, x_{i+1},..,x_n)
-- --
-- -- Returns
-- breakMonotonoic   :: (a -> a -> Bool) -> Seq.Seq a
--                   -> Break a
-- breakMonotonoic p = go
--   where
--     go = \case
--       Empty                        -> None
--       (x :< Empty)                 -> MaybeLast Empty x
--       (x :< y :< rest) | p x y     -> Break Empty x (y :< rest)
--                        | otherwise -> undefined
-- this is just FT.search







splitAtParent                      :: v -> Cusp a v -> Split a v
splitAtParent w (Cusp l ls a rs r) = case ( isLeftTurn  w l' a
                                                                , isRightTurn w r' a
                                                                ) of
    (False,False) -> AtApex (Cusp l a (single w)) (Cusp (single w) a r)
    (False,True)  -> undefined

  where
    (l :<< _) = viewl1 ls
    (r :<< _) = viewl1 rs


    single x = Seq.Empty :>> x


compute             :: forall source vertex.
                       source
                    -> Cusp source vertex
                    -> BinaryTree vertex
                    -> [(vertex :+ Either source vertex)]
compute s poly = go
  where
    go      Cusp source vertex -> BinaryTree vertex -> [(vertex :+ Either source vertex)]
    go cusp = \case
      Nil            -> [] -- we are done in this branch
      Internal l w r -> case splitAtParent w cusp of
        ApexLeft cl cr -> (w :+ Right $ cl^.apex)   : go cl l        <> goVertex' cr r
        AtApex   cl cr -> (w :+ Left  $ cusp^.apex) : go cl l        <> go cr r
        ApexLeft cl cr -> (w :+ Right $ cr^.apex)   : goVertex' cl l <> go cr r

    goVertex' cusp = fmap (over extra %~ Right) . goVertex cusp

    goVertex      :: Cusp vertex vertex -> BinaryTree vertex -> [(vertex :+ vertex)]
    goVertex cusp = \case
      Nil            -> [] -- we are done in this branch
      Internal l w r -> case splitAtParent w cusp of
        ApexLeft cl cr -> (w :+ cl^.apex)   : goVertex cl l <> goVertex cr r
        AtApex   cl cr -> (w :+ cusp^.apex) : goVertex cl l <> goVertex cr r
        ApexLeft cl cr -> (w :+ cr^.apex)   : goVertex cl l <> goVertex cr r


-}



type R = RealNumber 5
test = let g = triangulate myPolygon
       in toDualTreeFrom g mySource <$> dualTreeFrom mySource g

mySource :: Point 2 R
mySource = Point2 224 112
myPolygon :: SimplePolygon (Point 2 R)
myPolygon = maybe (error "absurd") id $ fromPoints
            [ Point2 80 160
            , Point2 64 64
            , Point2 96 64
            , Point2 128 80
            , Point2 96 112
            , Point2 192 112
            , Point2 192 32
            , Point2 304 64
            , Point2 224 160
            , Point2 304 128
            , Point2 320 48
            , Point2 352 160
            , Point2 224 224
            , Point2 208 128
            ]

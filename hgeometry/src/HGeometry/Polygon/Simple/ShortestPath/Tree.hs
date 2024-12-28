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
import           Data.FingerTree (Measured, FingerTree, ViewR(..), SearchResult(..))
import qualified Data.FingerTree as FT
import qualified Data.Foldable as F
import           Data.Function (on)
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


computeShortestPaths'        :: ( Point_ source 2 r
                                , Point_ vertex 2 r
                                , Num r, Ord r
                                , Show r, Show source, Show vertex
                                )
                             => source
                             -> CPlaneGraph s vertex PolygonEdgeType f
                             --  ^ the triangulated polygon
                             -> [(vertex :+ VertexId s) :+ Either source (vertex :+ VertexId s)]
computeShortestPaths' s poly = case dualTreeFrom s poly of
    Nothing -> []
    Just tr -> triang <> case toTreeRep poly s tr of
                           RootZero  _       -> []
                           RootOne   _ a     -> compute' a
                           RootTwo   _ a b   -> compute' a <> compute' b
                           RootThree _ a b c -> compute' a <> compute' b <> compute' c
      where
        triang = (\u -> u :+ Left s) <$> poly^..boundaryVerticesOf (tr^.rootVertex).asIndexedExt
        compute' = compute ((==) `on` (view extra)) s


--------------------------------------------------------------------------------

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
toTreeRep         :: ( PlanarGraph_ gr
                     , Eq (VertexIx gr)
                     )
                  => gr
                  -> source
                  -> DualTree (FaceIx gr) (DartIx gr) (FaceIx gr)
                  -> DualTree source
                              (Vector 2 (Vertex gr :+ VertexIx gr))
                              (Vertex gr :+ VertexIx gr)
toTreeRep gr s dt = case first endPoints' dt of
    RootZero _        -> RootZero s
    RootOne _ a       -> RootOne s (mapEdge a)
    RootTwo _ a b     -> RootTwo s (mapEdge a) (mapEdge b)
    RootThree _ a b c -> RootThree s (mapEdge a) (mapEdge b) (mapEdge c)
  where
    mapEdge (e,a) = (e, mapWithEdgeLabels (thirdVertex e) thirdVertex a)

    endPoints' d = let ((u,v),(ux,vx)) = gr^.endPointsOf d.withIndex in Vector2 (ux :+ u) (vx :+ v)
    -- TODO: make sure the oreitnation is indeed right

    thirdVertex (Vector2 (_ :+ l) (_ :+ r)) f =
      case ifindOf (boundaryVerticesOf f.asIndexedExt) (\v _ -> v /= l && v /= r) gr of
        Nothing -> error "absurd, third vertex not found"
        Just v  -> v

--------------------------------------------------------------------------------

type Vertex' poly point = VertexIx poly :+ point


--------------------------------------------------------------------------------

data Range r = EmptyRange
             | Range { _min :: !r
                     , _max :: !r
                     }
             deriving (Show,Eq)

instance Semigroup (Range r) where
  EmptyRange  <> r            = r
  l           <> EmptyRange   = l
  (Range l _) <> (Range _ u') = Range l u'

instance Monoid (Range r) where
  mempty = EmptyRange

newtype Elem a = Elem a
  deriving newtype (Show,Eq,Ord)

type OrdSeq a = FingerTree (Range a) (Elem a)

instance Measured (Range a) (Elem a) where
  measure (Elem x) = Range x x


testSeq :: OrdSeq Int
testSeq = FT.fromList $ map Elem [2,4..16]

testSplit = FT.search (isRightSplit 5) testSeq





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
apex = lens _apex (\(Cusp l ls _ rs r) a -> Cusp l ls a rs r)


--------------------------------------------------------------------------------

-- | The result of splitting a Cusp
data Split a v = ApexLeft  (Cusp a v) (Cusp v v)
               | AtApex    (Cusp a v) (Cusp a v)
               | ApexRight (Cusp v v) (Cusp a v)
               deriving (Show,Eq)

isRightSplit q pref = \case
  EmptyRange -> True
  Range y _  -> case pref of
    EmptyRange -> False
    Range _ x  -> x >= q && q <= y



-- |
-- Given a vertex w, the range \ell_1,..,\ell_i = p, and the range q=\ell_{i+1},..,\ell_n.
-- returns whether or not we make a right turn when going from w to p and then to q.
--
-- i.e. this procedure is for searching on the left sequence of the cusp
--
isRightTurn        :: (Point_ vertex 2 r, Ord r, Num r)
                   => vertex -> Range vertex -> Range vertex -> Bool
isRightTurn w pref = \case
  EmptyRange -> True
  Range q _  -> case pref of
    EmptyRange -> False
    Range _ p  -> ccw w p q /= CCW

-- | for searching on the right sequence of the cusp
isLeftTurn w pref = \case
  EmptyRange -> True
  Range y _  -> case pref of
    EmptyRange -> False
    Range _ x  -> ccw w x y /= CW


splitAtParent                      :: (Point_ vertex 2 r, Ord r, Num r)
                                   => vertex -> Cusp a vertex -> Split a vertex
splitAtParent w (Cusp l ls a rs r) = case FT.search (isRightTurn w) ls of
    Nowhere            -> error "splitAtParent; absurd: precondition on left chain failed"
    Position lsL p lsR -> ApexRight (Cusp l lsL (coerce p) mempty w) (Cusp w lsR a rs r)
    OnLeft             -> ApexRight undefined undefined -- TODO

    OnRight            -> case FT.search (isLeftTurn w) rs of
      Nowhere            -> error "splitAtParent; absurd: precondition on right chain failed"
      Position rsR p rsL -> ApexLeft (Cusp w ls a rsL r) (Cusp l mempty (coerce p) rsR w)
    OnLeft               -> ApexLeft undefined undefined -- TODO
    OnRight            -> AtApex    (Cusp l ls a mempty w)    (Cusp w mempty a rs r)



compute   :: forall source vertex r.
             (Point_ vertex 2 r, Ord r, Num r)
          => (vertex -> vertex -> Bool)
          -- ^ equality test between vertices.
          -> source
          -> (Vector 2 vertex , BinaryTrie (Vector 2 vertex) vertex)
          -> [(vertex :+ Either source vertex)]
compute (=.=) s poly@(Vector2 l r,_) = go (Cusp l mempty s mempty r) poly
  where

    go             :: Cusp source vertex -> (Vector 2 vertex, BinaryTrie (Vector 2 vertex) vertex)
                   -> [(vertex :+ Either source vertex)]
    go cusp (e,tr) = (w :+ p) : rest
      where
        w      = tr^.root
        split' = splitAtParent w cusp
        p      = case split' of
                   ApexLeft  _  cr -> Right (cr^.apex)
                   AtApex    _  _  -> Left  (cusp^.apex)
                   ApexRight cl _  -> Right (cl^.apex)
        rest   = case tr of
          Leaf _                      -> []
          OneNode _ c@(Vector2 l r,_)
            | w =.= l -> case split' of
                ApexLeft  _ cr -> goVertex' cr c
                AtApex    _ cr -> go        cr c
                ApexRight _ cr -> go        cr c
            | w =.= r -> case split' of
                ApexLeft  cl _ -> go        cl c
                AtApex    cl _ -> go        cl c
                ApexRight cl _ -> goVertex' cl c
          TwoNode _ l r -> case split' of
            ApexLeft  cl cr -> go cl l        <> goVertex' cr r
            AtApex    cl cr -> go cl l        <> go cr r
            ApexRight cl cr -> goVertex' cl l <> go cr r

    goVertex' cusp = fmap (over extra Right) . goVertex cusp





    goVertex             :: Cusp vertex vertex
                         -> (Vector 2 vertex, BinaryTrie (Vector 2 vertex) vertex)
                         -> [vertex :+ vertex]
    goVertex cusp (e,tr) = (w :+ p) : rest
      where
        w      = tr^.root
        split' = splitAtParent w cusp
        p      = case split' of
                   ApexLeft  _  cr -> cr^.apex
                   AtApex    _  _  -> cusp^.apex
                   ApexRight cl _  -> cl^.apex
        rest   = case tr of
          Leaf _                      -> []
          OneNode _ c@(Vector2 l' r',_)
            | w =.= l' -> case split' of
                ApexLeft  _ cr -> goVertex cr c
                AtApex    _ cr -> goVertex cr c
                ApexRight _ cr -> goVertex cr c
            | w =.= r' -> case split' of
                ApexLeft  cl _ -> goVertex cl c
                AtApex    cl _ -> goVertex cl c
                ApexRight cl _ -> goVertex cl c
          TwoNode _ l r -> case split' of
            ApexLeft  cl cr -> goVertex cl l <> goVertex cr r
            AtApex    cl cr -> goVertex cl l <> goVertex cr r
            ApexRight cl cr -> goVertex cl l <> goVertex cr r







type R = RealNumber 5
test = let g = triangulate myPolygon
       in toTreeRep g mySource <$> dualTreeFrom mySource g

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

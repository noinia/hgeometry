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

import           Control.Lens hiding ((:<), (<|))
import           Data.Bifoldable
import           Data.Bifunctor (first)
import           Data.Coerce
import           Data.FingerTree ( Measured, FingerTree, ViewR(..), ViewL(..), SearchResult(..)
                                 , (<|)
                                 )
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
import           HGeometry.Polygon.Simple.DualTree
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

                                , Eq vertex
                                )
                             => source
                             -> CPlaneGraph s vertex PolygonEdgeType f
                             --  ^ the triangulated polygon
                             -> [(vertex :+ VertexId s) :+ Either source (vertex :+ VertexId s)]
computeShortestPaths' s poly = case dualTreeFrom s poly of
    Nothing -> []
    Just tr -> triang <> case orientDualTree (=.=) $ toTreeRep poly s tr of
                           RootZero  _       -> []
                           RootOne   _ a     -> compute' a
                           RootTwo   _ a b   -> compute' a <> compute' b
                           RootThree _ a b c -> compute' a <> compute' b <> compute' c
      where
        triang = (\u -> u :+ Left s) <$> poly^..boundaryVerticesOf (tr^.rootVertex).asIndexedExt
        compute' = compute (=.=) s
        (=.=) = (==) `on` (view extra)


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
isLeftTurn        :: (Point_ vertex 2 r, Ord r, Num r)
                  => vertex -> Range vertex -> Range vertex -> Bool
isLeftTurn w pref = \case
  EmptyRange -> True
  Range y _  -> case pref of
    EmptyRange -> False
    Range _ x  -> ccw w x y /= CW


data ConeCompare = InCone | LeftOfCone | RightOfCone deriving (Show,Eq)

-- | test if w lies in the cone defined by apex a and vertices l and r.
inApexCone         :: (Point_ vertex 2 r, Point_ apex 2 r, Ord r, Num r)
                   => vertex -> apex -> vertex -> vertex -> ConeCompare
inApexCone w a l r = case ccw a l w of
                       CCW -> LeftOfCone
                       _   -> case ccw a r w of
                                CW -> RightOfCone
                                _  -> InCone

firstWith      :: OrdSeq a -> a -> a
firstWith s d = case FT.viewl s of
                  EmptyL -> d
                  x :< _ -> coerce x

lastWith     :: OrdSeq a -> a -> a
lastWith s d = case FT.viewr s of
                 FT.EmptyR -> d
                 _ FT.:> x -> coerce x

singleton :: a -> OrdSeq a
singleton = FT.singleton . Elem

-- | Given a vertex w, splits the cusp into two cusps corresponding to the two
-- sides incident to w.
splitAtParent                         :: ( Point_ vertex 2 r, Ord r, Num r
                                         , Point_ apex 2 r
                                         , Show vertex, Show r, Show apex
                                         )
                                      => vertex -> Cusp apex vertex -> Split apex vertex
splitAtParent w cu@(Cusp l ls a rs r) = traceShowWith ("splitAtParent",w,cu,)  $
                                        case inApexCone w a (lastWith ls l) (lastWith rs r) of
    InCone      -> AtApex (Cusp l ls a mempty w)    (Cusp w mempty a rs r)
    LeftOfCone  -> searchLeft
    RightOfCone -> searchRight
  where
    searchLeft = case FT.search (isRightTurn w) ls of
      Nowhere            -> error "splitAtParent: absurd: precondition on left chain failed"

      Position lsL p lsR -> case FT.viewl lsL of
        EmptyL | isRightTurn' w l p ->  traceShow ("Position,isRightTurn",w,l,p) $
                            ApexRight (Cusp l mempty l mempty w) (Cusp w (coerce l <| ls) a rs r)
                            -- we are really in the OnLeft case
        _                           ->
          traceShowWith ("Position",w,cu,p,) $         ApexRight (Cusp l lsL (coerce p) mempty w)    (Cusp w lsR a rs r)
      OnLeft             -> case FT.viewl ls of
        EmptyL   -> ApexRight (Cusp l mempty l mempty w) (Cusp w (coerce l <| ls) a rs r)
                    -- we necessarily make a right turn at l since we are left of the
                    -- visible cone
        p :< _ | isRightTurn' w l p ->  traceShow ("isRightTurn",w,l,p) $
                    ApexRight (Cusp l mempty l mempty w) (Cusp w (coerce l <| ls) a rs r)
                   -- we actually allready make a right turn at l
                   -- so this is actually the same as the emptyL case
                 | otherwise          -> traceShow ("otherwise",w,l,p) $
                    ApexRight (Cusp l mempty (coerce p) mempty w) (Cusp w ls a rs r)

      OnRight            -> case FT.viewr ls of
          FT.EmptyR   -> error "splitAtParent. searchLeft: absurd. emptyR"
          lsL FT.:> p -> traceShowWith ("OnRight",w,) $

            ApexRight (Cusp l lsL (coerce p) mempty w) (Cusp w mempty a rs r)

    searchRight = case FT.search (isLeftTurn w) rs of
      Nowhere            -> error "splitAtParent: absurd: precondition on right chain failed"
      Position rsR p rsL -> case FT.viewl rsR of
        EmptyL | isLeftTurn' w r p ->
                   ApexLeft (Cusp l ls a (coerce r <| rs) w) (Cusp w mempty r mempty r)
        _                          ->
                    ApexLeft (Cusp l ls a rsL w) (Cusp w mempty (coerce p) rsR r)
      OnLeft             -> case FT.viewl rs of
        EmptyL   -> ApexLeft (Cusp l ls a (coerce r <| rs) w) (Cusp w mempty r mempty r)
                    -- we necessarily make a left turn at r since we are right of the
                    -- visible cone
        p :< lsR | isLeftTurn' w r p ->
                    ApexLeft (Cusp l ls a (coerce r <| rs) w) (Cusp w mempty r mempty r)
                   -- we actually allready make a left turn at r
                   -- so this is actually the same as the emptyL case
                 | otherwise          ->
                    ApexLeft (Cusp l ls a rs w) (Cusp w mempty (coerce p) mempty r)

      OnRight            -> case FT.viewr rs of
          FT.EmptyR -> error "splitAtParent. searchRight: absurd. emptyR"
          rsR FT.:> p -> ApexLeft (Cusp l ls a mempty w) (Cusp w mempty (coerce p) rsR r)

    isRightTurn' a' b (Elem c) = ccw a' b c /= CCW
    isLeftTurn'  a' b (Elem c) = ccw a' b c /= CW

-- | Run the actual shortest path computation.
compute   :: forall source vertex r.
             ( Point_ vertex 2 r, Ord r, Num r
             , Point_ source 2 r
             , Show vertex, Show source, Show r
             )
          => (vertex -> vertex -> Bool)
          -- ^ equality test between vertices.
          -> source
          -> (Vector 2 vertex , BinaryTrie (Vector 2 vertex) vertex)
          -> [(vertex :+ Either source vertex)]
compute (=.=) s poly@(Vector2 l0 r0,_) = go Left (Cusp l0 mempty s mempty r0) poly
  where

    -- ^ The shortest path computation; the apex of the cusp may be the arbitrary source
    -- point initially. Afterwards it may swtich to always being a vertex.
    go      :: forall f apex. (Applicative f, Point_ apex 2 r, Show apex)
            => (apex -> f vertex)
            -- ^ action to lift the apex into an f vertex.
            -> Cusp apex vertex
            -- ^ current cusp
            -> (Vector 2 vertex, BinaryTrie (Vector 2 vertex) vertex)
            -- ^ the edge, and the
            -> [(vertex :+ f vertex)]
    go left = worker
      where
        right  = pure
        -- | run the computation, guarenteeing that the apex is a vertex
        goVertex' cusp' = fmap (over extra right) . goVertex cusp'
        -- | the actual worker; we create a closure for the 'left'.
        worker cusp (_,tr) = (w :+ p) : rest
           where
             w      = tr^.root -- ^ the vertex we are currently processing

             -- | the result of splitting the cusp into two cusps corresponding to the
             -- 'outgoing' diagonals.
             split' = splitAtParent w cusp

             -- | the parent we assign to w, i.e. predecessor of w on the shortest path to s
             p      = case split' of
                        ApexLeft  _  cr -> right (cr^.apex)
                        AtApex    _  _  -> left  (cusp^.apex)
                        ApexRight cl _  -> right (cl^.apex)

             -- | compute the rest of the shortest path tree; i.e.
             rest   = case tr of
               Leaf _                      -> []
               OneNode _ e@(Vector2 l _,_)
                 | w =.= l   -> case split' of
                     ApexLeft  _ cr -> goVertex' cr e
                     AtApex    _ cr -> worker    cr e
                     ApexRight _ cr -> worker    cr e
                 | otherwise -> case split' of
                     ApexLeft  cl _ -> worker    cl e
                     AtApex    cl _ -> worker    cl e
                     ApexRight cl _ -> goVertex' cl e
               TwoNode _ l r             -> case split' of
                 ApexLeft  cl cr -> worker cl l    <> goVertex' cr r
                 AtApex    cl cr -> worker cl l    <> worker cr r
                 ApexRight cl cr -> goVertex' cl l <> worker cr r


    -- | run the worker 'go' where the apex is now guaranteed to be a vertex as well.
    goVertex      :: Cusp vertex vertex
                  -> (Vector 2 vertex, BinaryTrie (Vector 2 vertex) vertex) -> [vertex :+ vertex]
    goVertex cusp = coerce . go Identity cusp







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
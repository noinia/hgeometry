--------------------------------------------------------------------------------
-- |
-- Module      :  HGeometry.Polygon.Simple.DualTree
-- Copyright   :  (C) Frank Staals
-- License     :  see the LICENSE file
-- Maintainer  :  Frank Staals
--
-- The Dual tree of a simple polygon
--
--------------------------------------------------------------------------------
module HGeometry.Polygon.Simple.DualTree
  ( DualTree(..), rootVertex
  , trimap
  --
  , dualTreeFrom
  , toTreeRep
  --
  , orientDualTree, toDualTree
  ) where

import           Control.Lens hiding ((:<), (<|))
import           Data.Bifoldable
import           Data.Bifunctor (first)
import           Data.Coerce
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
import           Debug.Trace

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

-- | Map over the dual tree
trimap       :: (a -> a') -> (e -> e') -> (b -> b') -> DualTree a e b -> DualTree a' e' b'
trimap f g h = let go = bimap g (bimap g h) in \case
  RootZero  r       -> RootZero  (f r)
  RootOne   r a     -> RootOne   (f r) (go a)
  RootTwo   r a b   -> RootTwo   (f r) (go a) (go b)
  RootThree r a b c -> RootThree (f r) (go a) (go b) (go c)


instance Bifoldable (DualTree a) where
  bifoldMap f g = let go = bifoldMap f g in \case
    RootZero _                     -> mempty
    RootOne _ (e, t)               -> f e <> go t
    RootTwo _ (e, a) (e',b)        -> f e <> go a <> f e' <> go  b
    RootThree _ (d, a) (e,b) (h,c) -> f d <> go a <> f e  <> go  b <> f h <> go c


-- foldDualTreeWithEdge :: a -> (b,r) ->



--------------------------------------------------------------------------------


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
-- toDualTree (Node root' chs) | traceShow ("toDualTree",root',fmap asBinaryTrie chs

--                                         ) False = undefined
toDualTree (Node root' chs)  = traverse asBinaryTrie chs >>= \res ->
                               case F.toList (assocs res) of
                                 []      -> Just $ RootZero  root'
                                 [c]     -> Just $ RootOne   root' c
                                 [l,r]   -> Just $ RootTwo   root' l r
                                 [a,b,c] -> Just $ RootThree root' a b c
                                 _       -> Nothing



-- orientDualTree    :: forall graph vertex dart r.
--                      (PlaneGraph_ graph vertex, Point_ vertex 2 r, Fractional r, Ord r)
--                   => graph
--                   -> DualTree (FaceIx graph) dart (FaceIx graph)
--                   -> DualTree (FaceIx graph) dart (FaceIx graph)
-- orientDualTree gr = unTag . go . tagWithPoint
--   where
--     tagWithPoint = trimap tag id tag
--     tag fi       = pt :+ fi
--     unTag'       = view extra
--     unTag        = trimap unTag id unTag


--     go = \case
--       RootZero r        -> RootZero s
--       RootOne r a       -> RootOne s (mapEdge a)
--       RootTwo r a b     -> RootTwo s (mapEdge a) (mapEdge b)
--       RootThree r a b c -> RootThree s (mapEdge a) (mapEdge b) (mapEdge c)


-- | Orient the dual tree so that the left child is actually on the left as seen when
-- traversing from parent to children.
orientDualTree    :: forall source vertex r.
                     ( Point_ source 2 r
                     , Point_ vertex 2 r
                     , Show source, Show vertex, Show r
                     , Num r, Ord r)
                  => (vertex -> vertex -> Bool)
                  -> DualTree source (Vector 2 vertex) vertex
                  -> DualTree source (Vector 2 vertex) vertex
orientDualTree (=.=) = \case
    RootZero r        -> RootZero r
    RootOne r a       -> RootOne r   (mapEdge r a)
    RootTwo r a b     -> RootTwo r   (mapEdge r a) (mapEdge r b)
    RootThree r a b c -> RootThree r (mapEdge r a) (mapEdge r b) (mapEdge r c)
  where
    mapEdge r (e,t) = (e,asOrientedBinaryTrie (=.=) r t)

-- | Mkae sure that the oreintation, i.e. left child and right child are consistent
asOrientedBinaryTrie      :: forall parent point r.
                             (Point_ parent 2 r, Point_ point 2 r, Ord r, Num r
                             , Show parent, Show point, Show r
                             )
                          => (point -> point -> Bool)
                          -> parent
                          -> BinaryTrie (Vector 2 point) point -> BinaryTrie (Vector 2 point) point
asOrientedBinaryTrie (=.=) = go
  where
    go      :: forall p. (Point_ p 2 r, Show p)
            => p -> BinaryTrie (Vector 2 point) point -> BinaryTrie (Vector 2 point) point
    go p tr = case tr of
        Leaf _                              -> tr
        LeftNode v (d@(Vector2 q _),t)      -> let t' = go v t
                                               in decideSide v q (LeftNode  v (d,t'))
                                                                 (RightNode v (d,t'))
        RightNode v (d@(Vector2 q _),t)     -> let t' = go v t
                                               in decideSide v q (LeftNode  v (d,t'))
                                                                 (RightNode v (d,t'))
           -- this case should not occur atually. Maybe make it so in the type
        TwoNode v (d@(Vector2 q _),l) (e,r) -> let l' = go v l
                                                   r' = go v r
                                               in decideSide v q (TwoNode v (d,l') (e,r'))
                                                                 (TwoNode v (e,r') (d,l'))
                                                  -- note that we switch l and r here

                                                 -- case traceShowWith ("oBST",p,v,d,e,"is ",) $
                                                 --       ccw p v q of
                                                 --    CCW      ->
                                                 --    CW       ->
                                                 --    CoLinear
                                                 --      | v =.= q   -> TwoNode v (e,r') (d,l')
                                                 --      | otherwise -> TwoNode v (d,l') (e,r')
                                                 --    -- TBH, there should be a better way for this..

      where
        -- we want to whether the left subtree (whose left endpoint is q) is tLeft or tRight.
        decideSide v q tLeft tRight = case ccw p v q of
                                        CCW                  -> tLeft
                                        CW                   -> tRight
                                        CoLinear | v =.= q   -> tRight
                                                 | otherwise -> tLeft
                                        -- TBH, there should be a better way for this..


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

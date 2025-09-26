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
  , dualTreeFrom
  , dualTreeFromTriangle
  ) where

import           Control.Lens hiding ((:<), (<|))
import           Data.Bifoldable
import           Data.Maybe (mapMaybe)
import qualified HGeometry.Boundary as Boundary
import           HGeometry.PlaneGraph.Connected
import           HGeometry.Point
import           HGeometry.Polygon.Simple
import           HGeometry.Polygon.Triangulation
import           HGeometry.Trie

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
                            )
                         => source
                         -> CPlaneGraph s point PolygonEdgeType  f
                         --  ^ the triangulated polygon
                         -> Maybe (DualTree (FaceIx (CPlaneGraph s point PolygonEdgeType f))
                                            (DartIx (CPlaneGraph s point PolygonEdgeType f))
                                            (FaceIx (CPlaneGraph s point PolygonEdgeType f)
                                            , ( VertexIx (CPlaneGraph s point PolygonEdgeType f)
                                              , point
                                              )
                                            )
                                  )
dualTreeFrom source poly = do
    let inTriangle (_, pg) = case source `inPolygon` pg of
                               Boundary.StrictlyOutside -> False
                               _                        -> True
    (root',_) <- findOf (outerBoundaryPolygons.withIndex) inTriangle poly
    dualTreeFromTriangle root' poly

-- | Construct the dual tree from a given triangle
dualTreeFromTriangle            :: forall triangulatedPolygon vertex.
                                   ( PlaneGraph_ triangulatedPolygon vertex
                                   , HasOuterBoundaryOf triangulatedPolygon
                                   , Dart triangulatedPolygon ~ PolygonEdgeType
                                   )
                                => FaceIx triangulatedPolygon
                                   -- ^ the (index of the) triangle from which to produce
                                   -- the dual tree.
                                -> triangulatedPolygon
                                --  ^ the triangulated polygon
                                -> Maybe (DualTree (FaceIx triangulatedPolygon)
                                                   (DartIx triangulatedPolygon)
                                                   ( FaceIx triangulatedPolygon
                                                   , (VertexIx triangulatedPolygon, vertex)
                                                   )
                                         )
dualTreeFromTriangle rt poly = case mapMaybe' buildTree $
                                      poly^..outerBoundaryDartsOf rt.withIndex of
    []           -> Just $ RootZero  rt
    [a]          -> Just $ RootOne   rt a
    [a,b]        -> Just $ RootTwo   rt a b
    [a,b,c]      -> Just $ RootThree rt a b c
    _            -> Nothing
  where
    buildTree             :: (DartIx triangulatedPolygon, PolygonEdgeType)
                          -> ( DartIx triangulatedPolygon
                             , Maybe (BinaryTrie (DartIx triangulatedPolygon)
                                                 ( FaceIx triangulatedPolygon
                                                 , (VertexIx triangulatedPolygon, vertex)
                                                 )
                                     )
                             )
    buildTree (e,x) = (e,) $ case x of
      Original -> Nothing -- ^ we've reached the outer face
      Diagonal -> let e' = poly^.twinOf e
                      f        = poly^.incidentFaceOf e'.asIndex
                      (l,mlT)  = buildTree $ poly^.prevDartOf e'.withIndex
                      (r,mrT)  = buildTree $ poly^.nextDartOf e'.withIndex
                      w        = poly^.headOf r.withIndex
                  in Just $ case (mlT,mrT) of
                              (Nothing, Nothing) -> Leaf      (f,w)
                              (Nothing, Just rT) -> RightNode (f,w) (r,rT)
                              (Just lT, Nothing) -> LeftNode  (f,w) (l,lT)
                              (Just lT, Just rT) -> TwoNode   (f,w) (l,lT) (r,rT)

    mapMaybe'   :: (a -> (b,Maybe c)) -> [a] -> [(b,c)]
    mapMaybe' f = mapMaybe (sequence . f)

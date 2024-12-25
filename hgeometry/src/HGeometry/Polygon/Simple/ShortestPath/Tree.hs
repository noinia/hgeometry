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
import           Data.Coerce
import qualified Data.FingerTree as FT
import qualified Data.Sequence as Seq
import qualified Data.Tree as Tree
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
import           HGeometry.Sequence.NonEmpty (ViewR1(..))
import qualified HGeometry.Sequence.NonEmpty as NESeq
import           HGeometry.Tree.Binary.Static
import           HGeometry.Tree.Util
import           HGeometry.Unbounded
import           HGeometry.Vector
import           Hiraffe.DFS
import           Hiraffe.PlanarGraph.Connected


import           HGeometry.Number.Real.Rational

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

-- | A dualTree of a polygon; essentially a binary tree (with the exception) that the root
-- may actually have three children.
data DualTree a b = OneNode   a (BinaryTree b)
                  | TwoNode   a (BinaryTree b) (BinaryTree b)
                  | ThreeNode a (BinaryTree b) (BinaryTree b) (BinaryTree b)
                  deriving (Show,Eq,Ord,Functor,Foldable,Traversable)


-- | Computes the dual tree of the polygon, starting with the triangle containing
-- the source point.
--
-- returns a Maybe in case the polygon actually lies outside the polygon
dualTreeFrom             :: ( Point_ source 2 r
                            , Point_ point 2 r
                            , Ord r, Num r
                            )
                         => source
                         -> CPlaneGraph s point e f
                         --  ^ the triangulated polygon
                         -> Maybe (DualTree (FaceIx (CPlaneGraph s point e f))
                                            (FaceIx (CPlaneGraph s point e f))
                                  )
dualTreeFrom source poly = do
    let inTriangle (_, pg) = case source `inPolygon` pg of
                               Boundary.StrictlyOutside -> True
                               _                        -> False
    (root',_) <- findOf (interiorFacePolygons.withIndex) inTriangle poly
    toDualTree $ toFaceIds $ dfs (dualGraph poly) (toDualVertexIx root')


-- | Coerce into the right type
toFaceIds    :: Tree.Tree (VertexIx (DualGraphOf (CPlaneGraph s point e f)))
             -> Tree.Tree (FaceIx (CPlaneGraph s point e f))
toFaceIds    = coerce

toDualVertexIx :: FaceIx (CPlaneGraph s point e f)
               -> VertexIx (DualGraphOf (CPlaneGraph s point e f))
toDualVertexIx = coerce

-- | Tries to convert the RoseTree into a DualTree
toDualTree                       :: Tree.Tree a -> Maybe (DualTree a a)
toDualTree (Tree.Node root' chs)  = traverse fromRoseTree' chs <&> \case
  []      -> OneNode   root' Nil
  [c]     -> OneNode   root' c
  [l,r]   -> TwoNode   root' l r
  [a,b,c] -> ThreeNode root' a b c

--------------------------------------------------------------------------------

toDualTreesFrom    :: gr
                   -> DualTree (FaceIx gr) (FaceIx gr)
                   -> DualTree (Vector 3 (Vector 2 (VertexIx gr, Vertex gr)))
                               (VertexIx gr, Vertex gr)
toDualTreesFrom gr = \case
    OneNode root' a   -> OneNode undefined undefined -- go root' a
    TwoNode root' a b -> let a' = go root' a
                             b' = go root' b
                         in TwoNode undefined a' b'
    ThreeNode root' a b c -> let a' = go root' a
                                 b' = go root' b
                                 c' = go root' c
                             in ThreeNode undefined a' b' c'
  where
    go0 parent = undefined


    go l r = \case
      Nil              -> Nil
      Internal l' t r' -> let otherVertex (i,_) = i /= fst l && i /= fst r
                          in case findOf (boundaryVerticesOf t.withIndex) otherVertex gr of
        Nothing -> error "toDualTreesFrom: absurd"
        Just w  -> Internal (go l w l') w (go r w r')



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
test = dualTreeFrom mySource $ triangulate myPolygon

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

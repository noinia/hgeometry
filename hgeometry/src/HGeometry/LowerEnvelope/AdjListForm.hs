{-# LANGUAGE UndecidableInstances #-}
--------------------------------------------------------------------------------
-- |
-- Module      :  HGeometry.LowerEnvelope.AdjListForm
-- Copyright   :  (C) Frank Staals
-- License     :  see the LICENSE file
-- Maintainer  :  Frank Staals
--
-- A Representation of the Lower envelope of planes in Adjacency-list
-- form.
--
--------------------------------------------------------------------------------
module HGeometry.LowerEnvelope.AdjListForm
  ( LowerEnvelope(LowerEnvelope)
  , theUnboundedVertex, boundedVertices

  , singleton
  , fromVertexForm

  , BoundedVertexF(Vertex)
  , location, definers, location2

  , LEEdge(Edge)

  ) where


--------------------------------------------------------------------------------

import           Control.Applicative
import           Control.Lens
import qualified Data.Foldable as F
import           Data.Foldable1
import           Data.Function (on)
import qualified Data.List.NonEmpty as NonEmpty
import           Data.Maybe (mapMaybe)
import           Data.Ord (comparing)
import qualified Data.Sequence as Seq
import qualified Data.Set as Set
import qualified Data.Vector as V
import qualified Data.Vector.NonEmpty as NonEmptyV
import           HGeometry.Combinatorial.Util
import           HGeometry.Ext
import           HGeometry.Foldable.Sort
import           HGeometry.Foldable.Util
import           HGeometry.HalfLine
import           HGeometry.HyperPlane.Class
import           HGeometry.HyperPlane.NonVertical
import           HGeometry.Line
import           HGeometry.LowerEnvelope.Type
import           HGeometry.LowerEnvelope.VertexForm (IntersectionLine(..),intersectionLine)
import qualified HGeometry.LowerEnvelope.VertexForm as VertexForm
import           HGeometry.Point
import           HGeometry.Properties
import           HGeometry.Vector
import qualified HGeometry.Vector as Vec
import           HGeometry.Vector.NonEmpty.Util ()
import           Hiraffe.Graph

--------------------------------------------------------------------------------
-- * Data type defining a lower envelope

-- | The lower envelope in adjacencylist form.
data LowerEnvelope plane =
  LowerEnvelope !(UnboundedVertex plane) (Seq.Seq (BoundedVertex plane))

deriving instance (Show plane, Show (NumType plane)) => Show (LowerEnvelope plane)
deriving instance (Eq plane, Eq (NumType plane))     => Eq   (LowerEnvelope plane)

-- instance Functor LowerEnvelope where
-- instance Foldable LowerEnvelope where
-- instance Traversable LowerEnvelope where

-- | Traversal of the planes in the lower envelope
traverseLowerEnvelope                         :: ( Applicative f, NumType plane ~ NumType plane'
                                                 , Ord plane'
                                                 )
                                              => (plane -> f plane')
                                              -> LowerEnvelope plane -> f (LowerEnvelope plane')
traverseLowerEnvelope f (LowerEnvelope v0 vs) =
    LowerEnvelope <$> traverse f v0 <*> traverse (traverseBoundedV f) vs

-- | lens to access the unbounded vertex
theUnboundedVertex :: Lens' (LowerEnvelope plane) (UnboundedVertex plane)
theUnboundedVertex = lens (\(LowerEnvelope v _) -> v)
                          (\(LowerEnvelope _ vs) v -> LowerEnvelope v vs)

-- | Lens to access the sequence of bounded vertices.
boundedVertices :: Lens' (LowerEnvelope plane) (Seq.Seq (BoundedVertex plane))
boundedVertices = lens (\(LowerEnvelope _ vs)    -> vs)
                       (\(LowerEnvelope u _ ) vs -> LowerEnvelope u vs)

--------------------------------------------------------------------------------

-- | Given a single Vertex, construct a LowerEnvelope out of it.
singleton   :: (Plane_ plane r, Ord r, Fractional r, Ord plane)
            => VertexForm.LEVertex plane -> LowerEnvelope plane
singleton v = LowerEnvelope v0 (Seq.singleton v')
  where
    i  = 1 -- the vertexID we are using for this vertex.
    v' = fromLEVertex v
    v0 = UnboundedVertex $ flipEdge i <$> view incidentEdgesB v'
    -- do we need to reverse the sequenceo f edges?

-- | Given a Lower envelope in vertex form, construct the AdjacencyList representation out
-- of it.
--
-- \(O(n\log n)\)
fromVertexForm      :: forall plane r. (Plane_ plane r, Ord plane, Ord r, Num r)
                    => VertexForm.VertexForm plane -> LowerEnvelope plane
fromVertexForm lEnv = LowerEnvelope v0 vs
  where
    mkTuples i (v,defs) = [ (h,i,v,defs) | h <- F.toList defs ]
    definingPlane (h,_,_,_) = h

    v0 = UnboundedVertex mempty -- FIXME !!
    vs = mempty
    -- vs = fromList
    --    . fmap toVertex
    --    . groupOnCheap (\(_,i,_,_) -> i)

    -- computes all edges;
    es :: [LEEdge plane]
    es = foldMap (faceToEdges . sortAlongBoundary)
       . groupOnCheap definingPlane
       $ ifoldMapOf (indexing (vertices.withIndex)) mkTuples lEnv



type IntermediateFace plane = NonEmptyV.NonEmptyVector (IntermediateVertex plane)

-- | Given a convex face h (represented by its bounded vertices in CCW order along the
-- face; starting with the leftmost one), construct the edges from it.
--
-- So in particular, we have to find and add the at most two bounded vertices that are
-- connected to the unbounded vertex, and construct bounded edges for the remaining
-- vertices.
--
-- For each face, we produce only the half-edges so that the face lies to the left.
--
-- O(n)
--
-- TODO: return a non-empty
faceToEdges      :: IntermediateFace plane -> [LEEdge plane]
faceToEdges face
  | length face == 1 = [] -- TODO: create two unbounded edges representing this vertex
  | length face == 2 = [] -- test if the edge v0 v1 has h to its left or to its right. This determines if we have to create the unbounded edges (v0,v_infty) and (v_infty,v1)  or (v1,v_infty) and (v_infty,v0)

  | otherwise        = [] -- zip up with the successor, and find/insert the unbounded edges if needed



-- fromEdges ::
-- fromEdges =

toVertex    :: NonEmptyV.NonEmptyVector (IntermediateVertex plane)
            -> BoundedVertexF f plane
toVertex xs = Vertex v defs undefined -- TODO: we are missing info here.
  where
    (_, _, v, defs) = NonEmptyV.head xs

groupOnCheap  :: (Foldable f, Ord b)
              => (a -> b) -> f a -> [NonEmptyV.NonEmptyVector a]
groupOnCheap f = fmap NonEmptyV.unsafeFromVector . V.groupBy ((==) `on` f) . sortOnCheap f


type IntermediateVertex plane =
  (plane, VertexID, Point 3 (NumType plane), VertexForm.Definers plane)


-- | Sort the vertices of a (convex) face in counter clockwise order around its
-- boundary. The first vertex in the output is the leftmost vertex of the face.
--
-- running time: \(O(n \log n)\)
sortAlongBoundary      :: forall plane r. (Plane_ plane r, Ord r, Num r)
                       => NonEmptyV.NonEmptyVector (IntermediateVertex plane)
                       -> NonEmptyV.NonEmptyVector (IntermediateVertex plane)
sortAlongBoundary face = case mv0 of
    Nothing            -> face -- face only has one vertex, so it is already sorted
    Just (_, _, v0, _) -> NonEmptyV.unsafeFromVector $ sortBy (cmpAround $ v1 .-. v0) face
  where
    -- we find the leftmost vertex v1 of the face (pick lexicographically when there are
    -- multiple), and then compute its predecessor v0 in the order along the boundary of
    -- the face (i.e. we find the point v for which the line through v1 and v has maximum
    -- slope). We can then sort the vertices CCW around v1, with respect to the line
    -- through v1 and v0.

    -- the leftmost vertex v1

    (_, iv1, v1, _) = minimumBy (comparing (^._3)) face
    v1' = projectPoint v1

    -- its predecessor v0, if it exists
    mv0 :: Maybe (IntermediateVertex plane)
    mv0 = foldr findPred Nothing face
    findPred v@(_, iv, _, _) acc
      | iv == iv1 = acc -- skip v1 itself
      | otherwise = Just $ case acc of
                             Nothing -> v -- anything is better than nothing
                             Just u  -> maxBy cmpSlope' u v

    cmpSlope' :: IntermediateVertex plane -> IntermediateVertex plane -> Ordering
    cmpSlope' (_, _, u, _) (_, _, v, _) = case ccw v1' (projectPoint u) (projectPoint v) of
                                            CCW      -> GT
                                            CW       -> LT
                                            CoLinear -> EQ

    cmpAround :: Vector 3 r -> IntermediateVertex plane -> IntermediateVertex plane -> Ordering
    cmpAround w (_, _, u, _) (_, _, v, _) =
        ccwCmpAroundWith (Vec.prefix w) v1' u' v' <> cmpByDistanceTo v1' u' v'
      where
        u' = projectPoint u
        v' = projectPoint v


maxBy         :: (t -> t -> Ordering) -> t -> t -> t
maxBy cmp a b = case cmp a b of
                  LT -> b
                  _  -> a

--------------------------------------------------------------------------------

-- | The unbounded vertex, which by definition will have index 0
newtype UnboundedVertex plane = UnboundedVertex { _incidentEdgesU :: Seq.Seq (LEEdge plane) }
                              deriving (Show,Eq,Functor,Foldable,Traversable)

-- | access the incidentEdges of an unbounded vertex
incidentEdgesU :: Iso (UnboundedVertex plane) (UnboundedVertex plane')
                      (Seq.Seq (LEEdge plane)) (Seq.Seq (LEEdge plane'))
incidentEdgesU = coerced

--------------------------------------------------------------------------------

-- | Vertices in of the lower envelope in adjacencylist form.
type BoundedVertex = BoundedVertexF Seq.Seq

-- | Given a vertex in vertex form, construct A BoundedVertex from
-- it. Note that this essentially assumes the vertex is connected to
-- the unbounded vertex with all its outgoing edgse.
--
-- \(O(k \log k)\), where \(k\) is the number of definers.
fromLEVertex                              :: (Plane_ plane r, Ord r, Fractional r, Ord plane)
                                          => VertexForm.LEVertex plane
                                          -> BoundedVertex plane
fromLEVertex (VertexForm.LEVertex v defs) = Vertex v defs es
  where
    es  = (\(_ :+ EdgeDefiners hl hr) -> Edge 0 hl hr) <$> sortAroundStart es'
    es' = mapMaybe (\t@(Two h1 h2) -> let defs' = toNonEmpty' $ Set.delete h1 $ Set.delete h2 defs
                                      in outgoingUnboundedEdge v t defs'
                   ) $ uniquePairs defs
    toNonEmpty' s = case NonEmpty.nonEmpty $ F.toList s of
                      Just xs -> xs
                      _       -> error "fromLEVertex: absurd, there should be at least 3 definers"


-- | Given a bunch of halflines that all share their starting point v,
-- sort them cyclically around the starting point v.
sortAroundStart :: (Foldable f, Ord r, Num r)
                => f (HalfLine 2 r :+ e) -> Seq.Seq (HalfLine 2 r :+ e)
sortAroundStart = fromFoldable . sortBy @V.Vector compareAroundStart
  where
    compareAroundStart (HalfLine v d  :+ _)
                       (HalfLine _ d' :+ _) = ccwCmpAround v (v .+^ d) (v .+^ d')



-- | Given a location of a vertex v, a pair of planes h1,h2 and the
-- remaining defining planes of v, computes the outgoing half-line
-- from v on which h1,h2 are the lowest (if such an halfline exists).
outgoingUnboundedEdge                   :: ( Plane_ plane r, Ord r, Fractional r
                                           , Foldable1 f
                                           )
                                        => Point 3 r -- ^ the location of the vertex v
                                        -> Two plane -- ^ the pair of planes for which to compute
                                        -- the halfine
                                        -> f plane -- ^ the other planes intersecting at v
                                        -> Maybe (HalfLine 2 r :+ EdgeDefiners plane)
outgoingUnboundedEdge v (Two h1 h2) h3s =
  intersectionLine' h1 h2 >>= toHalfLineFrom (projectPoint v) h3s
  -- todo, if there are more planes, I guess we should check if the hl is not dominated by the other
  -- planes either.

-- | Given :
--
-- v : the projected location of the vertex
-- hs : the remaining planes defining v (typically just one plane h3)
-- l   : the (projection of the) line l in which planes h1 and h2 intersect (containing v)
--
-- we compute the half-line eminating from v in which h1 and h2 define
-- an edge incident to v.
toHalfLineFrom                  :: (Plane_ plane r, Foldable1 f, Fractional r, Ord r)
                                => Point 2 r -- ^ vertex v
                                -> f plane     -- ^ the remaining plane(s) hs
                                -> LinePV 2 r :+ EdgeDefiners plane -- ^ the line l
                                -> Maybe (HalfLine 2 r :+ EdgeDefiners plane)
toHalfLineFrom v hs ((LinePV _ w) :+ defs@(EdgeDefiners h1 h2)) =
    validate w defs <|> validate ((-1) *^ w) (EdgeDefiners h2 h1)
    -- We try both directions. Note that if we reverse the direction
    -- of the line, what the left/right plane is changes.
    --
    -- If neither direction works, then h1,h2 do not define a good
    -- direction. This should happen only when there are more than 3
    -- planes intersecting in v, i.e. in degernate sitatuions
  where
    -- | test if direction d is a good direction, i.e. if towards direction d
    -- h1 (and thus also h2) is actually lower than all remaining defining planes.
    validate d defs' = let zVal = evalAt (v .+^ d)
                       in if all (\h3 -> zVal h1 < zVal h3) hs
                          then Just (HalfLine v d :+ defs') else Nothing




-- | The planes left (above) and right (below) their intersection
-- line.
data EdgeDefiners plane = EdgeDefiners { _leftPlane  :: plane -- ^ above plane
                                       , _rightPlane :: plane -- ^ below plane
                                       }

-- | Computes the line in which the two planes intersect, and labels
-- the halfplanes with the lowest plane.
intersectionLine'      :: ( Plane_ plane r, Ord r, Fractional r)
                       => plane -> plane -> Maybe (LinePV 2 r :+ EdgeDefiners plane)
intersectionLine' h h' = intersectionLine h h' <&> \case
    Vertical x    -> LinePV (Point2 x 0) (Vector2 0 1) :+ definers' (Point2 (x-1) 0)
    NonVertical l -> let l'@(LinePV p _) = fromLineEQ l
                     in l' :+ definers' (p&yCoord %~ (+1))
  where
    definers' q = let f = evalAt q
                  in if f h <= f h' then EdgeDefiners h h' else EdgeDefiners h' h
    fromLineEQ (LineEQ a b) = fromLinearFunction a b

--------------------------------------------------------------------------------

class HasIncidentEdges t  where
  incidentEdges' :: Lens' (t plane) (Seq.Seq (LEEdge plane))

instance HasIncidentEdges UnboundedVertex where
  incidentEdges' = incidentEdgesU

instance HasIncidentEdges BoundedVertex where
  incidentEdges' = lens (view incidentEdgesB) (\(Vertex p d _) es -> Vertex p d es)

-- instance HasIncidentEdges (Vertex (LowerEnvelope plane)) plane where
--   incidentEdges' = undefined -- pick either incidentEdges on the left or on the right thing.

instance HasVertices' (LowerEnvelope plane) where
  type Vertex   (LowerEnvelope plane) = Either (UnboundedVertex plane) (BoundedVertex plane)
  type VertexIx (LowerEnvelope plane) = VertexID

  -- | note, trying to assign the unbounded vertex to something with index >0 is an error
  vertexAt        :: VertexID
                  -> IndexedTraversal' VertexID (LowerEnvelope plane)
                                                (Vertex (LowerEnvelope plane))
  vertexAt i
      | i == 0    = conjoined trav0  (itrav0 .indexed)
      | otherwise = conjoined travVs (itravVs.indexed)
    where
      trav0  f (LowerEnvelope v0 vs)  = flip LowerEnvelope vs . unLeft <$> f   (Left v0)

      itrav0                          :: Applicative f
                                      => (VertexID -> Vertex   (LowerEnvelope plane)
                                         -> f (Vertex   (LowerEnvelope plane)))
                                      -> LowerEnvelope plane -> f (LowerEnvelope plane)
      itrav0 f (LowerEnvelope v0 vs)  = flip LowerEnvelope vs . unLeft <$> f 0 (Left v0)

      travVs                          :: Applicative f
                                      => (Vertex   (LowerEnvelope plane)
                                         -> f (Vertex   (LowerEnvelope plane)))
                                      -> LowerEnvelope plane -> f (LowerEnvelope plane)
      travVs f (LowerEnvelope v0 vs)  =
        LowerEnvelope v0 <$> (vs&ix (i+1) %%~ fmap unRight . f . Right)
      itravVs f (LowerEnvelope v0 vs) =
        LowerEnvelope v0 <$> (vs&ix (i+1) %%~ fmap unRight . f i . Right)

      unLeft  = either id (error "LowerEnvelope.vertexAt: trying to convert v_infty into a normal vertex is not allowed")
      unRight = either (error "LowerEnvelope.vertexAt: trying to convert a regular bounded vertex into v_infty is not allowed") id
  {-# INLINE vertexAt #-}


instance HasVertices (LowerEnvelope plane) (LowerEnvelope plane') where

  vertices = conjoined traverse' (itraverse' . indexed)
    where
      traverse' :: (Applicative f)
                => (Vertex (LowerEnvelope plane) -> f (Vertex (LowerEnvelope plane')))
                -> LowerEnvelope plane -> f (LowerEnvelope plane')
      traverse'  f (LowerEnvelope v0 vs) =
        LowerEnvelope <$> (fmap unLeft . f .Left) v0 <*> traverse (fmap unRight . f . Right) vs

      unLeft  = either id (error "LowerEnvelope.vertices: trying to convert v_infty into a normal vertex is not allowed")
      unRight = either (error "LowerEnvelope.vertices: trying to convert a regular bounded vertex into v_infty is not allowed") id

      itraverse' :: (Applicative f)
                 => (VertexID -> Vertex (LowerEnvelope plane) -> f (Vertex (LowerEnvelope plane')))
                -> LowerEnvelope plane -> f (LowerEnvelope plane')
      itraverse' f (LowerEnvelope v0 vs) =
        LowerEnvelope <$> (fmap unLeft . f 0 .Left) v0
                      <*> itraverse (\i -> fmap unRight . f (i+1) . Right) vs
  {-# INLINE vertices #-}

----------------------------------------

instance HasDarts' (LowerEnvelope plane) where
  type Dart   (LowerEnvelope plane) = LEEdge plane
  type DartIx (LowerEnvelope plane) = ( VertexIx (LowerEnvelope plane)
                                      , VertexIx (LowerEnvelope plane)
                                      )
  dartAt (u,v) = vertexAt u <.> beside l l
    where
      l :: HasIncidentEdges t => IndexedTraversal' VertexID (t plane) (LEEdge plane)
      l = incidentEdges' .> first' v (view destination)
  {-# INLINE dartAt #-}

-- | Helper function to access the an edge
first'              :: Eq i => i -> (a -> i)
                    -> IndexedTraversal' i (Seq.Seq a) a
first' i getIdx f s = case Seq.findIndexL ((== i) . getIdx) s of
                        Nothing -> pure s
                        Just j  -> s&ix j %%~ indexed f i


instance HasDarts (LowerEnvelope plane) (LowerEnvelope plane) where
  darts = itraverse' . indexed
    -- the traverse' variant that does not use the indices does not really look all that
    -- useful. So I didn't bother writing the faster version.
    where
      itraverse' :: (Applicative f)
                 => (DartIx (LowerEnvelope plane) -> LEEdge plane -> f (LEEdge plane))
                 -> LowerEnvelope plane -> f (LowerEnvelope plane)
      itraverse' f (LowerEnvelope v0 vs) =
        LowerEnvelope <$> (v0&incidentEdgesU.traverse %%~ liftF f 0)
                      <*> itraverse (\i vtx -> vtx&incidentEdgesB.traverse %%~ liftF f (i+1)) vs

      liftF       :: (DartIx (LowerEnvelope plane) -> LEEdge plane -> f (LEEdge plane))
                  -> VertexIx (LowerEnvelope plane)
                  -> LEEdge plane -> f (LEEdge plane)
      liftF f u e = f (u,e^.destination) e
  {-# INLINE darts #-}

--------------------------------------------------------------------------------


instance HasEdges' (LowerEnvelope plane) where
  type Edge   (LowerEnvelope plane) = LEEdge plane
  type EdgeIx (LowerEnvelope plane) = ( VertexIx (LowerEnvelope plane)
                                      , VertexIx (LowerEnvelope plane)
                                      )
  edgeAt = dartAt
  {-# INLINE edgeAt #-}

instance HasEdges (LowerEnvelope plane) (LowerEnvelope plane) where
  -- | Traversal of all edges in the graph; i.e. we traverse the edges only in the
  -- direction from lower vertex id to higher vertexId.
  edges = darts . ifiltered (\(u,v) _ -> u < v)
  {-# INLINE edges #-}

-- FIXME: I guess strictly speaking the lower envelope is a multigraph: in case
-- the lower envelope is a bunch of parallel edges connecting v_infty to v_infty
-- for example.

-- instance Graph_ (LowerEnvelope plane) where


--------------------------------------------------------------------------------



-- instance Semigroup (LowerEnvelope plane) where
--   (LowerEnvelope u vs) <> (LowerEnvelope u' vs') = LowerEnvelope undefined undefined
  -- main idea would be to insert the vertices of vs' into vs this
  -- requires testing if a vertex already exists, and shifting it if
  -- not.  this should run in O(log n) time per edge.
  --
  -- and therefore in O(l log l + r log l) = O(n log n) time.  hmm, I
  -- guess for iterative merging we cannot afford the "l" term.  I
  -- guess we need it only to maintain some Map from location -> id.
  -- so maybe we can maintain that separately.

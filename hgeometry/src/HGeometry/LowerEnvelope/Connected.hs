{-# LANGUAGE UndecidableInstances #-}
--------------------------------------------------------------------------------
-- |
-- Module      :  HGeometry.LowerEnvelope.Connected
-- Copyright   :  (C) Frank Staals
-- License     :  see the LICENSE file
-- Maintainer  :  Frank Staals
--
-- A Representation of the Lower envelope of planes in Adjacency-list
-- form.
--
--------------------------------------------------------------------------------
module HGeometry.LowerEnvelope.Connected
  ( LowerEnvelope'(LowerEnvelope)
  , theUnboundedVertex, boundedVertices

  , singleton
  , fromVertexForm'

  , BoundedVertexF(Vertex)
  , location, definers, location2

  , UnboundedVertex(UnboundedVertex)
  , unboundedVertexId
  , HasUnboundedEdges(..)

  , EdgeGeometry
  , projectedEdgeGeometries, projectedEdgeGeometry
  ) where

import           Control.Applicative
import           Control.Lens
import qualified Data.Foldable as F
import           Data.Foldable1
import           Data.Foldable1.WithIndex
import           Data.Function (on)
import qualified Data.List as List
import           Data.List.NonEmpty (NonEmpty(..))
import qualified Data.List.NonEmpty as NonEmpty
import           Data.Maybe (maybeToList)
import           Data.Ord (comparing)
import qualified Data.Sequence as Seq
import qualified Data.Set as Set
import           Data.Traversable (mapAccumL)
import qualified Data.Vector as V
import qualified Data.Vector.NonEmpty as NonEmptyV
import           Data.Vector.NonEmpty (NonEmptyVector)
import           HGeometry.Combinatorial.Util
import           HGeometry.Ext
import           HGeometry.Foldable.Sort
import           HGeometry.Foldable.Util
import           HGeometry.HalfLine
import           HGeometry.HyperPlane.Class
import           HGeometry.HyperPlane.NonVertical
import           HGeometry.Line
import           HGeometry.LineSegment
import           HGeometry.LowerEnvelope.Type
import           HGeometry.LowerEnvelope.VertexForm (IntersectionLine(..),intersectionLine)
import qualified HGeometry.LowerEnvelope.VertexForm as VertexForm
import           HGeometry.Point
import           HGeometry.Box
import           HGeometry.Properties
import           HGeometry.Vector
import qualified HGeometry.Vector as Vec
import           HGeometry.Vector.NonEmpty.Util ()
import           Hiraffe.Graph
import           Witherable

import           Debug.Trace

--------------------------------------------------------------------------------
-- * Data type defining a connected lower envelope

-- | A connected lower envelope in adjacencylist form.
--
-- invariant: there is always at least one bounded vertex.
data LowerEnvelope' plane =
  LowerEnvelope !(UnboundedVertex plane) (Seq.Seq (BoundedVertex plane))

deriving instance (Show plane, Show (NumType plane)) => Show (LowerEnvelope' plane)
deriving instance (Eq plane, Eq (NumType plane))     => Eq   (LowerEnvelope' plane)

type instance NumType   (LowerEnvelope' plane) = NumType plane
type instance Dimension (LowerEnvelope' plane) = 3

instance (Ord (NumType plane), Num (NumType plane)) => IsBoxable (LowerEnvelope' plane) where
  -- ^ the bounding box contains all bounded vertices
  boundingBox env = boundingBox . NonEmpty.fromList $ env^..boundedVertices.traverse.location
    -- the fromList is safe since there is alwasy at least one vertex

-- instance Functor LowerEnvelope' where
-- instance Foldable LowerEnvelope' where
-- instance Traversable LowerEnvelope' where

-- | Traversal of the planes in the lower envelope
traverseLowerEnvelope                         :: ( Applicative f, NumType plane ~ NumType plane'
                                                 , Ord plane'
                                                 )
                                              => (plane -> f plane')
                                              -> LowerEnvelope' plane -> f (LowerEnvelope' plane')
traverseLowerEnvelope f (LowerEnvelope v0 vs) =
    LowerEnvelope <$> traverse f v0 <*> traverse (traverseBoundedV f) vs

-- | lens to access the unbounded vertex
theUnboundedVertex :: Lens' (LowerEnvelope' plane) (UnboundedVertex plane)
theUnboundedVertex = lens (\(LowerEnvelope v _) -> v)
                          (\(LowerEnvelope _ vs) v -> LowerEnvelope v vs)

-- | Lens to access the sequence of bounded vertices.
boundedVertices :: Lens' (LowerEnvelope' plane) (Seq.Seq (BoundedVertex plane))
boundedVertices = lens (\(LowerEnvelope _ vs)    -> vs)
                       (\(LowerEnvelope u _ ) vs -> LowerEnvelope u vs)

--------------------------------------------------------------------------------

-- | Given a single Vertex, construct a LowerEnvelope' out of it.
singleton   :: (Plane_ plane r, Ord r, Fractional r, Ord plane)
            => VertexForm.LEVertex plane -> LowerEnvelope' plane
singleton v = LowerEnvelope v0 (Seq.singleton v')
  where
    i  = 1 -- the vertexID we are using for this vertex.
    v' = fromLEVertex v
    v0 = UnboundedVertex $ flipEdge i <$> view incidentEdgesB v'
    -- do we need to reverse the sequenceo f edges?

--------------------------------------------------------------------------------


--------------------------------------------------------------------------------


-- | Given a Lower envelope in vertex form, construct the AdjacencyList representation out
-- of it.
--
-- pre: The set of vertices is non-empty
--
-- \(O(n\log n)\)
fromVertexForm'      :: forall plane r. (Plane_ plane r, Ord plane, Ord r, Fractional r
                                       , Show plane, Show r
                                       )
                     => VertexForm.VertexForm plane -> LowerEnvelope' plane
fromVertexForm' lEnv = LowerEnvelope v0 boundedVs
  where
    v0 = UnboundedVertex unboundedEdges

    -- the bounded vertices.
    boundedVs = Seq.zipWith ( \(_,v,_) outs -> v&incidentEdgesB .~ toSeq outs
                            ) boundedVs' boundedEsByOrigin

    toSeq = foldMap (Seq.singleton . snd)

    -- all edges leaving from bounded vertices, ordered by their VertexIds
    boundedEsByOrigin = Seq.fromList . groupOnCheap fst . foldMap F.toList
                      $ allEdges

    -- computes all outgoing edges of all bounded vertices, they are grouped by face
    allEdges :: [FaceEdges plane]
    allEdges = traceShowWith ("allEdges",) .
      fmap (faceToEdges . sortAlongBoundary)
             . groupOnCheap definingPlane
             $ foldMap (^._3) boundedVs'

    -- | construct the unbounded edges, by simply filtering and flipping the
    -- edges towards infinity.
    unboundedEdges :: Seq.Seq (LEEdge plane)
    unboundedEdges = mapMaybe (\(u, e) -> if e^.destination == unboundedVertexId
                                          then Just (flipEdge u e) else Nothing
                              -- flip every unbounded edge
                              )
                   $ foldMap fromFoldable allEdges
    -- FIXME: we should probably sort those right?


    -- | A sequence of bounded vertices, ordered by vertexID, together with a bunch of
    -- copies; one for each face it will appear on
    --
    -- TODO: i guess that in degenerate sitations it could be that there are copies that
    -- wont actually proudce an edge (i.e. if there is some definer/plane containing the
    -- vertex, that does not appear on the lower envelope).
    --
    boundedVs' :: Seq.Seq ( VertexID, BoundedVertex plane, [IntermediateVertex plane])
    boundedVs' = ifoldMapOf (indexing (vertices.withIndex)) mkVtx lEnv

    mkVtx i (v,defs) = let j = i+1
                       in Seq.singleton ( j
                                        , Vertex v defs mempty
                                        , [ (h,j,v,defs) | h <- F.toList defs ]
                                        -- TODO: conceivably, we could delete the h from the
                                        -- defs here already? instead of in the various delete h
                                        -- cases
                                        )

    definingPlane (h,_,_,_) = h


-- | For each bounded vertex (represented by their ID) the outgoing halfedge.
type FaceEdges plane = NonEmpty (VertexID, LEEdge plane)


-- | Helper type for edgedefs
data EdgeDefs plane = EdgeDefs { common :: plane
                               , uNeigh :: plane
                               , vNeigh :: plane
                               } deriving (Show,Eq)

-- | Given a plane h, and vertices u (with its definers), and v (with its definers) that
-- define an edge of h, computes:
--
-- - plane h' that is on the other side of the edge from u to v,
-- - the plane hu incident only to u that is adjacent to h, and
-- - the plane hv incident only to v that is adjacent to h.
extractEdgeDefs                   :: (Ord plane
                                     , Show plane, Show r
                                     )
                                  => plane
                                  -> Point 3 r -> VertexForm.Definers plane
                                  -> Point 3 r -> VertexForm.Definers plane
                                  -> Maybe (EdgeDefs plane)
extractEdgeDefs h u uDefs v vDefs
  -- | traceShow ("extractEdgeDefs",h,u,uDefs,v,vDefs) False = undefined
  -- | otherwise
  = case traceShowWith ("commons",) commons of
    []   -> Nothing
    [h'] -> Just $ EdgeDefs h' hu hv
    _    -> error "extractEdgeDefs: unhandled degeneracy. u and v have >2 planes in common."
  where
    commons = F.toList $ Set.delete h --- $ traceShowWith ("udefs intersect vDefs",)
              (uDefs `Set.intersection` vDefs)
    uOnlies = F.toList $ uDefs Set.\\ vDefs
    vOnlies = F.toList $ vDefs Set.\\ uDefs

    hu = from' u uOnlies
    hv = from' v vOnlies

    from' x hss
      |traceShow ("from'",x,hss) False = undefined
      | otherwise
      = case hss of
      []   -> error "extractEdgeDefs: absurd, too few definers"
      [h'] -> h'
      hs    -> error $ "extractEdgeDefs: unhandled degeneracy. More than 3 planes at a vertex. "
                     <> show ("extractEdgeDefs",h,u,v,uOnlies,vOnlies)
             -- TODO we should either the neighbor of h in the order around the given
             -- vertex here.

--------------------------------------------------------------------------------

-- TODO: move to the definition below
data IntermediateVertex' plane =
  IntermediateVertex' { ivPlane :: plane
                      , ivId    :: {-# UNPACK #-} !VertexID
                      , ivLoc   :: Point 3 (NumType plane)
                      , ivDefs  :: VertexForm.Definers plane
                      , ivEdges :: Seq.Seq (LEEdge plane)
                      }

type IntermediateVertex plane =
  (plane, VertexID, Point 3 (NumType plane), VertexForm.Definers plane)


--------------------------------------------------------------------------------

-- type IntermediateFace plane = NonEmptyVector (IntermediateVertex plane)

-- type IntermediateFace' plane = IntermediateFaceF (IntermediateVertex plane)

-- data IntermediateFaceF v = BoundedFace (NonEmptyVector v)
--                          -- ^ the vertices of a bounded face, in order along the bounded
--                          -- face starting from the leftmost (and lowest if there are
--                          -- multiple) vertex
--                          | UnboundedFace (NonEmptyVector v)
--                          -- ^ vertices of an unbounded face, starting from the first
--                          -- vertex to the last one
--   deriving (Show,Eq,Functor)


--------------------------------------------------------------------------------

-- | Sort the vertices of a (convex) face in counter clockwise order around its
-- boundary.
--
-- running time: \(O(n \log n)\)
sortAlongBoundary      :: forall plane r. (Plane_ plane r, Ord r, Num r
                                          , Show plane, Show r
                                          )
                       => NonEmptyVector (IntermediateVertex plane)
                       -> NonEmptyVector (IntermediateVertex plane)
sortAlongBoundary face = case mv0 of
    Nothing            -> face -- already sorted, since there is only one vertex
    Just (_, _, v0, _) -> NonEmptyV.unsafeFromVector $ sortBy (cmpAround $ v1 .-. v0) face
    -- TODO: I guess that in degenerate situations, there now may be consecutive
    -- points (vertices) at the same location. We should group those and get rid of them?
  where
    -- we find the leftmost vertex v1 of the face (pick the lexicographically smallest
    -- when there are multiple), and then compute its predecessor v0 in the order along
    -- the boundary of the face (i.e. we find the point v for which the line through v1
    -- and v has maximum slope).

    -- To get the list of vertices in CCW order along the face, we then sort the vertices
    -- around v1
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

    -- | sort the vertices around the direction wrt. (the downward projection of) w
    cmpAround :: Vector 3 r -> IntermediateVertex plane -> IntermediateVertex plane -> Ordering
    cmpAround w (_, _, u, _) (_, _, v, _) =
        ccwCmpAroundWith (Vec.prefix w) v1' u' v' <> cmpByDistanceTo v1' u' v'
      where
        u' = projectPoint u
        v' = projectPoint v

-- | given an edge (u,v) that has h to its left, and all remaining vertices of the face,
-- sorted in CCW order around the face starting with *v*, compute all its edgesf
faceToEdges       :: forall plane r. (Plane_ plane r, Ord r, Fractional r, Ord plane
                                     , Show plane, Show r
                                     )
                   => NonEmptyVector (IntermediateVertex plane) -> FaceEdges plane
faceToEdges faceV = case toNonEmpty faceV of
                      u :| []  -> oneVertex u
                      u :| [v] -> twoVertices u v
                      _        -> toNonEmpty $ manyVertices
  where
    manyVertices = NonEmptyV.zipWith3 mkEdge (shiftR (-1) faceV) faceV (shiftR 1 faceV)

    mkEdge (_,uIdx,up,uDefs) (h,vIdx,vp,vDefs) (_,wIdx,wp,wDefs) =
      case extractEdgeDefs h vp vDefs wp wDefs of
        Nothing -> -- vw are separated by the unbounded vertex
                    case extractEdgeDefs h up uDefs vp vDefs of
                      Nothing                    ->
                        error "mkEdge: absurd, u and v don't share a plane!?"
                      Just (EdgeDefs hUV _hu hv) -> (vIdx, Edge unboundedVertexId h hv)
        Just (EdgeDefs hVW _ _) -> (vIdx, Edge wIdx h hVW)
      -- the main idea is that every pair of subsequent bounded vertices necessarily has
      -- exactly one defining plane, except for when two bounded vertices are separted by
      -- the vertex at infinity.


-- | compute the face edges of the face of h, when v is the only vertex incident to h.
oneVertex              :: (Plane_ plane r, Ord r, Fractional r, Ord plane
                          , Show plane, Show r
                          )
                       => IntermediateVertex plane -> FaceEdges plane
oneVertex (h,i,v,defs) = case List.sort outgoingEdges of
    [ Left hPred, Right hSucc ] -> NonEmpty.singleton (i, Edge unboundedVertexId h hSucc)
    _ -> error "oneVertex. absurd. Other than a single Left, and Right"
  where
    otherPlanes = Set.delete h defs
    -- | Tries to compute all outgoing edges involving h. Since every plane h appears
    -- exactly once in the order around v, this should produce exactly two edges; one
    -- left and one right.
    outgoingEdges = foldMap (\h' -> let rest = toNonEmpty' $ Set.delete h' otherPlanes
                                    in case outgoingUnboundedEdge v (Two h h') rest of
                                         Just e  -> [ tagOtherPlane $ e^.extra ]
                                         Nothing -> []
                            ) otherPlanes
    tagOtherPlane (EdgeDefiners hl hr) = if h == hl then Right hr else Left hl
    toNonEmpty' s = case NonEmpty.nonEmpty $ F.toList s of
                      Just xs -> xs
                      _       -> error "oneVertex. Absurd, there should be at least 3 definers"

-- | Compute the edges of the face incident to h, when there are only two vertices
-- incident to that face.
--
-- more or less a special case of the manyVertices scenario, in which we don't know
-- the if edge should be oriented from u to v or from v to u.
twoVertices  :: (Plane_ plane r, Ord r, Fractional r, Ord plane
                , Show plane, Show r
                )
             => IntermediateVertex plane -> IntermediateVertex plane -> FaceEdges plane
twoVertices u@(h,ui,up,uDefs) v@(_,vi,vp,vDefs) =
  case extractEdgeDefs h up uDefs vp vDefs >>= withUVOrder of
      Nothing  -> error "twoVertices. absurd, h and h' don't define an edge!?"
      Just (EdgeDefs hr hu hv, uBeforeV) -- -> traceShowWith ("twoVertices",u,v,uBeforeV,) $
        | uBeforeV -> NonEmpty.fromList [ (vi, Edge ui                h hr)
                                        , (ui, Edge unboundedVertexId h hu)
                                        ]
                      -- the edge must be oriented from v to u so that h is on the left
        | otherwise  -> NonEmpty.fromList [ (ui, Edge vi                h hr)
                                         , (vi, Edge unboundedVertexId h hv)
                                         ]
                        -- the edge must be oriented from u to v
  where
    -- determine if u lies before v in the order of u and v along the intersection line of
    -- h and hr.
    withUVOrder e@(EdgeDefs hr _ _) =
      ( \l -> let m = perpendicularTo l &anchorPoint .~ projectPoint up
              in (e, projectPoint vp `onSide` m /= LeftSide)
              -- if v lies on the left it lies further along commonLine. So u lies before
              -- v if v does not lie in the left haflpalne
      ) <$> intersectionLine' h hr

--------------------------------------------------------------------------------

-- | The unbounded vertex.
newtype UnboundedVertex plane = UnboundedVertex { _incidentEdgesU :: Seq.Seq (LEEdge plane) }
                              deriving (Show,Eq,Functor,Foldable,Traversable)

-- | access the incidentEdges of an unbounded vertex
incidentEdgesU :: Iso (UnboundedVertex plane) (UnboundedVertex plane')
                      (Seq.Seq (LEEdge plane)) (Seq.Seq (LEEdge plane'))
incidentEdgesU = coerced

-- | The vertexID of the unbounded vertex
unboundedVertexId :: VertexID
unboundedVertexId = 0

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
    es  = (\(_ :+ EdgeDefiners hl hr) -> Edge unboundedVertexId hl hr) <$> sortAroundStart es'
    es' = mapMaybe (\t@(Two h1 h2) -> let defs' = toNonEmpty' $ Set.delete h1 $ Set.delete h2 defs
                                      in outgoingUnboundedEdge v t defs'
                   ) $ uniquePairs defs
    toNonEmpty' s = case NonEmpty.nonEmpty $ F.toList s of
                      Just xs -> xs
                      _       -> error "fromLEVertex: absurd, there should be at least 3 definers"


-- | Given a bunch of halflines that all share their starting point v,
-- sort them cyclically around the starting point v.
sortAroundStart :: (Foldable f, Ord r, Num r)
                => f (HalfLine (Point 2 r) :+ e) -> Seq.Seq (HalfLine (Point 2 r) :+ e)
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
                                        -> Maybe (HalfLine (Point 2 r) :+ EdgeDefiners plane)
outgoingUnboundedEdge v (Two h1 h2) h3s =
  intersectionLineWithDefiners h1 h2 >>= toHalfLineFrom (projectPoint v) h3s
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
                                -> Maybe (HalfLine (Point 2 r) :+ EdgeDefiners plane)
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
--
-- The returned line will have h to its left and h' to its right.
intersectionLineWithDefiners      :: ( Plane_ plane r, Ord r, Fractional r)
                                  => plane -> plane -> Maybe (LinePV 2 r :+ EdgeDefiners plane)
intersectionLineWithDefiners h h' = (:+ EdgeDefiners h h') <$> intersectionLine' h h'

-- | Computes the line in which the two planes intersect. The returned line will have h to
-- its left and h' to its right.
--
--
intersectionLine'      :: ( Plane_ plane r, Ord r, Fractional r)
                       => plane -> plane -> Maybe (LinePV 2 r)
intersectionLine' h h' = intersectionLine h h' <&> \case
    Vertical x    -> reorient (LinePV (Point2 x 0) (Vector2 0 1)) (Point2 (x-1) 0)
    NonVertical l -> let l'@(LinePV p _) = fromLineEQ l
                     in reorient l' (p&yCoord %~ (+1))
  where
    -- make sure h is to the left of the line
    reorient l q = let f = evalAt q
                   in if f h <= f h' then l else l&direction %~ negated
    fromLineEQ (LineEQ a b) = fromLinearFunction a b




--------------------------------------------------------------------------------

-- |
class HasIncidentEdges t  where
  incidentEdges' :: Lens' (t plane) (Seq.Seq (LEEdge plane))

instance HasIncidentEdges UnboundedVertex where
  incidentEdges' = incidentEdgesU

instance HasIncidentEdges BoundedVertex where
  incidentEdges' = lens (view incidentEdgesB) (\(Vertex p d _) es -> Vertex p d es)

-- instance HasIncidentEdges (Vertex (LowerEnvelope' plane)) plane where
--   incidentEdges' = undefined -- pick either incidentEdges on the left or on the right thing.


--------------------------------------------------------------------------------
-- * The Instances for HasVertices, HasDarts, HasFaces etc

instance HasVertices' (LowerEnvelope' plane) where
  type Vertex   (LowerEnvelope' plane) = Either (UnboundedVertex plane) (BoundedVertex plane)
  type VertexIx (LowerEnvelope' plane) = VertexID

  -- | note, trying to assign the unbounded vertex to something with index /=
  -- unboundedVertexId is an error.
  vertexAt        :: VertexID
                  -> IndexedTraversal' VertexID (LowerEnvelope' plane)
                                                (Vertex (LowerEnvelope' plane))
  vertexAt i
      | i == unboundedVertexId = conjoined trav0  (itrav0 .indexed)
      | otherwise              = conjoined travVs (itravVs.indexed)
    where
      trav0  f (LowerEnvelope v0 vs)  = flip LowerEnvelope vs . unLeft <$> f   (Left v0)

      itrav0                          :: Applicative f
                                      => (VertexID -> Vertex   (LowerEnvelope' plane)
                                         -> f (Vertex   (LowerEnvelope' plane)))
                                      -> LowerEnvelope' plane -> f (LowerEnvelope' plane)
      itrav0 f (LowerEnvelope v0 vs)  = flip LowerEnvelope vs . unLeft <$> f 0 (Left v0)

      travVs                          :: Applicative f
                                      => (Vertex   (LowerEnvelope' plane)
                                         -> f (Vertex   (LowerEnvelope' plane)))
                                      -> LowerEnvelope' plane -> f (LowerEnvelope' plane)
      travVs f (LowerEnvelope v0 vs)  =
        LowerEnvelope v0 <$> (vs&ix (i-1) %%~ fmap unRight . f . Right)
      itravVs f (LowerEnvelope v0 vs) =
        LowerEnvelope v0 <$> (vs&ix (i-1) %%~ fmap unRight . f i . Right)

      unLeft  = either id (error "LowerEnvelope'.vertexAt: trying to convert v_infty into a normal vertex is not allowed")
      unRight = either (error "LowerEnvelope'.vertexAt: trying to convert a regular bounded vertex into v_infty is not allowed") id
  {-# INLINE vertexAt #-}


instance HasVertices (LowerEnvelope' plane) (LowerEnvelope' plane') where

  vertices = conjoined traverse' (itraverse' . indexed)
    where
      traverse' :: (Applicative f)
                => (Vertex (LowerEnvelope' plane) -> f (Vertex (LowerEnvelope' plane')))
                -> LowerEnvelope' plane -> f (LowerEnvelope' plane')
      traverse'  f (LowerEnvelope v0 vs) =
        LowerEnvelope <$> (fmap unLeft . f .Left) v0 <*> traverse (fmap unRight . f . Right) vs

      unLeft  = either id (error "LowerEnvelope'.vertices: trying to convert v_infty into a normal vertex is not allowed")
      unRight = either (error "LowerEnvelope'.vertices: trying to convert a regular bounded vertex into v_infty is not allowed") id

      itraverse' :: (Applicative f)
                 => (VertexID -> Vertex (LowerEnvelope' plane) -> f (Vertex (LowerEnvelope' plane')))
                -> LowerEnvelope' plane -> f (LowerEnvelope' plane')
      itraverse' f (LowerEnvelope v0 vs) =
        LowerEnvelope <$> (fmap unLeft . f unboundedVertexId .Left) v0
                      <*> itraverse (\i -> fmap unRight . f (i+1) . Right) vs
  {-# INLINE vertices #-}

----------------------------------------

instance HasDarts' (LowerEnvelope' plane) where
  type Dart   (LowerEnvelope' plane) = LEEdge plane
  type DartIx (LowerEnvelope' plane) = ( VertexIx (LowerEnvelope' plane)
                                       , VertexIx (LowerEnvelope' plane)
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


instance HasDarts (LowerEnvelope' plane) (LowerEnvelope' plane) where
  darts = itraverse' . indexed
    -- the traverse' variant that does not use the indices does not really look all that
    -- useful. So I didn't bother writing the faster version.
    where
      itraverse' :: (Applicative f)
                 => (DartIx (LowerEnvelope' plane) -> LEEdge plane -> f (LEEdge plane))
                 -> LowerEnvelope' plane -> f (LowerEnvelope' plane)
      itraverse' f (LowerEnvelope v0 vs) =
        LowerEnvelope <$> (v0&incidentEdgesU.traverse %%~ liftF f unboundedVertexId)
                      <*> itraverse (\i vtx -> vtx&incidentEdgesB.traverse %%~ liftF f (i+1)) vs

      liftF       :: (DartIx (LowerEnvelope' plane) -> LEEdge plane -> f (LEEdge plane))
                  -> VertexIx (LowerEnvelope' plane)
                  -> LEEdge plane -> f (LEEdge plane)
      liftF f u e = f (u,e^.destination) e
  {-# INLINE darts #-}

--------------------------------------------------------------------------------


instance HasEdges' (LowerEnvelope' plane) where
  type Edge   (LowerEnvelope' plane) = LEEdge plane
  type EdgeIx (LowerEnvelope' plane) = ( VertexIx (LowerEnvelope' plane)
                                       , VertexIx (LowerEnvelope' plane)
                                       )
  edgeAt = dartAt
  {-# INLINE edgeAt #-}

instance HasEdges (LowerEnvelope' plane) (LowerEnvelope' plane) where
  -- | Traversal of all edges in the graph; i.e. we traverse the edges only in the
  -- direction from lower vertex id to higher vertexId.
  edges = darts . ifiltered (\(u,v) _ -> u < v)
  {-# INLINE edges #-}

-- FIXME: I guess strictly speaking the lower envelope is a multigraph: in case
-- the lower envelope is a bunch of parallel edges connecting v_infty to v_infty
-- for example.

-- instance Graph_ (LowerEnvelope' plane) where

class HasUnboundedEdges t e | t -> e where
  -- | Lens to access the unbounded edges.
  unboundedEdges :: Lens' t (Seq.Seq e)

instance HasUnboundedEdges (LowerEnvelope' plane) (LEEdge plane) where
  unboundedEdges = theUnboundedVertex.incidentEdgesU


-- | Edges are either halflines or Linesegments
type EdgeGeometry point = Either (HalfLine point) (ClosedLineSegment point)

-- | Get the projected geometries (halflines and edges line segments ) representing the
-- edges.
projectedEdgeGeometries :: (Plane_ plane r, Ord r, Fractional r
                                       , Show plane, Show r

                           )
                        => IndexedFold (EdgeIx (LowerEnvelope' plane))
                                       (LowerEnvelope' plane) (EdgeGeometry (Point 2 r))
projectedEdgeGeometries = ifolding $ \env ->
  (\(i,e) -> (i, projectedEdgeGeometry env i e)) <$> env^..edges.withIndex
  -- TODO; this is kind of horrible, there must be a nicer way of writing this.

-- | Computes the (projected) LineSegment or HalfLine representing a given edge.
projectedEdgeGeometry               :: (Plane_ plane r, Ord r, Fractional r
                                       , Show plane, Show r
                                       )
                                    => LowerEnvelope' plane
                                    -> EdgeIx (LowerEnvelope' plane)
                                    -> Edge (LowerEnvelope' plane)
                                    -> EdgeGeometry (Point 2 r)
projectedEdgeGeometry env (ui,vi) e = case env^?!vertexAt ui of
    Left unboundedVtx -> case env^?!vertexAt vi of
      Left _  -> error "projectedEdgeGeometry: absurd edge from vInfty to vInfty"
      Right v -> unboundedEdge unboundedVtx v
    Right u           -> case env^?!vertexAt vi of
      Left unboundedVtx -> unboundedEdge unboundedVtx u
      Right v           -> Right $ ClosedLineSegment (u^.location2) (v^.location2)
  where
    unboundedEdge unboundedVtx v = case
--      traceShowWith ("unbEdge", e, "h=",e^.rightPlane, "h'=",e^.leftPlane, "line'",) $
        intersectionLine' (e^.rightPlane) (e^.leftPlane) of
      Just l  -> Left $ HalfLine (v^.location2) (l^.direction)
                 -- edge e is oriented from the lowerId towards the higherId,
                 -- so in particular *from* the unbounded vertex into the bounded vertex
                 -- that means that to construct the halfline we actually wish to
                 -- flip the left and right plane, so that the halfline is directed outwards.
      Nothing -> error "projectedEdgeGeometry: absurd, no intersection between the planes"


--------------------------------------------------------------------------------



-- instance Semigroup (LowerEnvelope' plane) where
--   (LowerEnvelope' u vs) <> (LowerEnvelope' u' vs') = LowerEnvelope' undefined undefined
  -- main idea would be to insert the vertices of vs' into vs this
  -- requires testing if a vertex already exists, and shifting it if
  -- not.  this should run in O(log n) time per edge.
  --
  -- and therefore in O(l log l + r log l) = O(n log n) time.  hmm, I
  -- guess for iterative merging we cannot afford the "l" term.  I
  -- guess we need it only to maintain some Map from location -> id.
  -- so maybe we can maintain that separately.


--------------------------------------------------------------------------------
-- * Convenience functions

-- | Group consecutive elements into non-empty groups.
--
--
-- running time: \(O(n\log n)\)
--
-- >>> groupOnCheap fst [ (1,"foo"), (2,"bar"), (2,"blaa"), (1,"boeez"), (1,"blap"), (4,"blax"), (4,"bleh"), (100,"floep")]
-- [[(1,"foo"),(1,"boeez"),(1,"blap")],[(2,"bar"),(2,"blaa")],[(4,"blax"),(4,"bleh")],[(100,"floep")]]
groupOnCheap  :: (Foldable f, Ord b)
              => (a -> b) -> f a -> [NonEmptyV.NonEmptyVector a]
groupOnCheap f = fmap NonEmptyV.unsafeFromVector . V.groupBy ((==) `on` f) . sortOnCheap f

-- | returns the maximum using the given comparison function
maxBy         :: (t -> t -> Ordering) -> t -> t -> t
maxBy cmp a b = case cmp a b of
                  LT -> b
                  _  -> a


-- | shift the vector by d
shiftR     :: Int -> NonEmptyVector v -> NonEmptyVector v
shiftR d v = let n = length v
             in NonEmptyV.generate1 n $ \i -> v NonEmptyV.! ((i+n+d) `mod` n)

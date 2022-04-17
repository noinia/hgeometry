module Geometry.PlanarSubdivision.Dynamic
  ( splitEdge, unSplitEdge
  , sproutIntoFace
  , splitFace
  ) where

import           Control.Lens
import           Data.Ext
import           Data.Functor.Identity
import           Geometry.Point -- hiding (Vector, head, imap)
import           Geometry.PlanarSubdivision
import           Geometry.PlanarSubdivision.Basic
import           Geometry.PlanarSubdivision.Raw
import           Data.List (sort, sortOn, findIndex)
import           Data.List.NonEmpty (NonEmpty(..))
import qualified Data.List.NonEmpty as NonEmpty
import           Data.PlanarGraph (Dart (Dart), Arc (Arc), VertexId (VertexId), FaceId (FaceId), Direction (Positive, Negative))
import           Data.PlaneGraph (PlaneGraph)
import qualified Data.PlaneGraph as PG
import qualified Data.PlaneGraph.AdjRep as AR (id, vData, fData, faces, Face (..))
import           Data.PlaneGraph.AdjRep hiding (id, vData, faces)
import           Data.Vector (Vector, toList, (//), empty)
import qualified Data.Vector as V

import           Debug.Trace


tracingOn = False

tr :: Show a => String -> a -> a
tr s a | tracingOn = trace ("\9608 " ++ s ++ ": " ++ show a) a
       | otherwise = a


-- TO DO:
-- ADD EDGE JOINING TWO COMPONENTS
-- CREATE NEW COMPONENT (SINGLE VERTEX)
-- DELETIONS


-- | Splits a given edge of a planar subdivision by inserting a new vertex on the edges.
--   Increases #vertices and #edges by 1.
splitEdge
  :: (Show v, Show e, Show f, Show r)
  => VertexId' s
  -> VertexId' s
  -> Point 2 r
  -> v
  -> (e -> (e, e))
  -> PlanarSubdivision s v e f r
  -> PlanarSubdivision s v e f r

splitEdge a b p v f d =
  let (_, la, _) = asLocalV a d
      (_, lb, _) = asLocalV b d
      v' = (freeVertexId d, v)
      fd = freeDart d
      f' (Dart i Positive, e) = ((Dart i Positive, fst $ f e), (fd, snd $ f e))
      f' (Dart i Negative, e) = ((twin fd, fst $ f e), (Dart i Negative, snd $ f e))
  in  tr "splitEdge" $ d & components' %~ fmap (splitEdgeInPlaneGraph la lb p v' f')

-- | Sprouts a new edge from a given vertex into the interior of a given (incident) face.
--   Increases #vertices and #edges by 1.
sproutIntoFace
  :: (Show v, Show e, Show f, Show r)
  => VertexId' s
  -> FaceId' s
  -> Point 2 r
  -> v
  -> (e, e)
  -> PlanarSubdivision s v e f r
  -> PlanarSubdivision s v e f r

sproutIntoFace a f p v (e1, e2) d =
  let [ea] = tr "[ea]" $ filter (\e -> headOf e d == a && leftFace e d == f) $ commonDarts d a f
      (_, la, _) = asLocalV a d
      (_, lc, _) = asLocalV (tailOf ea d) d
      v' = (freeVertexId d, v)
      fd = freeDart d
      e1' = (fd, e1)
      e2' = (twin fd, e2)
  in  tr "sproutIntoFace" $ d & components' %~ fmap (sproutIntoFaceInPlaneGraph la lc p v' (e1', e2'))

-- | Inserts a new edge between two given vertices, adjacent to a common face.
--   Increases #edges and #faces by 1.
splitFace
  :: (Show v, Show e, Show f, Show r)
  => VertexId' s
  -> VertexId' s
  -> (e, e)
  -> (f -> (f, f))
  -> PlanarSubdivision s v e f r
  -> PlanarSubdivision s v e f r

splitFace a b e g d =
  let (ca, _, _) = asLocalV a d
      (cb, _, _) = asLocalV b d
  in if ca == cb then splitFaceSameComponent a b e g d
                 else splitFaceDifferentComponents a b e g d

splitFaceSameComponent a b e g d =
  let fs   = commonFaces d a b
      f | length fs == 1 = tr "f(a)" $ headTrace "splitFaceSameComponent f" fs
        | otherwise = tr "f(b)" $ headTrace "splitFaceSameComponent f" $ filter (not . isOuterFace) fs
      [ea] = tr "[ea]" $ filter (\e -> headOf e d == a && leftFace e d == f) $ commonDarts d a f
      [eb] = tr "[eb]" $ filter (\e -> headOf e d == b && leftFace e d == f) $ commonDarts d b f
      (_, la, _) = asLocalV a d
      (_, lb, _) = asLocalV b d
      (_, lc, _) = asLocalV (tailOf ea d) d
      (_, ld, _) = asLocalV (tailOf eb d) d
      (_, lf, _) :| [] = asLocalF f d
      fd = freeDart d
      e' = ((fd, fst e), (twin fd, snd e))
      tf = freeFaceId d
      g' (ef, x) = ((ef, fst $ g x), (tf, snd $ g x))
  in tr "splitFaceSameComponent" $ d & components' %~ fmap (splitFaceInPlaneGraph (tr "la" la) (tr "lb" lb) (tr "lc" lc) (tr "ld" ld) (tr "lf" lf) e' g')

splitFaceDifferentComponents = undefined


-- | Splits a given edge of a planar subdivision by inserting a new vertex on the edges.
--   Increases #vertices and #edges by 1.
unSplitEdge
  :: (Show v, Show e, Show f, Show r)
  => VertexId' s
  -> ((e, e) -> e)
  -> PlanarSubdivision s v e f r
  -> PlanarSubdivision s v e f r

unSplitEdge b f d =
  let [a, c] = tr "[a, c]" $ toList $ neighboursOf b d
      (_, la, _) = asLocalV a d
      (_, lb, _) = asLocalV b d
      (_, lc, _) = asLocalV c d
      [dab] = filter (\e -> tailOf e d == a) $ commonDarts d a b
      [dcb] = filter (\e -> tailOf e d == c) $ commonDarts d b c
      f' ((di, ei), (dj, ej)) | di == dab = (     dab, f (ei, ej))
                              | di == dcb = (twin dab, f (ei, ej))
                              | otherwise = error "you shouldn't call f' on any other dart"
      -- no longer used: vertex id b and dart id dcb
  in  tr "unSplitEdge" $ d & components' %~ fmap (unSplitEdgeInPlaneGraph la lb lc f')
-- globally, need to restore VertexId and DartIds ???





-- nodig:

freeVertexId :: PlanarSubdivision s v e f r -> VertexId' s
freeDart :: PlanarSubdivision s v e f r -> Dart s
freeFaceId :: PlanarSubdivision s v e f r -> FaceId' s

freeVertexId = VertexId . numVertices
freeDart     = flip Dart Positive . Arc . numEdges
freeFaceId   = FaceId . VertexId . numFaces

components' :: (Show v, Show e, Show f, Show r) => Lens' (PlanarSubdivision s v e f r) (Vector (Component' s v e f r))
type Component' s v e f r = PlaneGraph (Wrap s) (VertexId' s, v) (Dart s, e) (FaceId' s, f) r
components' = lens getComponents' setComponents'

getComponents' :: PlanarSubdivision s v e f r -> Vector (Component' s v e f r)
getComponents' p = fmap (addExtraData p) $ p ^. components

addExtraData :: PlanarSubdivision s v e f r -> Component s r -> Component' s v e f r
addExtraData p c = c & PG.vertexData  . traverse %~ (\i -> (i, p ^. dataOf i))
                     & PG.rawDartData . traverse %~ (\i -> (i, p ^. dataOf i))
                     & PG.faceData    . traverse %~ (\i -> (i, p ^. dataOf i))

setComponents' :: (Show v, Show e, Show f, Show r) => PlanarSubdivision s v e f r -> Vector (Component' s v e f r) -> PlanarSubdivision s v e f r
setComponents' p cs = p & components .~ fmap remExtraData cs
                        & rawVertexData .~ (tr "rawVertexData" . vectorise $ getRawVertexData cs)
                        & rawDartData   .~ (tr "rawDartData"   . vectorise $ getRawEdgeData cs)
                        & rawFaceData   .~ (tr "rawFaceData"   . vectorise $ getRawFaceData cs)

getRawVertexData :: Vector (Component' s v e f r)
                 -> [(VertexId' s, Raw s (VertexId' (Wrap s)) v)]
getRawVertexData = concat . imap (\ci g -> map (\(li, VertexData _ (gi, v)) -> (gi, Raw (toEnum ci) li v)) $ toList $ PG.vertices g) . toList

--getEdgeData :: Vector (Component' s v e f r) -> [(Dart s, (Dart s, e))]
--getEdgeData = map (\(a, b) -> (a, (a, b))) . concatMap (toList . (^. PG.rawDartData)) . toList

getRawEdgeData :: Vector (Component' s v e f r)
               -> [(Dart s, Raw s (Dart (Wrap s)) e)]
getRawEdgeData = concat . imap (\ci g -> map (\(li, (gi, e)) -> (gi, Raw (toEnum ci) li e)) $ toList $ PG.darts g) . toList


--getFaceData :: Vector (Component' s v e f r) -> [(FaceId' s, f)]
--getFaceData = concatMap (toList . (^. PG.faceData)) . toList


-- data RawFace	s f
-- _faceIdx :: !(Maybe (ComponentId s, FaceId' (Wrap s)))
-- _faceDataVal :: !(FaceData (Dart s) f)

-- | Something in this implementation is not right. It makes asLocalF produce an error.
getRawFaceData :: Vector (Component' s v e f r)
               -> [(FaceId' s, RawFace s f)]
getRawFaceData = concat . imap (\ci g -> map (bla ci) $ toList $ PG.faces g) . toList
  where
    bla ci (li, (gi, f)) | isOuterFace gi = (gi, RawFace Nothing (FaceData Empty f))
                         | otherwise      = (gi, RawFace (Just (toEnum ci, li)) (FaceData Empty f))
-- holes are always empty! (where to get them from?)

isOuterFace :: FaceId' s -> Bool
isOuterFace i = fromEnum i == 0

remExtraData :: Component' s v e f r -> Component s r
remExtraData c = c & PG.vertexData  . traverse %~ fst
                   & PG.rawDartData . traverse %~ fst
                   & PG.faceData    . traverse %~ fst


vectorise :: (Enum i, Show i) => [(i, a)] -> Vector a
vectorise vs = V.replicate (length vs) undefined // map (\(i, a) -> (fromEnum i, a)) vs




------------------
-- PLANE GRAPHS --
------------------


-- INSERTIONS --


splitEdgeInPlaneGraph
  :: (Show v, Show e, Show f, Show r)
  => VertexId' s
  -> VertexId' s
  -> Point 2 r
  -> v
  -> (e -> (e, e))
  -> PlaneGraph s v e f r
  -> PlaneGraph s v e f r
-- LET OP! TEST OF a EN b WEL VOORKOMEN!
splitEdgeInPlaneGraph a b p v f
  = tr "splitEdgeInPlaneGraph"
  . PG.fromAdjRep
  . splitEdgeInAdjRep (fromEnum a) (fromEnum b) p v f
  . PG.toAdjRep

sproutIntoFaceInPlaneGraph
  :: (Show v, Show e, Show f, Show r)
  => VertexId' s
  -> VertexId' s
  -> Point 2 r
  -> v
  -> (e, e)
  -> PlaneGraph s v e f r
  -> PlaneGraph s v e f r
sproutIntoFaceInPlaneGraph a c p v e g =
  let ai = fromEnum a
      ci = fromEnum c
  in tr "splitEdgeInPlaneGraph"
   $ PG.fromAdjRep
   $ sproutInAdjRep ai ci p v e
   $ PG.toAdjRep g


-- PG.toAdjRep :: PlaneGraph s v e f r -> Gr (Vtx v e r) (Face f)
-- PG.fromAdjRep :: proxy s -> Gr (Vtx v e r) (Face f) -> PlaneGraph s v e f r


splitFaceInPlaneGraph
  :: (Show v, Show e, Show f, Show r)
  => VertexId' s             -- index van vertex a
  -> VertexId' s             -- index van vertex b
  -> VertexId' s             -- index van vertex c
  -> VertexId' s             -- index van vertex d
  -> FaceId' s               -- index van te splitsen face
  -> (e, e)                  -- extra data voor nieuwe edge ab
  -> (f -> (f, f))           -- functie om face data in twee stukken te knippen
  -> PlaneGraph s v e f r -- input graaf
  -> PlaneGraph s v e f r -- output graaf

splitFaceInPlaneGraph a b c d f e h g =
  let ai = fromEnum a
      bi = fromEnum b
      ci = fromEnum c
      di = fromEnum d
      fi = fromEnum $ tr "fi" $ traceShow (g ^. dataOf f) $ PG.tailOf (PG.boundaryDart f g) g
      fj = fromEnum $ tr "fj" $ PG.headOf (PG.boundaryDart f g) g
      -- ^ boundaryDart seems not working either
  in tr "splitFaceInPlaneGraph"
   $ PG.fromAdjRep
   $ splitFaceInAdjRep ai bi ci di fi fj e h
   $ PG.toAdjRep g


-- DELETIONS --


unSplitEdgeInPlaneGraph
  :: (Show v, Show e, Show f, Show r)
  => VertexId' s
  -> VertexId' s
  -> VertexId' s
  -> ((e, e) -> e)
  -> PlaneGraph s v e f r
  -> PlaneGraph s v e f r

unSplitEdgeInPlaneGraph a b c f
  = tr "unSplitEdgeInPlaneGraph"
  . PG.fromAdjRep
  . unSplitEdgeInAdjRep (fromEnum a) (fromEnum b) (fromEnum c) f
  . PG.toAdjRep


-------------
-- ADJREPS --
-------------

-- Gr
-- adjacencies :: [v]
-- faces :: [f]

-- Vtx
-- id :: Int
-- loc :: Point 2 r
-- adj :: [(Int, e)]
-- vData :: v

-- Face
-- incidentEdge :: (Int, Int)
-- fData :: f

--deriving instance (Show v, Show f) => Show (Gr v f)
--deriving instance (Show v, Show e, Show r) => Show (Vtx v e r)
--deriving instance Show f => Show (Face f)


-- instance {-# OVERLAPS #-} Show (VertexId s Primal) where show i = 'v' : show (fromEnum i)
-- instance {-# OVERLAPS #-} Show (FaceId   s Primal) where show i = 'f' : show (fromEnum i)
-- instance {-# OVERLAPS #-} Show (Dart s, v) where
--   show (Dart (Arc s) Positive, _) = 'd' : show (fromEnum s) ++ "+"
--   show (Dart (Arc s) Negative, _) = 'd' : show (fromEnum s) ++ "-"

-- instance Show f => Show (Face f) where show f = (show $ AR.fData f) ++ "~>" ++ (show $ incidentEdge f)
-- instance (Show e, Show r) => Show (Vtx v e r) where show v = (show $ AR.id v) ++ "~>" ++ (show $ adj v)
-- instance (Show v, Show f) => Show (Gr v f) where show g = "Gr " ++ (show $ adjacencies g) ++ " " ++ (show $ AR.faces g)

-- ik heb:
splitEdgeInAdjRep
  :: (Show v, Show e, Show f, Show r)
  => Int                     -- index van vertex a
  -> Int                     -- index van vertex b
  -> Point 2 r               -- locatie voor nieuwe vertex c
  -> v                       -- extra data voor vertex c
  -> (e -> (e, e))           -- functie om edge data in twee stukken te knippen
  -> Gr (Vtx v e r) (Face f) -- input graaf
  -> Gr (Vtx v e r) (Face f) -- output graaf

splitEdgeInAdjRep a b p v f g =
  let n  = length $ adjacencies g
      -- first find vertices a and b
      oa = headTrace "splitEdgeInAdjRep oa" $ filter ((== a) . AR.id) $ adjacencies g
      ob = headTrace "splitEdgeInAdjRep ob" $ filter ((== b) . AR.id) $ adjacencies g
      os = filter ((lift (&&) (/= a) (/= b)) . AR.id) $ adjacencies g
      -- find edge data
      e1 = snd $ headTrace "splitEdgeInAdjRep e1" $ filter ((== b) . fst) $ adj oa
      e2 = snd $ headTrace "splitEdgeInAdjRep e2" $ filter ((== a) . fst) $ adj ob
      -- create new adjacencies to c in a and b
      na = oa {adj = replace ((== b) . fst) (const (n, fst $ f e1)) $ adj oa}
      nb = ob {adj = replace ((== a) . fst) (const (n, fst $ f e2)) $ adj ob}
      -- create new vertex c
      nc = Vtx {AR.id = n, loc = p, adj = [(a, snd $ f e2), (b, snd $ f e1)], AR.vData = v}
      -- update faces (only if incidentEdge happens to point to ab)
      nf = replace ((== (a, b)) . incidentEdge) (\f -> f {incidentEdge = (a, n)})
         $ replace ((== (b, a)) . incidentEdge) (\f -> f {incidentEdge = (b, n)})
         $ AR.faces g
  in tr "splitEdgeInAdjRep" $ (tr "original" g) {adjacencies = sortOn AR.id $ na : nb : nc : os, AR.faces = nf}


sproutInAdjRep
  :: (Show v, Show e, Show f, Show r)
  => Int                     -- index van vertex a
  -> Int                     -- index van vertex c (andere kant van edge a)
  -> Point 2 r               -- locatie voor nieuwe vertex c
  -> v                       -- extra data voor vertex c
  -> (e, e)                  -- extra data voor nieuwe edge
  -> Gr (Vtx v e r) (Face f) -- input graaf
  -> Gr (Vtx v e r) (Face f) -- output graaf

sproutInAdjRep a c p v e g =
  let n  = length $ adjacencies g
      -- first find vertex a
      oa = tr "oa" $ headTrace "sproutInAdjRep oa" $ filter ((== a) . AR.id) $ adjacencies g
      os = tr "os" $ filter ((/= a) . AR.id) $ adjacencies g
      -- need to find index of c
      fj (Just x) = x
      fj Nothing  = error "splitFaceInAdjRep got Nothing"
      ci = tr "ci" $ fj $ findIndex ((== c) . fst) $ adj oa
      -- create new adjacency to new vertex z in a
      na = tr "na" $ oa {adj = take ci (adj oa) ++ (n, fst e) : drop ci (adj oa)}
      -- create new vertex z
      nz = Vtx {AR.id = n, loc = p, adj = [(a, snd e)], AR.vData = v}
  in tr "splitFaceInAdjRep" $ (tr "original" g) {adjacencies = sortOn AR.id $ na : nz : os}

splitFaceInAdjRep
  :: (Show v, Show e, Show f, Show r)
  => Int                     -- index van vertex a
  -> Int                     -- index van vertex b
  -> Int                     -- index van vertex c (andere kant van edge a)
  -> Int                     -- index van vertex d (andere kant van edge b)
  -> Int                     -- index van face edge start
  -> Int                     -- index van face edge eind
  -> (e, e)                  -- extra data voor nieuwe edge ab
  -> (f -> (f, f))           -- functie om face data in twee stukken te knippen
  -> Gr (Vtx v e r) (Face f) -- input graaf
  -> Gr (Vtx v e r) (Face f) -- output graaf

-- is it easier to split a vertex than a face?

splitFaceInAdjRep a b c d u v e f g =
  let
      -- first find vertices a and b
      oa = tr "oa" $ headTrace "splitFaceInAdjRep oa" $ filter ((== a) . AR.id) $ adjacencies g
      ob = tr "ob" $ headTrace "splitFaceInAdjRep ob" $ filter ((== b) . AR.id) $ adjacencies g
      os = tr "os" $ filter ((lift (&&) (/= a) (/= b)) . AR.id) $ adjacencies g
      -- insert new adjacency between a and b
      fj (Just x) = x
      fj Nothing  = error "splitFaceInAdjRep got Nothing"
      -- need to find indices c and d!
      ci = tr "ci" $ fj $ findIndex ((== c) . fst) $ adj oa
      di = tr "di" $ fj $ findIndex ((== d) . fst) $ adj ob
      -- insert new adjacencies to each other in a and b
      na = tr "na" $ oa {adj = take ci (adj oa) ++ (b, fst e) : drop ci (adj oa)}
      nb = tr "nb" $ ob {adj = take di (adj ob) ++ (a, snd e) : drop di (adj ob)}
      -- find the face that is incident to both a and b
      i  = tr "i"  $ fj $ findIndex ((== (u, v)) . incidentEdge) $ AR.faces g
      fd = tr "fd" $ AR.fData $ AR.faces g !! i
      ef = tr "ef" $ take i (AR.faces g) ++ drop (i + 1) (AR.faces g)
      f1 = tr "f1" $ AR.Face {incidentEdge = (a, b), AR.fData = fst $ f fd}
      f2 = tr "f2" $ AR.Face {incidentEdge = (b, a), AR.fData = snd $ f fd}
  in tr "splitFaceInAdjRep" $ (tr "original" g) {adjacencies = sortOn AR.id $ na : nb : os, AR.faces = ef ++ [f1, f2]}





unSplitEdgeInAdjRep
  :: (Show v, Show e, Show f, Show r)
  => Int                     -- index van vertex a
  -> Int                     -- index van vertex b (te verwijderen)
  -> Int                     -- index van vertex c
  -> ((e, e) -> e)           -- functie om edge data te mergen
  -> Gr (Vtx v e r) (Face f) -- input graaf
  -> Gr (Vtx v e r) (Face f) -- output graaf

unSplitEdgeInAdjRep a b c f g =
  let n  = length $ adjacencies g
      -- first find vertices a, b and c
      oa = head $ filter ((== a) . AR.id) $ adjacencies g
      ob = head $ filter ((== b) . AR.id) $ adjacencies g
      oc = head $ filter ((== c) . AR.id) $ adjacencies g
      os = filter ((\i -> i /= a && i /= b && i /= c) . AR.id) $ adjacencies g
      -- find edge data
      eab = snd $ head $ filter ((== b) . fst) $ adj oa
      eba = snd $ head $ filter ((== a) . fst) $ adj ob
      ebc = snd $ head $ filter ((== c) . fst) $ adj ob
      ecb = snd $ head $ filter ((== b) . fst) $ adj oc
      -- create new adjacencies between a and c
      na = oa {adj = replace ((== b) . fst) (const (c, f (eab, ebc))) $ adj oa}
      nc = oc {adj = replace ((== b) . fst) (const (a, f (ecb, eba))) $ adj oc}
      nv = sortOn AR.id $ na : nc : os
      -- update faces (only if incidentEdge happens to point to ab or bc)
      nf = replace ((== (a, b)) . incidentEdge) (\f -> f {incidentEdge = (a, c)})
         $ replace ((== (b, a)) . incidentEdge) (\f -> f {incidentEdge = (c, a)})
         $ replace ((== (b, c)) . incidentEdge) (\f -> f {incidentEdge = (a, c)})
         $ replace ((== (c, b)) . incidentEdge) (\f -> f {incidentEdge = (c, a)})
         $ AR.faces g
      -- restore consecutive numbering: replace vertex n-1 by b
      ng = replaceIndex (n - 1) b $ (tr "original" g) {adjacencies = nv, AR.faces = nf}
  in tr "unSplitEdgeInAdjRep" $ ng

-- Gr
-- adjacencies :: [v]
-- faces :: [f]

-- Vtx
-- id :: Int
-- loc :: Point 2 r
-- adj :: [(Int, e)]
-- vData :: v

-- Face
-- incidentEdge :: (Int, Int)
-- fData :: f

replaceIndex :: Int -> Int -> Gr (Vtx v e r) (Face f) -> Gr (Vtx v e r) (Face f)
replaceIndex i j g = g { adjacencies = map (replaceIndexAdjacency i j) $ adjacencies g
                       , AR.faces    = map (replaceIndexFace      i j) $ AR.faces    g
                       }

replaceIndexAdjacency :: Int -> Int -> Vtx v e r -> Vtx v e r
replaceIndexAdjacency i j v = v { AR.id = if AR.id v == i then j else AR.id v
                                , adj   = replace ((== i) . fst) (set _1 j) $ adj v
                                }

replaceIndexFace :: Int -> Int -> Face f -> Face f
replaceIndexFace i j f | fst (incidentEdge f) == i = f {incidentEdge = incidentEdge f & set _1 j}
                       | snd (incidentEdge f) == i = f {incidentEdge = incidentEdge f & set _2 j}
                       | otherwise = f


-------------
-- HELPERS --
-------------

replace :: (a -> Bool) -> (a -> a) -> [a] -> [a]
replace f g = map $ replace' f g

replace' :: (a -> Bool) -> (a -> a) -> a -> a
replace' f g x | f x = g x
               | otherwise = x

lift :: (a -> b -> c) -> (d -> a) -> (d -> b) -> d -> c
lift f g h x = f (g x) (h x)



headTrace :: String -> [a] -> a
headTrace s xs | null xs   = error $ s ++ ": head of empty list"
               | otherwise = head xs
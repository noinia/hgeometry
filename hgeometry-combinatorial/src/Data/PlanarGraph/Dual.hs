--------------------------------------------------------------------------------
-- |
-- Module      :  Data.PlanarGraph
-- Copyright   :  (C) Frank Staals
-- License     :  see the LICENSE file
-- Maintainer  :  Frank Staals
--
-- Data type for representing connected planar graphs. This module contains
-- everything that has to do with the dual graph (i.e. computing it/ operations
-- on faces etc.)
--------------------------------------------------------------------------------
module Data.PlanarGraph.Dual where

import           Control.Lens hiding ((.=))
import           Data.PlanarGraph.Core
import           Data.PlanarGraph.Dart
import qualified Data.Vector as V
import           Data.Maybe (fromMaybe)

--------------------------------------------------------------------------------

--------------------------------------------------------------------------------
-- $setup
-- >>> :{
-- let dart i s = Dart (Arc i) (read s)
--     (aA:aB:aC:aD:aE:aG:_) = take 6 [Arc 0..]
--     myGraph :: PlanarGraph () Primal () String ()
--     myGraph = planarGraph [ [ (Dart aA Negative, "a-")
--                             , (Dart aC Positive, "c+")
--                             , (Dart aB Positive, "b+")
--                             , (Dart aA Positive, "a+")
--                             ]
--                           , [ (Dart aE Negative, "e-")
--                             , (Dart aB Negative, "b-")
--                             , (Dart aD Negative, "d-")
--                             , (Dart aG Positive, "g+")
--                             ]
--                           , [ (Dart aE Positive, "e+")
--                             , (Dart aD Positive, "d+")
--                             , (Dart aC Negative, "c-")
--                             ]
--                           , [ (Dart aG Negative, "g-")
--                             ]
--                           ]
-- :}
--
--
-- This represents the following graph. Note that the graph is undirected, the
-- arrows are just to indicate what the Positive direction of the darts is.
--
-- ![myGraph](docs/Data/PlanarGraph/testG.png)


-- | Enumerate all faces in the planar graph
faces' :: PlanarGraph s w v e f -> V.Vector (FaceId s w)
faces' = fmap FaceId . vertices' . _dual

-- | All faces with their face data.
faces   :: PlanarGraph s w v e f -> V.Vector (FaceId s w, f)
faces g = V.zip (faces' g) (g^.faceData)

-- | The face to the left of the dart
--
-- >>> leftFace (dart 1 "+1") myGraph
-- FaceId 1
-- >>> leftFace (dart 1 "-1") myGraph
-- FaceId 2
-- >>> leftFace (dart 2 "+1") myGraph
-- FaceId 2
-- >>> leftFace (dart 0 "+1") myGraph
-- FaceId 0
--
-- running time: \(O(1)\).
leftFace     :: Dart s -> PlanarGraph s w v e f -> FaceId s w
leftFace d g = FaceId . headOf d $ _dual g


-- | The face to the right of the dart
--
-- >>> rightFace (dart 1 "+1") myGraph
-- FaceId 2
-- >>> rightFace (dart 1 "-1") myGraph
-- FaceId 1
-- >>> rightFace (dart 2 "+1") myGraph
-- FaceId 1
-- >>> rightFace (dart 0 "+1") myGraph
-- FaceId 1
--
-- running time: \(O(1)\).
rightFace     :: Dart s -> PlanarGraph s w v e f -> FaceId s w
rightFace d g = FaceId . tailOf d $ _dual g


-- | Get the next edge along the face
--
-- running time: \(O(1)\).
nextEdge   :: Dart s -> PlanarGraph s w v e f -> Dart s
nextEdge d = nextIncidentEdge d . _dual

-- | Get the previous edge along the face
--
-- running time: \(O(1)\).
prevEdge :: Dart s -> PlanarGraph s w v e f -> Dart s
prevEdge d = prevIncidentEdge d . _dual


-- | Gets a dart bounding this face. I.e. a dart d such that the face lies to
-- the right of the dart.
boundaryDart   :: FaceId s w -> PlanarGraph s w v e f -> Dart s
boundaryDart f = V.head . boundary f
-- TODO: make sure that this is indeed to the right.

-- | The darts bounding this face, for internal faces in clockwise order, for
-- the outer face in counter clockwise order.
--
--
-- running time: \(O(k)\), where \(k\) is the output size.
boundary              :: FaceId s w -> PlanarGraph s w v e f -> V.Vector (Dart s)
boundary (FaceId v) g = incidentEdges v $ _dual g


-- | Generates the darts incident to a face, starting with the given dart.
--
--
-- \(O(k)\), where \(k\) is the number of darts reported
boundary'     :: Dart s -> PlanarGraph s w v e f -> V.Vector (Dart s)
boundary' d g = fromMaybe (error "boundary'")  . rotateTo d $ boundary (rightFace d g) g
  where
    rotateTo     :: Eq a => a -> V.Vector a -> Maybe (V.Vector a)
    rotateTo x v = f <$> V.elemIndex x v
      where
        f i = let (a,b) = V.splitAt i v  in b <> a


-- | The vertices bounding this face, for internal faces in clockwise order, for
-- the outer face in counter clockwise order.
--
--
-- running time: \(O(k)\), where \(k\) is the output size.
boundaryVertices     :: FaceId s w -> PlanarGraph s w v e f -> V.Vector (VertexId s w)
boundaryVertices f g = fmap (flip tailOf g) $ boundary f g

-- -- | Gets the next dart along the face
-- nextDart     :: Dart s -> PlanarGraph s w v e f -> Dart s
-- nextDart d g = f rightFace e

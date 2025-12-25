{-# LANGUAGE TemplateHaskell #-}
module Line.PointLocation.Type
  ( PointLocationDS(PointLocationDS), subdivision, vrStructure, outerFaceIx
  , PointLocationDS'
  , Subdiv
  ) where


import Control.Lens
import HGeometry.PlaneGraph.Connected
import HGeometry.VerticalRayShooting.PersistentSweep

--------------------------------------------------------------------------------

type Subdiv v e f = CPlaneGraph () v e f

-- | The Point Location Data structure
data PointLocationDS v e f  =
  PointLocationDS { _subdivision :: Subdiv v e f
                  , _vrStructure :: VerticalRayShootingStructure (e :+ DartIx (Subdiv v e f))
                  , _outerFaceIx :: FaceIx (Subdiv v e f)
                  }

makeLenses ''PointLocationDS

instance HasFaces' (PointLocationDS v e f) where
  type FaceIx (PointLocationDS v e f) = FaceIx (Subdiv v e f)
  type Face   (PointLocationDS v e f) = Face   (Subdiv v e f)
  faceAt f = subdivision .> faceAt f
  {-# INLINE faceAt #-}
  numFaces = numFaces . view subdivision
  {-# INLINE numFaces #-}

--------------------------------------------------------------------------------

type PointLocationDS' r line =
  PointLocationDS (Point 2 r :+ Seq.Seq (Point 2 r))
                  (ViewL1 (ClosedLineSegment (Point 2 r) :+ Maybe line))
                  ()

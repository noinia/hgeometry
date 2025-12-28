{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE UndecidableInstances #-}
--------------------------------------------------------------------------------
-- |
-- Module      :  HGeometry.PointLocation.Type
-- Copyright   :  (C) Frank Staals
-- License     :  see the LICENSE file
-- Maintainer  :  Frank Staals
--
-- The data type representing a point location data structure
--
--------------------------------------------------------------------------------
module HGeometry.PointLocation.Type
  ( PointLocationDS(PointLocationDS), subdivision, vrStructure, outerFaceIx
  , PointLocationDS'
  , Subdiv
  ) where


import Control.Lens
import HGeometry.Ext
import HGeometry.Point
import HGeometry.LineSegment
import HGeometry.PlaneGraph.Connected
import HGeometry.Properties
import HGeometry.VerticalRayShooting.PersistentSweep

--------------------------------------------------------------------------------

type Subdiv v e f = CPlaneGraph () v e f

-- | The Point Location Data structure
data PointLocationDS v e f  =
  PointLocationDS { _subdivision :: Subdiv v e f
                  , _vrStructure :: VerticalRayShootingStructure
                                    (ClosedLineSegment v :+ DartIx (Subdiv v e f))
                  , _outerFaceIx :: FaceIx (Subdiv v e f)
                  }

makeLenses ''PointLocationDS

deriving instance (Show v, Show e, Show f, Show (NumType v)) => Show (PointLocationDS v e f)

-- TODO: make the edge type in the VRS something that is strict in extra

instance HasFaces' (PointLocationDS v e f) where
  type FaceIx (PointLocationDS v e f) = FaceIx (Subdiv v e f)
  type Face   (PointLocationDS v e f) = Face   (Subdiv v e f)
  faceAt f = subdivision .> faceAt f
  {-# INLINE faceAt #-}
  numFaces = numFaces . view subdivision
  {-# INLINE numFaces #-}

--------------------------------------------------------------------------------

type PointLocationDS' r line =
  PointLocationDS (Point 2 r)
                  (ClosedLineSegment (Point 2 r) :+ Maybe line)
                  ()

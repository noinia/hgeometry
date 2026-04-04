{-# LANGUAGE TemplateHaskell #-}
module HalfPlane.Cone
  ( Cone(Cone), apex, leftBoundaryVector, rightBoundaryVector
  , leftBoundary, rightBoundary
  ) where

import HGeometry.Point
import HGeometry.Line
import HGeometry.Vector
import HGeometry.Ext
import HGeometry.HalfLine
import HGeometry.Properties
import Control.Lens

--------------------------------------------------------------------------------

-- | A Cone
data Cone r edge = Cone { _apex                :: Point 2 r
                        , _leftBoundaryVector  :: Vector 2 r :+ edge
                        -- ^ the interior of the cone is to the right
                        , _rightBoundaryVector :: Vector 2 r :+ edge
                        -- ^ the interior of the cone is to the left
                        }
                 deriving (Show,Eq,Ord)

makeLenses ''Cone

type instance NumType   (Cone r edge) = r
type instance Dimension (Cone r edge) = 2

-- | Get the left boundary as a HalfLine starting at the apex.
leftBoundary   :: Cone r edge -> HalfLine (Point 2 r) :+ edge
leftBoundary c = (c^.leftBoundaryVector)&core %~ (HalfLine (c^.apex))

-- | Get the left boundary as a HalfLine starting at the apex.
rightBoundary   :: Cone r edge -> HalfLine (Point 2 r) :+ edge
rightBoundary c = (c^.leftBoundaryVector)&core %~ (HalfLine (c^.apex))

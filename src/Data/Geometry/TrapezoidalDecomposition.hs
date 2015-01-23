module Data.Geometry.TrapezoidalDecomposition where


import Data.Ext

import Data.ExplicitOrdSet
import Data.Vector


import Data.Geometry.Point
import Data.Geometry.Interval
import Data.Geometry.Line


--------------------------------------------------------------------------------

type TrapezoidId = Int


-- | We represent a topless trapezoid by its bottom edge.
data ToplessTrapezoid p r = ToplessTrapezoid { _tTrapId :: TrapezoidId
                                             , _tBottom :: LineSegment 2 p r
                                             }

data Trapezoid p r = Trapezoid { _trapId :: TrapezoidId
                               , _bottom :: LineSegment 2 p r
                               , _top    :: LineSegment 2 p r
                               }


-- | A vertical slab of trapezoids
type VSlab pe e r = ExpSet (ToplessTrapezoid pe r :+ e)


type TrapezoidalDecomposition pe e r = GenericDecomposition Vector pe e r


-- | A Generic Trapezoidal Decomposition
newtype GenericDecomposition xCont pe e r = TrapezoidalDecomposition (xCont (VSlab pe e r))
-- xCont : the type of container we use to separate on x-coordinate

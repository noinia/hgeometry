module Data.Geometry.Envelope where


import qualified Data.Map as M
import Control.Lens


-- | Data in the envelope
data EnvelopeData e r = EnvelopeData { _yValue          :: !r
                                     , _incomingSegment :: !e
                                     }
                        deriving (Show,Eq)

-- | Strict pair
data SP a b = SP !a !b deriving (Show,Eq,Ord)

-- | The x-coordinates are the keys in the map, the y-coordinates are in the
-- envelope data. The type e represents the type 'segemnts' that host the edges.
data Envelope e r = Envelope { _leftmostVertex   :: !(SP r (EnvelopeData r (Maybe e)))
                                                    -- ^ The leftmost vertex, which
                                                    -- may have an incoming edge
                                                    -- that extends to -infty
                             , _interiorVertices :: !(M.Map r (EnvelopeData r e))
                             , _rightmostEdge    :: !(Maybe e)
                                                    -- ^ the rightmost edge
                                                    -- in the lower envelope
                                                    -- that extends to +infty
                             }
                    deriving (Show,Eq)

vertices :: Evenlope e r -> Point 2 r
vertices = undefined

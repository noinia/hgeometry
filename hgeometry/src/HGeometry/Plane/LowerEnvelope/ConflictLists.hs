module HGeometry.LowerEnvelope.ConflictLists
  ( Prism(Prism), corners, conflictList
  , computePrisms
  ) where

import           Control.Lens
import           HGeometry.LowerEnvelope.BatchedPointLoc
import qualified HGeometry.LowerEnvelope.Naive as Naive
import           HGeometry.LowerEnvelope.Sample
import           HGeometry.LowerEnvelope.Type

--------------------------------------------------------------------------------

-- computeConflictLists ::

data PrismF f g r = Prism { _corners      :: f (Vertex r)
                          , _conflictList :: g (Plane r)
                          }

type Prism = PrismF (Vector 3)
-- typically we just store three vertices

corners :: Lens (PrismF f g r) (PrismF f' g r) (f (Vertex r)) (f' (Vertex r))
corners = lens _corners (\p cs -> p { _corners = cs })

conflictList :: Lens (PrismF f g r) (PrismF f g' r) (g (Plane r)) (g' (Plane r))
conflictList = lens _conflictList (\p cl -> p { _conflictList = cl })

--------------------------------------------------------------------------------

delta :: Rational
delta = (1/8)

--------------------------------------------------------------------------------

-- | Compute the prisms and their conflict lists
computePrisms :: f (Plane r) -- ^ The full set of planes
              -> f (Plane r) -- ^ The subset
              -> m ( LowerEnvelope [] Boxed.Vector r
                   , g (Prism h r)
                   )
computePrisms hs ss
  | s <= nDelta = computePrisms' hs ss ss
  | otherwise   = sample p >>= computePrisms' hs ss
  where
    n = length hs
    s = length ss
    nDelta = n ^^^ delta
    r = min s nDelta
    p = probability r s

-- | Compute the prisms and their conflict lists
computePrisms :: f (Plane r) -- ^ The full set of planes
              -> f (Plane r) -- ^ The subset S
              -> f (Plane r) -- ^ The subset R
              -> m ( LowerEnvelope [] Boxed.Vector r
                   , g (Prism h r)
                   )
computePrisms hs ss rs = undefined
  where
    envR  = Naive.lowerEnvelope rs  -- triangulated -> convert to prisms
    envR' = computeConflictLists hs envR


--------------------------------------------------------------------------------

data WithConflicts h plane = WithConflicts plane (h plane)

computeConflictLists :: f (Plane r) -- ^ full set of planes
                     -> g (Prism g r) -- ^ the prisms of the lower env
                     -> g (Prism (WithConflicts Boxed.Vector) r)
computeConflictLists hs prisms = undefined -- batchedPointLocation queries planes
  where
    queries = undefined -- fmap dualize hs
    planes = undefined -- foldMap (dualize.  planeOfAPrism) prisms
  -- TODO: dualize + call batchedPointLocation

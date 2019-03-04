{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE UndecidableInstances #-}
--------------------------------------------------------------------------------
-- |
-- Module      :  Algorithms.Geometry.LinearProgramming.LP2DRIC
-- Copyright   :  (C) Frank Staals
-- License     :  see the LICENSE file
-- Maintainer  :  Frank Staals
--
-- 2D Linear programming in expected linear time.
--
--------------------------------------------------------------------------------
module Algorithms.Geometry.LinearProgramming.LP2DRIC where

import           Control.Lens
import           Control.Monad (foldM)
import           Control.Monad.Random.Class
import           Data.Ext
import qualified Data.Foldable as F
import           Data.Geometry.HalfLine
import           Data.Geometry.HalfSpace
import           Data.Geometry.Boundary
import           Data.Geometry.HyperPlane
import           Data.Geometry.Line
import           Data.Geometry.Point
import           Data.Geometry.Properties
import           Data.Geometry.Vector
import           Data.Maybe (mapMaybe)
import           Data.Vinyl
import           Data.Vinyl.CoRec
import           System.Random.Shuffle


--------------------------------------------------------------------------------

-- | Data type representing the solution to a linear program
data GLPolution p = NoSolution
                  | Single !p
                  | UnBounded
                  deriving (Show,Eq,Functor)


type LPSolution d r = GLPolution (Point d r)

data LinearProgram d r = LinearProgram { _objective   :: !(Vector d r)
                                       , _constraints :: [HalfSpace d r]
                                       }
makeLenses ''LinearProgram

deriving instance Arity d             => Functor (LinearProgram d)
deriving instance (Arity d, Show r)   => Show    (LinearProgram d r)
deriving instance (Arity d, Eq r)     => Eq      (LinearProgram d r)

--------------------------------------------------------------------------------

-- | State during the LP algo
data LPState d r = LPState { _obj     :: !(Vector d r)
                           , _seen    :: [HalfSpace d r]
                           , _current :: !(Point d r)
                           }
makeLenses ''LPState

deriving instance (Arity d, Show r)   => Show    (LPState d r)
deriving instance (Arity d, Eq r)     => Eq      (LPState d r)

--------------------------------------------------------------------------------

-- | collect all intersection points on the boundary l of h. If we return a Nothing there
-- is no solution. Just [] indicates that somehow this halfplane is contained in all other
-- halfplanes.
collectOn     :: (Ord r, Fractional r)
              => Vector 2 r
              -> HalfSpace 2 r
              -> [HalfSpace 2 r]
              -> Maybe [Point 2 r]
collectOn o h = sequence . mapMaybe collect . map (l `intersect`)
  where
    l = h^.boundingPlane._asLine

    collect   :: Intersection (Line 2 r) (HalfSpace 2 r) -> Maybe (Maybe (Point 2 r))
    collect r = match r $
         (H $ \NoIntersection -> Just Nothing)
      :& (H $ \hl             -> Just $ Just (hl^.startPoint))
      :& (H $ \_              -> Nothing)
      :& RNil

-- | Let l be the boundary of h, and assume that we know that the new point in
-- the common intersection must lie on h, try to find this point. In
-- partiuclar, we find the 'minimum' point in the s^.direction vector. The
-- funtion returns Nothing if no such point exists, i.e. if there is no point
-- on l that is contained in all halfspaces.
minimumOn     :: (Ord r, Fractional r) => LPState 2 r -> HalfSpace 2 r -> Maybe (Point 2 r)
minimumOn s h = fmap (F.minimumBy cmp) . collectOn (s^.obj) h $ s^.seen
  where
    l = h^.boundingPlane._asLine

    -- take the normal with respect to the objective vector. We can then test
    -- if a is smaller (i.e. better) than b by testing if a occurs in the
    -- "left" halfspace defined by b and the normal.
    Line _ n = perpendicularTo (Line origin $ s^.obj)
    cmp a b = case a `inHalfSpace` (HalfSpace $ HyperPlane b n) of
                Inside     -> LT
                OnBoundary -> EQ
                Outside    -> GT

-- | What we do when we get a new halfplane h
step                                   :: (Fractional r, Ord r)
                                       => LPState 2 r -> HalfSpace 2 r
                                       -> Maybe (LPState 2 r)
step s h | (s^.current) `intersects` h = Just $ s&seen     %~ (h:)
         | otherwise                   = (\p -> s&seen     %~ (h:)
                                                 &current .~ p)
                                        <$> minimumOn s h

-- TODO: Fix this



findWith o h []
  | o `isPerpendicularTo` (h^.boundingPlane._asLine) = Single $ h^.boundingPlane.inPlane
  | otherwise                                        = UnBounded
findWith o h (h':hs) = match (h^.boundingPlane._asLine) `intersect` h' $
      (H $ \Nothing -> if (h'^.boundingPlane._asLine) `intersects` h
                      then findWith o h' -- no solution on h ; h is redundant
                      else insert h' $ findWith o h hs)  -- add h'
   :& (H $ \hl -> undefined)
   :& (H $ \_ -> undefined)
   :& RNil





initialize          :: (Ord r, Fractional r)
                    => Vector 2 r -> [HalfSpace d r]
                    -> GLPolution (LPState d r, [HalfSpace d r])
initialize _ []     = NoSolution
initialize o (h:hs) = findWith o h hs


-- -- | Given a vector v and a halfplane h, return the extremal point in direction
-- -- v that lies in h.
-- findBest v h =


  -- ( LPState o [h,h2] undefined
--                         (h^.boundingPlane `intersect` h2^.boundingPlane)
--                          , hs)
-- initialize _ _         = error "initialize: unbounded LP"



-- (\h' -> (h'^.halfSpaceBoundary) `intersect`) h^.halfSpaceBoundary


--------------------------------------------------------------------------------





-- | Solve a linear program
solveLinearProgram :: MonadRandom m => LinearProgram 2 r -> m (LPSolution 2 r)
solveLinearProgram = undefined


-- | O(n) expected time
solveBoundedLinearProgram                      :: MonadRandom m
                                               => LinearProgram 2 r -> m (Maybe (Point 2 r))
solveBoundedLinearProgram (LinearProgram o cs) =
  (solveBoundedLinearProgram' . LinearProgram o . F.toList) <$> shuffle cs



solveBoundedLinearProgram'    :: (Ord r, Fractional r)
                              => LinearProgram 2 r -> Maybe (Point 2 r)
solveBoundedLinearProgram' lp = let (s,hs) = initialize lp
                                in foldM step s hs

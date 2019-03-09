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
-- is no solution. Just [] indicates that somehow this halfspace h is contained in all other
-- halfspaces.
collectOn     :: (Ord r, Fractional r)
              => Vector 2 r
              -> HalfSpace 2 r
              -> [HalfSpace 2 r]
              -> Maybe [HalfLine 2 r]
collectOn _ h = sequence . mapMaybe collect . map (l `intersect`)
  where
    l = h^.boundingPlane._asLine

    collect   :: Intersection (Line 2 r) (HalfSpace 2 r) -> Maybe (Maybe (Point 2 r))
    collect r = match r $
         (H $ \NoIntersection -> Just Nothing)
      :& (H $ \hl             -> Just $ Just hl)
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
    -- a is smaller (i.e. better) than b if it occurs in the halfspace defined
    -- by the objective vector through point b.
    cmp a b = case a `inHalfSpace` (HalfSpace $ HyperPlane b $ s^.obj) of
                Inside     -> LT
                OnBoundary -> EQ
                Outside    -> GT
  -- TODO: FIx the minimumOn function, this is kind of nonsense


-- | What we do when we get a new halfplane h
step                                   :: (Fractional r, Ord r)
                                       => LPState 2 r -> HalfSpace 2 r
                                       -> Maybe (LPState 2 r)
step s h | (s^.current) `intersects` h = Just $ s&seen     %~ (h:)
         | otherwise                   = (\p -> s&seen     %~ (h:)
                                                 &current .~ p)
                                        <$> minimumOn s h




--------------------------------------------------------------------------------

-- | Assumes that the first two constraints somehow bound the solution in direction c.
initialize :: forall r. (Ord r, Fractional r)
           => LinearProgram 2 r -> (LPState 2 r, [HalfSpace 2 r])
initialize (LinearProgram c (m1:m2:hs)) = (LPState c [m1,m2] p, hs)
  where
    Just p = asA @(Point 2 r)
           $ (m1^.boundingPlane._asLine) `intersect` (m2^.boundingPlane._asLine)

--------------------------------------------------------------------------------

-- | Solve a linear program
solveLinearProgram :: MonadRandom m => LinearProgram 2 r -> m (LPSolution 2 r)
solveLinearProgram = undefined


-- | Solvess a bounded linear program in 2d. There are at least two constraints
-- m1,m2 that bound the solution; they are assumed to be the first two contraints.
--
-- Returns Nothing if there is sno solution.
--
--
-- \(O(n)\) expected time
--
--
solveBoundedLinearProgram                      :: (MonadRandom m, Ord r, Fractional r)
                                               => LinearProgram 2 r -> m (Maybe (Point 2 r))
solveBoundedLinearProgram (LinearProgram c (m1:m2:hs)) =
  solveBoundedLinearProgram'  . LinearProgram c . ([m1,m2] ++) . F.toList <$> shuffle hs
  -- (solveBoundedLinearProgram' . LinearProgram o . F.toList) <$> shuffle cs



solveBoundedLinearProgram'    :: (Ord r, Fractional r)
                              => LinearProgram 2 r -> Maybe (Point 2 r)
solveBoundedLinearProgram' lp = let (s,hs) = initialize lp
                                in (^.current) <$> foldM step s hs


--------------------------------------------------------------------------------




-- TODO: Fix this


-- findWith' o h ho hs =


-- findWith o h []
--   | o `isPerpendicularTo` (h^.boundingPlane._asLine) = Single $ h^.boundingPlane.inPlane
--   | otherwise                                        = UnBounded
-- findWith o h hs = case commonSpace oppo of
--                     []     ->
--                     (ho:_) | ho `intersects1` hX -> findWith' o hX ho hs'
--                              -- observe that we can ignore the rest of the planes in
--                              -- parra and oppo from our computation, since they
--                              -- contain hX and hO, respectively anyway.
--                            | otherwise          -> NoSolution
--                              -- appararently ho and hX are disjoint, so there is no
--                              -- point in the common intersection of (all halfspaces)
--   where
--     -- parra: parrallel halfspaces where the space is on the same "side" as h
--     -- oppo: parallel halfspaces with opposite side as h
--     -- hs' : non-parallel
--     STR parra oppo hs' = foldr (extractParallel h) (STR [] [] []) hs
--     Just hX = commonSpace (h:parra)

--     intersects1 a b = (a^.boundingPlane.inPlane) `intersects` b



-- -- | Given two halfspaces a and h, test if the bounding plane of h is parallel to a, with
-- -- the spaces on the same side (first arg of the result), opposite (second), or
-- -- neither (third).
-- extractParallel                    :: (Arity d, Ord r, Fractional r)
--                                    => HalfSpace d r
--                                    -> HalfSpace d r
--                                    -> STR [HalfSpace d r] [HalfSpace d r] [HalfSpace d r]
--                                    -> STR [HalfSpace d r] [HalfSpace d r] [HalfSpace d r]
-- extractParallel a h (STR ps os hs) = case scalarMultiple n v of
--                                         Nothing              -> STR ps     os     (h:hs)
--                                         Just lam | lam < 0   -> STR ps     (h:os) hs
--                                                  | otherwise -> STR (h:ps) os     hs

--   where
--     n = a^.boundingPlane.normalVec
--     v = h^.boundingPlane.normalVec



-- -- | Given a bunch of halfpsaces that have parallel bounding planes and whose
-- -- normal vectors are oriented in the same direciton, reports the common
-- -- halfspace, i.e. the "smallest" common intersection.
-- --
-- -- this function returns a Nothing only if the initial list is empty
-- commonSpace :: forall d r. (Ord r, Fractional r) => [HalfSpace d r] -> Maybe (HalfSpace d r)
-- commonSpace = \case
--     []     -> Nothing
--     (h:hs) -> foldr trim (Just h) hs
--   where
--     trim h acc = acc >>= \h' -> let p = h^.boundingPlane.inPlane in
--                                 if p `intersects` h' then h' else h
--                                 -- We use that the boudning planes of h and h' are parallel
--                                 -- so if h' contains the inPlane point of h then h'
--                                 -- contains h. If not then it is the other way around







--   match (h^.boundingPlane._asLine) `intersect` h' $
--       (H $ \Nothing -> if (h'^.boundingPlane._asLine) `intersects` h
--                       then findWith o h' -- no solution on h ; h is redundant
--                       else insert h' $ findWith o h hs)  -- add h'
--    :& (H $ \hl -> undefined)
--    :& (H $ \_ -> undefined)
--    :& RNil





-- initialize          :: (Ord r, Fractional r)
--                     => Vector 2 r -> [HalfSpace d r]
--                     -> GLPolution (LPState d r, [HalfSpace d r])
-- initialize _ []     = NoSolution
-- initialize o (h:hs) = findWith o h hs


-- -- -- | Given a vector v and a halfplane h, return the extremal point in direction
-- -- -- v that lies in h.
-- -- findBest v h =


--   -- ( LPState o [h,h2] undefined
-- --                         (h^.boundingPlane `intersect` h2^.boundingPlane)
-- --                          , hs)
-- -- initialize _ _         = error "initialize: unbounded LP"



-- -- (\h' -> (h'^.halfSpaceBoundary) `intersect`) h^.halfSpaceBoundary

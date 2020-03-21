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
module Algorithms.Geometry.LinearProgramming.LP2DRIC( solveBoundedLinearProgram
                                                    , solveBoundedLinearProgram'

                                                    , maximumOn
                                                    , oneDLinearProgramming
                                                    , commonIntersection
                                                    , cmpHalfPlane
                                                    ) where

import           Algorithms.Geometry.LinearProgramming.Types
import           Control.Lens
import           Control.Monad (foldM)
import           Control.Monad.Random.Class
import           Data.Ext
import qualified Data.Foldable as F
import           Data.Geometry.Boundary
import           Data.Geometry.HalfLine
import           Data.Geometry.HalfSpace
import           Data.Geometry.HyperPlane
import           Data.Geometry.Line
import           Data.Geometry.Point
import           Data.Geometry.Properties
import           Data.Geometry.Vector
import qualified Data.List.NonEmpty as NonEmpty
import           Data.Maybe (mapMaybe)
import           Data.Util
import           Data.Vinyl
import           Data.Vinyl.CoRec
import           System.Random.Shuffle

--------------------------------------------------------------------------------

-- | Solve a linear program
solveLinearProgram :: MonadRandom m => LinearProgram 2 r -> m (LPSolution 2 r)
solveLinearProgram = undefined


-- | Solves a bounded linear program in 2d. Returns Nothing if there is no
-- solution.
--
-- pre: The linear program is bounded, meaning that *the first two constraints*
-- m1,m2 make sure th the there is no arbitrarily large/good
-- solution. I..e. these halfspaces bound the solution in the c direction.
--
-- (note that if there is only one constraint, m1, the assumption that the LP
-- is bounded means that the contraint must be perpendicular to the objective
-- direction. Hence, any point on the bounding plane is a solution, and they
-- are all equally good.)
--
-- \(O(n)\) expected time
--
--
solveBoundedLinearProgram                      :: (MonadRandom m, Ord r, Fractional r)
                                               => LinearProgram 2 r -> m (Maybe (Point 2 r))
solveBoundedLinearProgram (LinearProgram c hs') = case hs' of
    []         -> pure Nothing
    [m1]       -> pure $ Just (m1^.boundingPlane.inPlane)
    (m1:m2:hs) -> solveBoundedLinearProgram' . LinearProgram c . ([m1,m2] ++) . F.toList
                  <$> shuffle hs


-- | Solves a bounded linear program (like 'solveBoundedLinearProgram')
-- assuming that the first two constraints [m1,m2] make sure the solutions is
-- bounded, and the other constraints already have been shuffled.
solveBoundedLinearProgram'    :: (Ord r, Fractional r)
                              => LinearProgram 2 r -> Maybe (Point 2 r)
solveBoundedLinearProgram' lp = let (s,hs) = initialize lp
                                in (^.current) <$> foldM step s hs

--------------------------------------------------------------------------------

-- | State during the LP algo
data LPState d r = LPState { _obj     :: !(Vector d r)
                           , _seen    :: [HalfSpace d r]
                           , _current :: !(Point d r)
                           }

deriving instance (Arity d, Show r)             => Show    (LPState d r)
deriving instance (Arity d, Eq r, Fractional r) => Eq      (LPState d r)

obj     :: Lens' (LPState d r) (Vector d r)
obj     = lens _obj     (\(LPState _ s p) o -> LPState o s p)
seen    :: Lens' (LPState d r) [HalfSpace d r]
seen    = lens _seen    (\(LPState o _ p) s -> LPState o s p)
current :: Lens' (LPState d r) (Point d r)
current = lens _current (\(LPState o s _) p -> LPState o s p)


-- | What we do when we get a new halfspace h
step                                   :: (Fractional r, Ord r)
                                       => LPState 2 r -> HalfSpace 2 r
                                       -> Maybe (LPState 2 r)
step s h | (s^.current) `intersects` h = Just $ s&seen     %~ (h:)
         | otherwise                   = (\p -> s&seen     %~ (h:)
                                                 &current .~ p)
                                        <$> maximumOn s (h^.boundingPlane._asLine)

--------------------------------------------------------------------------------

-- | collect all intersecting halflines on the boundary l of h. If we return a Nothing there
-- is no solution. Just [] indicates that somehow this halfspace h is contained in all other
-- halfspaces.
collectOn     :: (Ord r, Fractional r)
              => Line 2 r
              -> [HalfSpace 2 r]
              -> Maybe [HalfLine 2 r]
collectOn l = sequence . mapMaybe collect . map (l `intersect`)
  where
    collect   :: Intersection (Line 2 r) (HalfSpace 2 r) -> Maybe (Maybe (HalfLine 2 r))
    collect r = match r $
         (H $ \NoIntersection -> Just Nothing)
      :& (H $ \hl             -> Just $ Just hl)
      :& (H $ \_              -> Nothing)
      :& RNil


-- | Given a vector v and two points a and b, determine which is smaller in direction v.
cmpHalfPlane       :: (Ord r, Num r, Arity d)
                   => Vector d r -> Point d r -> Point d r -> Ordering
cmpHalfPlane v a b = case a `inHalfSpace` (HalfSpace $ HyperPlane b $ v) of
                       Inside     -> GT
                       OnBoundary -> EQ
                       Outside    -> LT

type OneOrTwo a = Either a (Two a)

flatten :: OneOrTwo a -> [a]
flatten = either (:[]) (\(Two a b) -> [a,b])

-- | Computes the common intersection of a nonempty list of halfines that are
-- all colinear with the given line l.
--
-- We return either the two halflines that prove that there is no counter
-- example or we return one or two points that form form the boundary points of
-- the range in which all halflines intersect.
commonIntersection                :: (Ord r, Num r, Arity d)
                                  => Line d r
                                  -> NonEmpty.NonEmpty (HalfLine d r :+ a)
                                  -> Either (Two ((HalfLine d r :+ a)))
                                            (OneOrTwo (Point d r :+ a))
commonIntersection (Line _ v) hls = case (nh,ph) of
     (Nothing,Nothing) -> error "absurd; this case cannot occur"
     (Nothing, Just p) -> Right . Left . extract $ p
     (Just n, Nothing) -> Right . Left . extract $ n
     (Just n, Just p)  -> case cmpHalfPlane' v n p of
                            LT -> Left $ Two n p
                            EQ -> Right . Left . extract $ p
                            GT -> Right . Right $ Two (extract p) (extract n)
  where
    extract = over core (^.startPoint)
    (pos,neg) = NonEmpty.partition (\hl -> hl^.core.halfLineDirection == v) $ hls
    ph = maximumBy' (cmpHalfPlane' v) pos
    nh = maximumBy' (flip $ cmpHalfPlane' v) neg

    cmpHalfPlane' vv a b = cmpHalfPlane vv (a^.core.startPoint) (b^.core.startPoint)

commonIntersection'               :: (Ord r, Num r, Arity d)
                                  => Line d r
                                  -> NonEmpty.NonEmpty (HalfLine d r)
                                  -> [Point d r]
commonIntersection' l hls = either (const []) (map (^.core) . flatten)
                          $ commonIntersection l (ext <$> hls)


-- | maximum of a list using a given comparison ; if the list is empty returns Nothing
maximumBy'     :: (a -> a -> Ordering) -> [a] -> Maybe a
maximumBy' cmp = \case
  [] -> Nothing
  xs -> Just $ F.maximumBy cmp xs


-- | One dimensional linear programming on lines embedded in \(\mathbb{R}^d\).
--
-- Given an objective vector c, a line l, and a collection of half-lines hls that are all
-- sublines of l (i.e. halfspaces *on* l), compute if there is a point inside
-- all these halflines. If so, we actually return the one that maximizes c.
--
-- running time: \(O(n)\)
oneDLinearProgramming         :: (Ord r, Num r, Arity d)
                              => Vector d r -> Line d r -> [HalfLine d r] -> Maybe (Point d r)
oneDLinearProgramming c l hls = do
                                  hls'       <- NonEmpty.nonEmpty hls
                                  let candidates = commonIntersection' l hls'
                                  maximumBy' (cmpHalfPlane c) candidates


-- | Let l be the boundary of h, and assume that we know that the new point in
-- the common intersection must lie on h, try to find this point. In
-- partiuclar, we find the 'maximum' point in the s^.direction vector. The
-- funtion returns Nothing if no such point exists, i.e. if there is no point
-- on l that is contained in all halfspaces.
--
-- Note that this is essentially one dinsional LP
maximumOn     :: (Ord r, Fractional r) => LPState 2 r -> Line 2 r -> Maybe (Point 2 r)
maximumOn s l = do hls  <- collectOn l $ s^.seen
                   oneDLinearProgramming (s^.obj) l hls
  -- if collectOn returns a Just [] it means there is a point in the common intersection,
  -- however it does not lie on the boundary of h. This violates our input assumption
  -- thus if this would happen we can safely return a Nothing


--------------------------------------------------------------------------------

-- | Assumes that the first two constraints somehow bound the solution in direction c.
initialize :: forall r. (Ord r, Fractional r)
           => LinearProgram 2 r -> (LPState 2 r, [HalfSpace 2 r])
initialize (LinearProgram c (m1:m2:hs)) = (LPState c [m1,m2] p, hs)
  where
    Just p = asA @(Point 2 r)
           $ (m1^.boundingPlane._asLine) `intersect` (m2^.boundingPlane._asLine)



--------------------------------------------------------------------------------

-- | Let \(n(h)\) denote the normal of the line bounding a halfspace \(h\).
--
-- This function tries to Find an "unbounded direction" \(d\). If such a
-- direction \(d\) exits the LP is unbounded, and we can produce evidence of
-- this in the form of a half-line in direction \(d\).
--
-- More formally, we are looking for a direction \(d\) so that
-- - \(c \cdot d > 0\), and
-- - \(d \cdot n(h) \geq 0\), wherefor every half space \(h\).
--
findD                      :: (Ord r, Fractional r)
                           => LinearProgram 2 r -> Maybe (Vector 2 r)
findD (LinearProgram c hs) = do hls <- collectOn nl hs'
                                d   <- toVec <$> oneDLinearProgramming v nl hls
                                       -- the direction v here does not really matter
                                if c `dot` d > 0 then pure d
                                                 else Nothing
  where
    -- we interpret the points on nl as directions w.r.t the origin
    nl@(Line _ v) = perpendicularTo (Line (origin .+^ c) c)
    hs' = map toHL hs

    -- every halfspace creates an allowed set of directions, modelled by a
    -- half-line on nl
    toHL h = let n              = h^.boundingPlane.normalVec
             in undefined


-- | Either finds an unbounded Haflline, or evidence the two halfspaces that provide
-- evidence that no solution exists
findUnBoundedHalfLine :: LinearProgram 2 r -> Either (Two (HalfSpace 2 r)) (HalfLine 2 r)
findUnBoundedHalfLine = undefined -- use findD then find the starting point








    -- ok; we can normalize the "y-coord" of our d-vector by taking the
    -- length of vector c. (I should probably square the c to avoid having to take square roots.)

  -- but how about the "x-coord"; we need to express that as the "lambda coord" along nl



-- ok so the global plan is: Find a vector d



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

--------------------------------------------------------------------------------
-- |
-- Module      :  HGeometry.Interval.Internal
-- Copyright   :  (C) Frank Staals
-- License     :  see the LICENSE file
-- Maintainer  :  Frank Staals
--
-- Intervals
--
--------------------------------------------------------------------------------
{-# LANGUAGE UndecidableInstances #-}
module HGeometry.Interval.Internal
  ( Interval(Interval,ClosedInterval,OpenInterval)
  , ClosedInterval, OpenInterval
  , asClosedInterval, asOpenInterval, asAnInterval
  , isIntervalOfType
  , Interval_x_IntervalIntersection(..)
  ) where

import Control.Lens
import Control.Monad (guard)
import HGeometry.Intersection
import HGeometry.Interval.Class
import HGeometry.Interval.EndPoint ()
import HGeometry.Point
import HGeometry.Properties
import HGeometry.Vector
import Text.Read

--------------------------------------------------------------------------------
-- | Data type representing intervals
newtype Interval endPoint r = MkInterval (Vector 2 (endPoint r))
  deriving newtype (Eq,Ord)

type instance NumType (Interval endPoint r) = r

-- | Construct an interval
pattern Interval     :: endPoint r -> endPoint r -> Interval endPoint r
pattern Interval s t = MkInterval (Vector2 s t)
{-# COMPLETE Interval #-}


-- | Cosed intervals (using a boxed representation)
type ClosedInterval r = Interval (EndPoint Closed) r

-- | Construct a closed interval
pattern ClosedInterval     :: r -> r -> ClosedInterval r
pattern ClosedInterval s t = Interval (ClosedE s) (ClosedE t)
{-# COMPLETE ClosedInterval #-}


-- | Open intervals (using a boxed representation)
type OpenInterval r   = Interval (EndPoint Open) r

-- | Construct an open ended interval
pattern OpenInterval     :: r -> r -> OpenInterval r
pattern OpenInterval s t = Interval (OpenE s) (OpenE t)
{-# COMPLETE OpenInterval #-}


instance Functor endPoint => Functor (Interval endPoint) where
  fmap f (Interval s t) = Interval (fmap f s) (fmap f t)
instance Foldable endPoint => Foldable (Interval endPoint) where
  foldMap f (Interval s t) = foldMap f s <> foldMap f t
instance Traversable endPoint => Traversable (Interval endPoint) where
  traverse f (Interval s t) = Interval <$> traverse f s <*> traverse f t

instance ( EndPoint_ (endPoint r), IxValue (endPoint r) ~ r
         ) => HasStart (Interval endPoint r) r where
  start = startPoint._endPoint
instance ( EndPoint_ (endPoint r), IxValue (endPoint r) ~ r
         ) => HasEnd (Interval endPoint r) r where
  end = endPoint._endPoint

instance HasStartPoint (Interval endPoint r) (endPoint r) where
  startPoint = lens (\(Interval s _) -> s) (\(Interval _ t) s -> Interval s t)

instance HasEndPoint (Interval endPoint r) (endPoint r) where
  endPoint = lens (\(Interval _ t) -> t) (\(Interval s _) t -> Interval s t)

type instance StartPointOf (Interval endPoint r) = endPoint r
type instance EndPointOf   (Interval endPoint r) = endPoint r

instance ( EndPoint_ (endPoint r), IxValue (endPoint r) ~ r
         ) => IntervalLike_ (Interval endPoint r) r where
  mkInterval = Interval

instance ( EndPoint_ (endPoint r), IxValue (endPoint r) ~ r
         ) => Interval_ (Interval endPoint r) r where


instance ClosedInterval_ (ClosedInterval r) r

instance OpenInterval_ (OpenInterval r) r

--------------------------------------------------------------------------------

instance ( Show (endPoint r)
         ) => Show (Interval endPoint r) where
  showsPrec k (Interval s t) = showParen (k > app_prec) $
                                 showString "Interval "
                               . showsPrec (app_prec+1) s
                               . showChar ' '
                               . showsPrec (app_prec+1) t

-- | application precedence
app_prec :: Int
app_prec = 10

instance Read (endPoint r) => Read (Interval endPoint r) where
  readPrec = parens $ (prec app_prec $ do
                          Ident "Interval" <- lexP
                          p <- step readPrec
                          q <- step readPrec
                          return (Interval p q))


--------------------------------------------------------------------------------

-- | Try to interpret the interval as an OpenInterval, i.e. with both endpoints open
asOpenInterval   :: Interval_ interval r => interval -> Maybe (OpenInterval r)
asOpenInterval i
  | isIntervalOfType Open Open i = Just $ OpenInterval (i^.start) (i^.end)
  | otherwise                    = Nothing
{-# INLINE asOpenInterval #-}

-- | Try to interpret the interval as a ClosedInterval, i.e. with both endpoints Closed
asClosedInterval   :: Interval_ interval r => interval -> Maybe (ClosedInterval r)
asClosedInterval i
  | isIntervalOfType Closed Closed i = Just $ ClosedInterval (i^.start) (i^.end)
  | otherwise                        = Nothing
{-# INLINE asClosedInterval #-}

-- | convert into an interval whose endpoints are explicitly tagged.
asAnInterval   :: Interval_ interval r => interval -> Interval AnEndPoint r
asAnInterval i = mkInterval (asAnEndPoint $ i^.startPoint) (asAnEndPoint $ i^.endPoint)

-- | Test if the interval is of a particular type
isIntervalOfType       :: Interval_ interval r
                       => EndPointType -- ^ startPoint type
                       -> EndPointType -- ^ endPoint type
                       -> interval -> Bool
isIntervalOfType s t i = endPointType (i^.startPoint) == s &&  endPointType (i^.endPoint) == t
{-# INLINE isIntervalOfType #-}

--------------------------------------------------------------------------------


-- type instance IntersectionOf r (Interval endPoint r) = [NoIntersection, r]
-- GHC does not understand the r here cannot be 'Interval endPoint r' itself :(













-- type instance IntersectionOf (Interval point r) (Interval point r)
--   = [NoIntersection, Interval point r]

-- instance Ord r => Interval a r `HasIntersectionWith` Interval b r
-- instance Ord r => Interval point r `IsIntersectableWith` Interval point r where

--   nonEmptyIntersection = defaultNonEmptyIntersection

--   (GInterval r) `intersect` (GInterval s) = match (r' `intersect` s') $
--          H (\NoIntersection -> coRec NoIntersection)
--       :& H (\(Range l u)    -> coRec . GInterval $ Range (l&unEndPoint %~ g)
--                                                          (u&unEndPoint %~ g) )
--       :& RNil
--     where
--       r' :: Range (Arg r (r :+ Either a b))
--       r' = fmap (\(x :+ a) -> Arg x (x :+ Left a))  r
--       s' :: Range (Arg r (r :+ Either a b))
--       s' = fmap (\(x :+ b) -> Arg x (x :+ Right b)) s

--       g (Arg _ x) = x

type instance Intersection (Point 1 r) (Interval endPoint r) = Maybe r

instance Ord r => Point 1 r `HasIntersectionWith` ClosedInterval r where
  (Point1 q) `intersects` int = int^.start <= q && q <= int^.end
instance Ord r => Point 1 r `HasIntersectionWith` OpenInterval r where
  (Point1 q) `intersects` int = int^.start < q && q < int^.end

instance Ord r
         => Point 1 r `HasIntersectionWith` Interval AnEndPoint r where
  (Point1 q) `intersects` int = compare' (int^.startPoint.to endPointType) (int^.start) q
                             && compare' (int^.endPoint.to endPointType)   q            (int^.end)
    where
      compare' = \case
        Open   -> (<)
        Closed -> (<=)

-- | intersect implementation
intersectImpl  :: HasIntersectionWith (Point 1 r) i => Point 1 r -> i -> Maybe r
q@(Point1 q') `intersectImpl` int | q `intersects` int = Just q'
                                  | otherwise          = Nothing

instance Ord r => Point 1 r `IsIntersectableWith` ClosedInterval r where
  intersect = intersectImpl
instance Ord r => Point 1 r `IsIntersectableWith` OpenInterval r where
  intersect = intersectImpl
instance Ord r
         => Point 1 r `IsIntersectableWith` Interval AnEndPoint r where
  intersect = intersectImpl


-- type instance Intersection (ClosedInterval r) (ClosedInterval r) =
--   Maybe (IntersectionOf (ClosedInterval r) (ClosedInterval r))

-- data instance IntersectionOf (ClosedInterval r) (ClosedInterval r) =
--     ClosedInterval_x_ClosedInterval_Point     !r
--   | ClosedInterval_x_ClosedInterval_Contained !(ClosedInterval r)
--   | ClosedInterval_x_ClosedInterval_Partial   !(ClosedInterval r)

-- deriving stock instance (Eq r) => Eq (IntersectionOf (ClosedInterval r) (ClosedInterval r) )
-- deriving stock instance (Show r) => Show (IntersectionOf (ClosedInterval r) (ClosedInterval r) )

--------------------------------------------------------------------------------

-- | Data type representing intersections of intervals of the same type
data Interval_x_IntervalIntersection r interval =
    Interval_x_Interval_Point     !r
  | Interval_x_Interval_Contained !interval
  | Interval_x_Interval_Partial   !interval
  deriving (Show,Eq,Ord,Functor,Foldable,Traversable)

type instance Intersection (Interval endPoint r) (Interval endPoint r) =
  Maybe (Interval_x_IntervalIntersection r (Interval endPoint r))

-- | Implementation of interval intersection
intersectIntervalImpl     :: ( Ord r, IxValue (endPoint r) ~ r
                             , EndPoint_ (endPoint r)
                             )
                          => Interval endPoint r -> Interval endPoint r
                          -> Maybe (Interval_x_IntervalIntersection r (Interval endPoint r))
intersectIntervalImpl a b = case (a^.start) `compareIntervalExact` b of
    Before   -> case (a^.end) `compareIntervalExact` b of
        Before   -> Nothing
        OnStart  -> do guard $ isClosed (a^.endPoint) && isClosed (b^.startPoint)
                       pure $ Interval_x_Interval_Point (a^.end)
        Interior -> Just partialBA
        OnEnd
          | isClosed (a^.endPoint) || isOpen (b^.endPoint) ->
                          Just $ Interval_x_Interval_Contained b
          | otherwise  -> Just partialBA
            -- if b's endpoint is "contained" in that of b, i.e. if
            -- a is closed, or if b is open, then b is fully contained.
        After    -> Just $ Interval_x_Interval_Contained b

    OnStart
      | isClosed (b^.endPoint) || isOpen (a^.endPoint) ->
                      aInteriorCase $ Interval_x_Interval_Contained a
      | otherwise  -> aInteriorCase partialAB

    Interior -> aInteriorCase $ Interval_x_Interval_Contained a
    OnEnd    -> do guard $ isClosed (a^.startPoint) && isClosed (b^.endPoint)
                   pure $ Interval_x_Interval_Point (a^.start)
    After    -> Nothing -- by invariant, a^.end >= a.start, so they don't intersect
  where
    isClosed = (== Closed) . endPointType
    isOpen = not . isClosed
    endPointContained = isClosed (a^.endPoint) || not (isClosed (b^.endPoint))

    -- the case when the startpoint of a is contained in B or coincides witht the start of B
    -- the argument is how to actually construct a contained interval
    aInteriorCase contained = case (a^.end) `compare` (b^.end) of
      LT -> Just contained
      EQ
        | isClosed (b^.endPoint) || isOpen (a^.endPoint) -> Just contained
        | otherwise                                      -> Just partialAB
      GT -> Just partialAB

    partialAB = Interval_x_Interval_Partial $ Interval (a^.startPoint) (b^.endPoint)
    partialBA = Interval_x_Interval_Partial $ Interval (b^.startPoint) (a^.endPoint)

--------------------------------------------------------------------------------
-- * Closed Interval

----------------------------------------
-- ** HasIntersection


instance ( Ord r
         , IxValue (endPoint r) ~ r, EndPoint_ (endPoint r)
         ) => ClosedInterval r `HasIntersectionWith` Interval endPoint r where
  {-# SPECIALIZE instance Ord r => ClosedInterval r `HasIntersectionWith` ClosedInterval r #-}
  {-# SPECIALIZE instance Ord r => ClosedInterval r `HasIntersectionWith` OpenInterval r #-}
  {-# SPECIALIZE instance Ord r => ClosedInterval r `HasIntersectionWith` Interval AnEndPoint r #-}
  intA `intersects` intB = case (intA^.start) `compareInterval` intB of
    LT -> case (intA^.end) `compareInterval` intB of
            LT -> False
            EQ -> True
            GT -> True
    EQ -> True
    GT -> False -- by invariant, intA^.end >= intA.start, so they don't intersect

----------------------------------------
-- ** IsIntersectable

instance Ord r => ClosedInterval r `IsIntersectableWith` ClosedInterval r where
  intersect = intersectIntervalImpl

  -- intA `intersect` intB = case (intA^.start) `compareInterval` intB of
  --     LT -> case (intA^.end) `compareInterval` intB of
  --             LT -> Nothing
  --             EQ -> Just $ mkInterval' (intB^.start) (intA^.end)
  --             GT -> Just $ ClosedInterval_x_ClosedInterval_Contained intB
  --               -- intB is fully contained
  --     EQ -> case (intA^.end) `compareInterval` intB of
  --             LT -> error "intersecting intervals; invariant failed, intA should be swapped?"
  --             EQ -> Just $ ClosedInterval_x_ClosedInterval_Contained intA
  --             GT -> Just $ if intA^.start == intB^.start then
  --                            ClosedInterval_x_ClosedInterval_Contained intB
  --                          else mkInterval' (intA^.start) (intB^.end)
  --     GT -> Nothing -- by invariant, intA^.end > intA.start, so they don't intersect
  --   where
  --     mkInterval' l r
  --       | l == r    = ClosedInterval_x_ClosedInterval_Point l
  --       | otherwise = ClosedInterval_x_ClosedInterval_Partial $ ClosedInterval l r



--------------------------------------------------------------------------------
-- * Open intervals

instance Ord r => OpenInterval r  `HasIntersectionWith` OpenInterval r where
  intA `intersects` intB = case (intA^.start) `compareInterval` intB of
    LT -> case (intA^.end) `compareInterval` intB of
            LT -> False
            EQ -> True -- IntB is non-empty
            GT -> intB^.start < intB^.end -- intB needs to be non-empty
    EQ -> True -- IntB is non-empty
    GT -> False -- by invariant, intA^.end >= intA.start, so they don't intersect
  {-# INLINE intersects #-}

instance Ord r => OpenInterval r  `HasIntersectionWith` ClosedInterval r where
  intersects intA intB = intersects intB intA
  {-# INLINE intersects #-}

instance Ord r => OpenInterval r  `HasIntersectionWith` Interval AnEndPoint r where
  intA `intersects` intB = case (intA^.start) `compareInterval` intB of
      LT -> case (intA^.end) `compareInterval` intB of
              LT -> False
              EQ -> intA^.end > intB^.start -- we do need to be propertly larger
              GT -> isIntervalOfType Open Open intB
                    `implies` (intB^.start < intB^.end) -- intB needs to be non-empty
      EQ -> True -- IntB is non-empty
      GT -> False -- by invariant, intA^.end >= intA.start, so they don't intersect
    where
      p `implies` q = not p || q
  {-# INLINE intersects #-}

--------------------------------------------------------------------------------
-- * Mixed

instance Ord r => Interval AnEndPoint r `HasIntersectionWith` Interval AnEndPoint r where
  intA `intersects` intB = case (intA^.start) `compareIntervalExact` intB of
      Before   -> case (intA^.end) `compareIntervalExact` intB of
                    Before   -> False
                    OnStart  -> isClosed (intA^.endPoint) && isClosed (intB^.startPoint)
                    Interior -> True
                    OnEnd    -> True
                    After    -> True
      OnStart  -> True  -- by invariant, both a and B are non-empty, so they share
                        -- their endpoint they both contain the point start+eps
      Interior -> True -- by invariant intA is non-empty, so intA^.start+eps inside intB
      OnEnd    -> isClosed (intA^.startPoint) && isClosed (intB^.endPoint)
      After    -> False -- by invariant, intA^.end >= intA.start, so they don't intersect
    where
      isClosed = (== Closed) . endPointType
  {-# INLINE intersects #-}

instance Ord r => Interval AnEndPoint r `HasIntersectionWith` ClosedInterval r where
  intersects intA intB = intersects intB intA
  {-# INLINE intersects #-}

instance Ord r => Interval AnEndPoint r `HasIntersectionWith` OpenInterval r where
  intersects intA intB = intersects intB intA
  {-# INLINE intersects #-}

-- instance Ord r => Interval AnEndPoint r `HasIntersectionWith` ClosedInterval r where
--   intersects intA intB = intersects intB intA
--   {-# INLINE intersect #-}


         -- Interval (AnEndPoint Open 2) (AnEndPoint Closed 18)
         -- Interval (AnEndPoint Open (-26)) (AnEndPoint Closed 2)

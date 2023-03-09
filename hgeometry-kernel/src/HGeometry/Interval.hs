--------------------------------------------------------------------------------
-- |
-- Module      :  HGeometry.Interval
-- Copyright   :  (C) Frank Staals
-- License     :  see the LICENSE file
-- Maintainer  :  Frank Staals
--
-- Intervals
--
--------------------------------------------------------------------------------
{-# LANGUAGE UndecidableInstances #-}
module HGeometry.Interval
  ( Interval(Interval,ClosedInterval,OpenInterval)
  , ClosedInterval, OpenInterval
  , module HGeometry.Interval.Class
  , IntersectionOf(..)
  ) where

import Control.Lens
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

type instance EndPointOf (Interval endPoint r) = endPoint r

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

instance Ord r => (Point 1 r) `HasIntersectionWith` (ClosedInterval r) where
  (Point1 q) `intersects` int = int^.start <= q && q <= int^.end
instance Ord r => (Point 1 r) `HasIntersectionWith` (OpenInterval r) where
  (Point1 q) `intersects` int = int^.start < q && q < int^.end

instance Ord r
         => (Point 1 r) `HasIntersectionWith` (Interval AnEndPoint r) where
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


type instance Intersection (ClosedInterval r) (ClosedInterval r) =
  Maybe (IntersectionOf (ClosedInterval r) (ClosedInterval r))

data instance IntersectionOf (ClosedInterval r) (ClosedInterval r) =
    ClosedInterval_x_ClosedInterval_Point     !r
  | ClosedInterval_x_ClosedInterval_Contained !(ClosedInterval r)
  | ClosedInterval_x_ClosedInterval_Partial   !(ClosedInterval r)

deriving stock instance (Eq r) => Eq (IntersectionOf (ClosedInterval r) (ClosedInterval r) )
deriving stock instance (Show r) => Show (IntersectionOf (ClosedInterval r) (ClosedInterval r) )


instance (Ord r) => ClosedInterval r `HasIntersectionWith` ClosedInterval r where
  intA `intersects` intB = case (intA^.start) `compareInterval` intB of
    LT -> case (intA^.end) `compareInterval` intB of
            LT -> False
            EQ -> True
            GT -> True
    EQ -> True
    GT -> False -- by invariant, intA^.end > intA.start, so they don't intersect

instance ( Ord r
         ) => ClosedInterval r `IsIntersectableWith` ClosedInterval r where
  intA `intersect` intB = case (intA^.start) `compareInterval` intB of
      LT -> case (intA^.end) `compareInterval` intB of
              LT -> Nothing
              EQ -> Just $ mkInterval' (intB^.start) (intA^.end)
              GT -> Just $ ClosedInterval_x_ClosedInterval_Contained intB
                -- intB is fully contained
      EQ -> case (intA^.end) `compareInterval` intB of
              LT -> error "intersecting intervals; invariant failed, intA should be swapped?"
              EQ -> Just $ ClosedInterval_x_ClosedInterval_Contained intA
              GT -> Just $ if intA^.start == intB^.start then
                             ClosedInterval_x_ClosedInterval_Contained intB
                           else mkInterval' (intA^.start) (intB^.end)
      GT -> Nothing -- by invariant, intA^.end > intA.start, so they don't intersect
    where
      mkInterval' l r
        | l == r    = ClosedInterval_x_ClosedInterval_Point l
        | otherwise = ClosedInterval_x_ClosedInterval_Partial $ ClosedInterval l r
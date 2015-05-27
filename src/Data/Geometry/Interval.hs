{-# LANGUAGE TemplateHaskell  #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE DeriveFunctor  #-}
{-# LANGUAGE FunctionalDependencies  #-}
{-# LANGUAGE ScopedTypeVariables  #-}
module Data.Geometry.Interval-- (
                             -- -- * 1 dimensional Intervals
                             --   Interval(..)
                             -- , Intersection(..)

                             -- -- * querying the start and end of intervals
                             -- , HasStart(..), HasEnd(..)
                             -- -- * Working with intervals
                             -- , width
                             -- , inInterval
                             -- )
       where

import Control.Lens(makeLenses, (^.),(%~),(.~),(&), Lens')
import Data.Ext
import Data.Geometry.Properties
import Data.Maybe(fromJust)
import Data.Proxy
import Data.Semigroup
import Data.Vinyl
import Data.Vinyl.Functor
import Frames.CoRec

--------------------------------------------------------------------------------

data EndPointType = Open | Closed deriving (Show, Eq, Ord)

newtype EndPoint (t :: EndPointType) a = EndPoint { _unEndPoint :: a }
                                       deriving (Show,Read,Eq,Ord
                                                ,Num,Enum,Bounded,Real
                                                ,Integral,Fractional,Floating
                                                ,Functor
                                                )
makeLenses ''EndPoint

openEndPoint :: a -> EndPoint Open a
openEndPoint = EndPoint

closedEndPoint :: a -> EndPoint Closed a
closedEndPoint = EndPoint



data Range l u a = GRange { _lower :: EndPoint l a
                          , _upper :: EndPoint u a
                          } deriving (Eq,Ord,Functor)
makeLenses ''Range

instance (AlwaysTruePrettyShow l u, Show a) => Show (Range l u a) where
  show r = "Range " ++ prettyShow r



-- | Range :: a -> a -> Range l u a
-- takes care of automatically unwrapping the 'EndPoint' types.
pattern Range l u = GRange (EndPoint l) (EndPoint u)

type SymRange t = Range t t


type GInterval s t a r = Range s t (r :+ a)


type SymInterval t a r = GInterval t t a r

type OpenInterval  a r  = SymInterval Open   a r
type ClosedInterval a r = SymInterval Closed a r



class EndPointIn t where
  include :: EndPoint t a -> Bool


instance EndPointIn Open   where include _ = False
instance EndPointIn Closed where include _ = True

openRange :: a -> a -> SymRange Open a
openRange = Range

closedRange :: a -> a -> SymRange Closed a
closedRange = Range

openInterval :: (r :+ a) -> (r :+ a) -> OpenInterval a r
openInterval = Range

closedInterval :: (r :+ a) -> (r :+ a) -> ClosedInterval a r
closedInterval = Range


type InRangeTrue l u = (EndPointIn l, EndPointIn u)

-- | Test if a value lies in a range.
--
-- >>> 1 `inRange` (openRange 0 2)
-- True
-- >>> 1 `inRange` (openRange 0 1)
-- False
-- >>> 1 `inRange` (closedRange 0 1)
-- True
-- 1 `inRange` (closedRange 1 1)
-- True
-- 10 `inRange` (openRange 1 10)
-- False
-- 10 `inRange` (closedRange 0 1)
-- False
inRange                  :: (Ord a, InRangeTrue l u) => a -> Range l u a -> Bool
x `inRange` (GRange l u) = case ((l^.unEndPoint) `compare` x, x `compare` (u^.unEndPoint)) of
    (_, GT) -> False
    (GT, _) -> False
    (LT,LT) -> True
    (LT,EQ) -> include u -- depends on only u
    (EQ,LT) -> include l -- depends on only l
    (EQ,EQ) -> include l || include u -- depends on l and u

-- | Test if a value lies in an interval. Note that the difference between
--  inInterval and inRange is that the extra value is *not* used in the
--  comparison with inInterval, whereas it is in inRange.
inInterval :: (Ord r, InRangeTrue l u) => r -> GInterval l u a r -> Bool
x `inInterval` r = x `inRange` (fmap (^.core) r)





-- overlap :: Range l u a -> Range l' u' a -> Intersection1 (Range l u a) (Range l' u' a)
-- (GRange a@(EndPoint a') b@(EndPoint b') `overlap` (GRange c@(EndPoint c') d@(EndPoint d')) =





type family IntersectionEndPoints l u l' u' a :: [*] where
  IntersectionEndPoints l  u l  u  a = '[ Range l  u a ]
  IntersectionEndPoints l  u l  u' a =  [ Range l  u a, Range l  u' a ]
  IntersectionEndPoints l  u l' u  a =  [ Range l  u a, Range l' u  a ]
  IntersectionEndPoints l  u l' u' a =  [ Range l  u a, Range l  u' a
                                        , Range l' u a, Range l' u' a ]



type instance IntersectionOf (Range l u a) (Range l' u' a) =
  NoIntersection ': IntersectionEndPoints l u l' u' a


type AlwaysTrueIntersectionRanges l u l' u' a =
  ( AlwaysTrueIntersection (Range l u a) (Range l' u' a)
  , TestEndPointType (RestrictOpen l l'), TestEndPointType (RestrictOpen u u')
  , (Range (RestrictOpen l l') (RestrictOpen u u')) a ∈
    IntersectionOf (Range l u a) (Range l' u' a)
  )

instance ( AlwaysTrueIntersectionRanges l u l' u' a
         , Ord a
         ) => IsIntersectableWith (Range l u a) (Range l' u' a) where

  nonEmptyIntersection = defaultNonEmptyIntersection

  r `intersect` s = case (sel el) `compare` (sel eu) of
      LT -> undefined
      EQ -> undefined
      GT -> coRec NoIntersection
    where
      el = argEndPoint max (r^.lower) (s^.lower)
      eu = argEndPoint min (r^.upper) (s^.upper)

      -- isO = either isOpenEndPoint isOpenEndPoint

      sel :: Either (EndPoint t a) (EndPoint t' a) -> a
      sel = either (^.unEndPoint) (^.unEndPoint)

      -- construct = case (el,eu) of
      --              (Left l, Left u) -> coRec $ GRange l u


type family OnlyA (bool :: Bool) a b where
   OnlyA False a b = [a,b]
   OnlyA True  a b = '[a]


-- type family ClipLowerResult b l' l u a :: [*] where
--   ClipLowerResult True  l  l u a = '[ Range l u a ]
--   ClipLowerResult False l' l u a =  [ Range l u a, Range l' u a ]

-- type family ClipUpperResult b l u u' a :: [*] where
--   ClipUpperResult True  l u u' a = '[ Range l u a ]
--   ClipUpperResult False l u u' a =  [ Range l u a, Range l u' a ]



type family t ==~ t' where
  t ==~ t  = True
  t ==~ t' = False



class ClipLower (b :: Bool) l' l u a where
  -- clipLower'' :: Ord a => proxy b
  --           -> EndPoint l' a -> Range l u a -> CoRec Identity (ClipLowerResult b l' l u a)
  clipLower' :: Ord a => proxy b
             -> EndPoint l' a -> Range l u a -> CoRec Identity (OnlyA b (Range l  u a)
                                                                        (Range l' u a))
instance ClipLower True l l u a where
  clipLower' _ p = either coRec coRec . clipLower'' p

instance (IsAlwaysTrueFromEither (Range l u a) (Range l' u a)) =>
         ClipLower False l' l u a where
  clipLower' _ p r = fromEither $ clipLower'' p r

clipLower''     :: Ord a
                => EndPoint l' a -> Range l u a -> Either (Range l u a) (Range l' u a)
clipLower'' p r = case argEndPoint max (r^.lower) p of
                  Left  l -> Left r
                  Right l -> Right $ r&lower .~ l


class ClipUpper (b :: Bool) l u u' a where
  clipUpper' :: Ord a => proxy b
             -> EndPoint u' a -> Range l u a -> CoRec Identity (OnlyA b (Range l u  a)
                                                                        (Range l u' a))

clipLower :: forall b l' l u a. (Ord a, ClipLower b l' l u a, b ~ (l' ==~ l)) =>
          EndPoint l' a -> Range l u a -> CoRec Identity (OnlyA b (Range l  u a)
                                                                  (Range l' u a))
clipLower = clipLower' (Proxy :: Proxy b)


-- | FIXME: dunno why this instance does not want to typecheck
instance ClipUpper True l u u' a where
  clipUpper' _ p = either coRec coRec . clipUpper'' p


instance (IsAlwaysTrueFromEither (Range l u a) (Range l u' a)) =>
         ClipUpper False l u u' a where
  clipUpper' _ p r = fromEither $ clipUpper'' p r


clipUpper''     :: Ord a
                => EndPoint u' a -> Range l u a -> Either (Range l u a) (Range l u' a)
clipUpper'' p r = case argEndPoint min (r^.upper) p of
                  Left  l -> Left r
                  Right l -> Right $ r&upper .~ l


clipUpper :: forall b l u u' a. (Ord a, ClipUpper b l u u' a, b ~ (u' ==~ u)) =>
          EndPoint u' a -> Range l u a -> CoRec Identity (OnlyA b (Range l u  a)
                                                                  (Range l u' a))
clipUpper = clipUpper' (Proxy :: Proxy b)




-- constructI                   :: Either (EndPoint l a) (EndPoint l' a)
--                              -> Either (EndPoint u a) (EndPoint u' a)
--                              -> CoRec Identity (IntersectionEndPoints l u l' u' a)
-- constructI (Left l) (Left u) = coRec $ GRange l u


-- constructRec :: proxy l' -> EndPoint l a -> Either (EndPoint u a) (EndPoint u' a)
--                 -> CoRec Identity (IntersectionEndPoints l u l' u' a)
-- constructRec _ l (Left u) = coRec $ GRange l u


class TestEndPointType t where
  isOpenEndPoint :: EndPoint t a -> Bool

instance TestEndPointType Open   where isOpenEndPoint _ = True
instance TestEndPointType Closed where isOpenEndPoint _ = False


type family RestrictOpen t t' where
  RestrictOpen Closed Closed = Closed
  RestrictOpen t      t'     = Open



argEndPoint                                   :: Eq a => (a -> a -> a)
                                              -> EndPoint t a -> EndPoint t' a
                                              -> Either (EndPoint t a) (EndPoint t' a)
argEndPoint f aa@(EndPoint a) bb@(EndPoint b)
                                  | p a b     = Left aa
                                  | otherwise = Right bb
  where
    p a' b' = a' == f a' b'



-- type family SubLineEndPoints l u l' u' p q r :: [*] where
--   SubLineEndPoints l  u l  u  p q r = '[ SubLine 2 l  u  (Either p q) r ]
--   SubLineEndPoints l  u l  u' p q r =  [ SubLine 2 l  u  (Either p q) r
--                                        , SubLine 2 l  u' (Either p q) r ]
--   SubLineEndPoints l  u l' u  p q r =  [ SubLine 2 l  u  (Either p q) r
--                                        , SubLine 2 l' u  (Either p q) r ]
--   SubLineEndPoints l  u l' u' p q r =  [ SubLine 2 l  u  (Either p q) r
--                                        , SubLine 2 l  u' (Either p q) r
--                                        , SubLine 2 l' u  (Either p q) r
--                                        , SubLine 2 l' u' (Either p q) r ]


-- type instance IntersectionOf (SubLine 2 l u p r) (SubLine 2 l' u' q r) =
--   NoIntersection ': Point 2 r ': SubLineEndPoints l u l' u' p q r


-- intersect1 :: SubLine 2 l u p r -> SubLine 2 l' u' q r -> Intersection1 (SubLine 2 l u p r) (SubLine 2 l' u' q r)
-- (SubLine l (Range s t)) `intersect1` (SubLine m (Range a b)) = undefined


-- newtype GLineSegment s t d p r = GLineSegment (GInterval s t p (Point d r))


-- data CurveSegment c s t r = SubLine { _support :: c , _range :: Range s t r }



--------------------------------------------------------------------------------

type Interval a r = ClosedInterval a r

class HasStart t where
  type StartCore t
  type StartExtra t
  start :: Lens' t (StartCore t :+ StartExtra t)

instance HasStart (Interval a r) where
  type StartCore (Interval a r) = r
  type StartExtra (Interval a r) = a
  start = lower.unEndPoint

class HasEnd t where
  type EndCore t
  type EndExtra t
  end :: Lens' t (EndCore t :+ EndExtra t)

instance HasEnd (Interval a r) where
  type EndCore (Interval a r) = r
  type EndExtra (Interval a r) = a
  end = upper.unEndPoint

type instance Dimension (Interval a r) = 1
type instance NumType   (Interval a r) = r


-- instance Ord r => IsIntersectableWith (Interval a r) (Interval a r) where

--   data Intersection (Interval a r) (Interval a r) = IntervalIntersection (Interval a r)
--                                                   | NoOverlap
--                                                   deriving (Show,Read,Eq,Ord)

--   nonEmptyIntersection NoOverlap = False
--   nonEmptyIntersection _         = True

--   (Interval a b) `intersect` (Interval c d)
--       | s^.core <= t^.core = IntervalIntersection $ Interval s t
--       | otherwise          = NoOverlap
--     where

--       s = a `maxOnCore` c
--       t = b `minOnCore` d


-- data IntervalUnion a  r = DisjointIntervals (Interval a r) (Interval a r)
--                         | OneInterval       (Interval a r)
--                         deriving (Show,Read,Eq,Ord)

-- type instance Union (Interval a r) (Interval a r) = IntervalUnion a r

-- instance Ord r => IsUnionableWith (Interval a r) (Interval a r) where

--   i@(Interval a b) `union` j@(Interval c d)
--       | i `intersects` j = OneInterval $ Interval s t
--       | otherwise        = DisjointIntervals i j
--     where
--       s = a `minOnCore` c
--       t = b `maxOnCore` d


-- maxOnCore :: Ord c => c :+ e -> c :+ e -> c :+ e
-- l@(lc :+ _) `maxOnCore` r@(rc :+ _) = if lc >= rc then l else r

-- minOnCore :: Ord c => c :+ e -> c :+ e -> c :+ e
-- l@(lc :+ _) `minOnCore` r@(rc :+ _) = if lc <= rc then l else r


-- | Get the width of the interval
width   :: Num r => Range l u r -> r
width i = i^.upper.unEndPoint - i^.lower.unEndPoint

testOpen :: SymRange Open Int
testOpen = openRange 0 10

closedR :: SymRange Closed Int
closedR = closedRange 5 11



type AlwaysTruePrettyShow l u = (TestEndPointType l, TestEndPointType u)

prettyShow :: (AlwaysTruePrettyShow l u, Show a) => Range l u a -> String
prettyShow (GRange l u) = concat [ lowerB, show (l^.unEndPoint), ", "
                                 , show (u^.unEndPoint), upperB
                                 ]
  where
    lowerB = if isOpenEndPoint l then "(" else "["
    upperB = if isOpenEndPoint u then ")" else "]"

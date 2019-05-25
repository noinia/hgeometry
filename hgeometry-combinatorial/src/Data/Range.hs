{-# LANGUAGE TemplateHaskell   #-}
{-# LANGUAGE DeriveAnyClass  #-}
--------------------------------------------------------------------------------
-- |
-- Module      :  Data.Range
-- Copyright   :  (C) Frank Staals
-- License     :  see the LICENSE file
-- Maintainer  :  Frank Staals
--
-- Data type for representing Generic Ranges (Intervals) and functions that
-- work with them.
--
--------------------------------------------------------------------------------
module Data.Range( EndPoint(..)
                 , isOpen, isClosed
                 , unEndPoint
                 , Range(..)
                 , prettyShow
                 , lower, upper
                 , pattern OpenRange, pattern ClosedRange, pattern Range'
                 , inRange, width, clipLower, clipUpper, midPoint, clampTo
                 , isValid, covers

                 , shiftLeft, shiftRight
                 ) where

import Control.DeepSeq
import Control.Lens
import Data.Intersection
import Data.Vinyl.CoRec
import GHC.Generics (Generic)
import Test.QuickCheck
import Text.Printf (printf)

--------------------------------------------------------------------------------
-- * Representing Endpoints of a Range

-- | Endpoints of a range may either be open or closed.
data EndPoint a = Open   !a
                | Closed !a
                deriving (Show,Read,Eq,Functor,Foldable,Traversable,Generic,NFData)

instance Ord a => Ord (EndPoint a) where
  -- | order on the actual value, and Open before Closed
  a `compare` b = f a `compare` f b
    where
      f (Open x)   = (x,False)
      f (Closed x) = (x,True)

instance Arbitrary r => Arbitrary (EndPoint r) where
  arbitrary = frequency [ (1, Open   <$> arbitrary)
                        , (9, Closed <$> arbitrary)
                        ]

_unEndPoint            :: EndPoint a -> a
_unEndPoint (Open a)   = a
_unEndPoint (Closed a) = a

unEndPoint :: Lens (EndPoint a) (EndPoint b) a b
unEndPoint = lens _unEndPoint f
  where
    f (Open _) a   = Open a
    f (Closed _) a = Closed a
{-# INLINE unEndPoint #-}

isOpen          :: EndPoint a -> Bool
isOpen (Open _) = True
isOpen _        = False

isClosed :: EndPoint a -> Bool
isClosed = not . isOpen


--------------------------------------------------------------------------------
-- * The Range Data type

-- | Data type for representing ranges.
data Range a = Range { _lower :: !(EndPoint a)
                     , _upper :: !(EndPoint a)
                     }
               deriving (Eq,Functor,Foldable,Traversable,Generic,NFData)
makeLenses ''Range

instance Show a => Show (Range a) where
  show (Range l u) = printf "Range (%s) (%s)" (show l) (show u)


pattern OpenRange       :: a -> a -> Range a
pattern OpenRange   l u = Range (Open l)   (Open u)

pattern ClosedRange     :: a -> a -> Range a
pattern ClosedRange l u = Range (Closed l) (Closed u)

-- | A range from l to u, ignoring/forgetting the type of the endpoints
pattern Range'     :: a -> a -> Range a
pattern Range' l u <- ((\r -> (r^.lower.unEndPoint,r^.upper.unEndPoint) -> (l,u)))
{-# COMPLETE Range' #-}

instance (Arbitrary r, Ord r) => Arbitrary (Range r) where
  arbitrary = do
                l <- arbitrary
                r <- suchThat arbitrary (p l)
                return $ Range l r
   where
     p (Open l)   r = l <  r^.unEndPoint
     p (Closed l) r = l <= r^.unEndPoint


-- | Helper function to show a range in mathematical notation.
--
-- >>> prettyShow $ OpenRange 0 2
-- "(0,2)"
-- >>> prettyShow $ ClosedRange 0 2
-- "[0,2]"
-- >>> prettyShow $ Range (Open 0) (Closed 5)
-- "(0,5]"
prettyShow             :: Show a => Range a -> String
prettyShow (Range l u) = concat [ lowerB, show (l^.unEndPoint), ","
                                , show (u^.unEndPoint), upperB
                                ]
  where
    lowerB = if isOpen l then "(" else "["
    upperB = if isOpen u then ")" else "]"



-- | Test if a value lies in a range.
--
-- >>> 1 `inRange` (OpenRange 0 2)
-- True
-- >>> 1 `inRange` (OpenRange 0 1)
-- False
-- >>> 1 `inRange` (ClosedRange 0 1)
-- True
-- >>> 1 `inRange` (ClosedRange 1 1)
-- True
-- >>> 10 `inRange` (OpenRange 1 10)
-- False
-- >>> 10 `inRange` (ClosedRange 0 1)
-- False
--
-- This one is kind of weird
--
-- >>> 0 `inRange` Range (Closed 0) (Open 0)
-- False
inRange                 :: Ord a => a -> Range a -> Bool
x `inRange` (Range l u) = case ((l^.unEndPoint) `compare` x, x `compare` (u^.unEndPoint)) of
    (_, GT) -> False
    (GT, _) -> False
    (LT,LT) -> True
    (LT,EQ) -> include u -- depends on only u
    (EQ,LT) -> include l -- depends on only l
    (EQ,EQ) -> include l && include u -- depends on l and u
  where
    include = isClosed

type instance IntersectionOf (Range a) (Range a) = [ NoIntersection, Range a]

instance Ord a => (Range a) `IsIntersectableWith` (Range a) where

  nonEmptyIntersection = defaultNonEmptyIntersection

  -- The intersection is empty, if after clipping, the order of the end points is inverted
  -- or if the endpoints are the same, but both are open.
  (Range l u) `intersect` s = let i = clipLower' l . clipUpper' u $ s
                              in if isValid i then coRec i else coRec NoIntersection

-- | Get the width of the interval
--
-- >>> width $ ClosedRange 1 10
-- 9
-- >>> width $ OpenRange 5 10
-- 5
width   :: Num r => Range r -> r
width i = i^.upper.unEndPoint - i^.lower.unEndPoint

midPoint   :: Fractional r => Range r -> r
midPoint r = let w = width r in r^.lower.unEndPoint + (w / 2)

-- | Clamps a value to a range. I.e. if the value lies outside the range we
-- report the closest value "in the range". Note that if an endpoint of the
-- range is open we report that value anyway, so we return a value that is
-- truely inside the range only if that side of the range is closed.
--
-- >>> clampTo (ClosedRange 0 10) 20
-- 10
-- >>> clampTo (ClosedRange 0 10) (-20)
-- 0
-- >>> clampTo (ClosedRange 0 10) 5
-- 5
-- >>> clampTo (OpenRange 0 10) 20
-- 10
-- >>> clampTo (OpenRange 0 10) (-20)
-- 0
-- >>> clampTo (OpenRange 0 10) 5
-- 5
clampTo                :: Ord r => Range r -> r -> r
clampTo (Range' l u) x = (x `max` l) `min` u


--------------------------------------------------------------------------------
-- * Helper functions

-- | Clip the interval from below. I.e. intersect with the interval {l,infty),
-- where { is either open, (, orr closed, [.
clipLower     :: Ord a => EndPoint a -> Range a -> Maybe (Range a)
clipLower l r = let r' = clipLower' l r in if isValid r' then Just r' else Nothing

-- | Clip the interval from above. I.e. intersect with (-\infty, u}, where } is
-- either open, ), or closed, ],
clipUpper     :: Ord a => EndPoint a -> Range a -> Maybe (Range a)
clipUpper u r = let r' = clipUpper' u r in if isValid r' then Just r' else Nothing

-- | Wether or not the first range completely covers the second one
covers       :: forall a. Ord a => Range a -> Range a -> Bool
x `covers` y = maybe False (== y) . asA @(Range a) $ x `intersect` y


-- | Check if the range is valid and nonEmpty, i.e. if the lower endpoint is
-- indeed smaller than the right endpoint. Note that we treat empty open-ranges
-- as invalid as well.
isValid             :: Ord a => Range a -> Bool
isValid (Range l u) = case (_unEndPoint l) `compare` (_unEndPoint u) of
                          LT                            -> True
                          EQ | isClosed l || isClosed u -> True
                          _                             -> False

-- operation is unsafe, as it may produce an invalid range (where l > u)
clipLower'                  :: Ord a => EndPoint a -> Range a -> Range a
clipLower' l' r@(Range l u) = case l' `cmpLower` l of
                                GT -> Range l' u
                                _  -> r
-- operation is unsafe, as it may produce an invalid range (where l > u)
clipUpper'                  :: Ord a => EndPoint a -> Range a -> Range a
clipUpper' u' r@(Range l u) = case u' `cmpUpper` u of
                                LT -> Range l u'
                                _  -> r

-- | Compare end points, Closed < Open
cmpLower     :: Ord a => EndPoint a -> EndPoint a -> Ordering
cmpLower a b = case (_unEndPoint a) `compare` (_unEndPoint b) of
                 LT -> LT
                 GT -> GT
                 EQ -> case (a,b) of
                         (Open _,   Open _)   -> EQ  -- if both are same type, report EQ
                         (Closed _, Closed _) -> EQ
                         (Open _,  _)         -> GT  -- otherwise, choose the Closed one
                         (Closed _,_)         -> LT  -- is the *smallest*


-- | Compare the end points, Open < Closed
cmpUpper     :: Ord a => EndPoint a -> EndPoint a -> Ordering
cmpUpper a b = case (_unEndPoint a) `compare` (_unEndPoint b) of
                 LT -> LT
                 GT -> GT
                 EQ -> case (a,b) of
                         (Open _,   Open _)   -> EQ  -- if both are same type, report EQ
                         (Closed _, Closed _) -> EQ
                         (Open _,  _)         -> LT  -- otherwise, choose the Closed one
                         (Closed _,_)         -> GT  -- is the *largest*




--------------------------------------------------------------------------------

-- | Shift a range x units to the left
--
-- >>> prettyShow $ shiftLeft 10 (ClosedRange 10 20)
-- "[0,10]"
-- >>> prettyShow $ shiftLeft 10 (OpenRange 15 25)
-- "(5,15)"
shiftLeft   :: Num r => r -> Range r -> Range r
shiftLeft x = shiftRight (-x)

-- | Shifts the range to the right
--
-- >>> prettyShow $ shiftRight 10 (ClosedRange 10 20)
-- "[20,30]"
-- >>> prettyShow $ shiftRight 10 (OpenRange 15 25)
-- "(25,35)"
shiftRight   :: Num r => r -> Range r -> Range r
shiftRight x = fmap (+x)

{-# LANGUAGE TemplateHaskell   #-}
module Data.Range( EndPoint(..)
                 , isOpen, isClosed
                 , unEndPoint
                 , Range(..)
                 , lower, upper
                 , pattern OpenRange, pattern ClosedRange, pattern Range'
                 , inRange, width, clipLower, clipUpper
                 , isValid


                 , Top(..), toMaybe
                 , Bottom
                 , UnBounded(..)
                 ) where

import           Control.Applicative
import           Control.Arrow((&&&))
import           Control.Lens
import qualified Data.Foldable as F
import           Data.Geometry.Properties
import qualified Data.Traversable as T

--------------------------------------------------------------------------------


data EndPoint a = Open   a
                | Closed a
                deriving (Show,Read,Eq,Functor,F.Foldable,T.Traversable)

_unEndPoint            :: EndPoint a -> a
_unEndPoint (Open a)   = a
_unEndPoint (Closed a) = a

unEndPoint :: Lens (EndPoint a) (EndPoint b) a b
unEndPoint = lens _unEndPoint f
  where
    f (Open _) a   = Open a
    f (Closed _) a = Closed a


isOpen          :: EndPoint a -> Bool
isOpen (Open _) = True
isOpen _        = False

isClosed :: EndPoint a -> Bool
isClosed = not . isOpen


--------------------------------------------------------------------------------

data Range a = Range { _lower :: EndPoint a
                     , _upper :: EndPoint a
                     }
               deriving (Show,Read,Eq,Functor,F.Foldable,T.Traversable)

makeLenses ''Range

pattern OpenRange   l u = Range (Open l)   (Open u)
pattern ClosedRange l u = Range (Closed l) (Closed u)

-- | A range from l to u, ignoring/forgetting the type of the enpoints
pattern Range' l u <- (_lower &&& _upper -> (l,u))



prettyShow             :: Show a => Range a -> String
prettyShow (Range l u) = concat [ lowerB, show (l^.unEndPoint), ", "
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
inRange                 :: Ord a => a -> Range a -> Bool
x `inRange` (Range l u) = case ((l^.unEndPoint) `compare` x, x `compare` (u^.unEndPoint)) of
    (_, GT) -> False
    (GT, _) -> False
    (LT,LT) -> True
    (LT,EQ) -> include u -- depends on only u
    (EQ,LT) -> include l -- depends on only l
    (EQ,EQ) -> include l || include u -- depends on l and u
  where
    include = isClosed

type instance IntersectionOf (Range a) (Range a) = [ NoIntersection, Range a]

instance Ord a => (Range a) `IsIntersectableWith` (Range a) where

  nonEmptyIntersection = defaultNonEmptyIntersection

  -- The intersection is empty, if after clipping, the order of the end points is inverted
  -- or if the endpoints are the same, but both are open.
  r@(Range l u) `intersect` s = let i@(Range l' u') = clipLower' l . clipUpper' u $ s
                                in if isValid i then coRec i else coRec NoIntersection

-- | Get the width of the interval
--
-- >>> width $ ClosedRange 1 10
-- 9
-- >>> width $ OpenRange 5 10
-- 5
width   :: Num r => Range r -> r
width i = i^.upper.unEndPoint - i^.lower.unEndPoint



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


-- | Check if the range is valid and nonEmpty, i.e. if the lower endpoint is
-- indeed smaller than the right endpoint. Note that we treat empty open-ranges
-- as invalid as well.
isValid               :: Ord a => Range a -> Bool
isValid r@(Range l u) = case (_unEndPoint l) `compare` (_unEndPoint u) of
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
-- "[0, 10]"
-- >>> prettyShow $ shiftLeft 10 (OpenRange 15 25)
-- "(5, 15)"
shiftLeft   :: Num r => r -> Range r -> Range r
shiftLeft x = shiftRight (-x)

-- | Shifts the range to the right
--
-- >>> prettyShow $ shiftRight 10 (ClosedRange 10 20)
-- "[20, 30]"
-- >>> prettyShow $ shiftRight 10 (OpenRange 15 25)
-- "(25, 35)"
shiftRight   :: Num r => r -> Range r -> Range r
shiftRight x = fmap (+x)

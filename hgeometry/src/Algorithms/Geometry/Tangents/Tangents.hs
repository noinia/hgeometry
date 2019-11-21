{-# LANGUAGE TemplateHaskell  #-}
module Algorithms.Geometry.Tangents.Tangents where

import           Control.Lens ((^.),(%~), (^?), (.~), (&), to
                              , makeLenses, Index, Ixed(..), IxValue)
import qualified Data.CircularSeq as CSeq
import           Data.Ext
import           Data.Foldable (toList)
import           Data.Geometry.Line
import           Data.Geometry.LineSegment
import           Data.Geometry.Point
import           Data.Geometry.Polygon
import           Data.Geometry.Polygon.Convex hiding (upperTangent)
import           Data.Maybe (fromJust)
import           Data.Sequence (Seq, ViewL(..),ViewR(..), viewl, viewr)
import qualified Data.Sequence as Seq

--------------------------------------------------------------------------------



data Status f p r = Status { _polygon :: f (Point 2 r :+ p)
                           , _size    :: !Int
                           , _st      :: !Int
                           , _tent    :: !Int
                           , _end     :: !Int
                           , _wrap    :: !Bool
                           , _isCCW   :: !Bool
                           }
makeLenses ''Status

ccwMod   :: Status f p r -> Int
ccwMod p = if p^.isCCW then -1 else 1

-- | Test if we are done with this polygon
isDone   :: Status f p r -> Bool
isDone p = p^.end - p^.tent == ccwMod p

isRefined   :: Status f p r -> Bool
isRefined p = p^.st /= p^.tent


type Ixed'' f a = (Ixed (f a), Index (f a) ~ Int, IxValue (f a) ~ a)
type Ixed' f p r = Ixed'' f (Point 2 r :+ p)

(!)   :: Ixed' f p r => Status f p r -> Int -> Point 2 r :+ p
p ! i = fromJust $ p^?polygon.ix (i `mod` p^.size)

infix 1 !

-- tangent extending from i
tang     :: Ixed' f p r => Status f p r -> Int -> (Point 2 r :+ p, Point 2 r :+ p)
tang p i = (p ! i, p ! i + ccwMod p)

--------------------------------------------------------------------------------


-- | Compute the upper tangent of two disjoint convex polgyons
--
--   pre: - p and q are disjoint
--        - The vertices of the polygons are given in counter clockwise order
--
--
-- TODO: Describe what the output is in case of degeneracy
--
-- Running time: \(O(\log (n+m))\), where $n$ and $m$ are the sizes of the two
-- polygons respectively
upperTangent       :: (Num r, Ord r)
                   => ConvexPolygon p r -> ConvexPolygon p r -> LineSegment 2 p r
upperTangent pP pQ = upperTangent' $ initialize (p,q) (p0,q0)
  where
     -- guaranteed to have one elem:
    p@(viewl -> p0 :< _) = pP^.simplePolygon.outerBoundary.to CSeq.asSeq
    q@(viewl -> q0 :< _) = pQ^.simplePolygon.outerBoundary.to CSeq.asSeq

-- | Same as upperTangent, but computes the lower tangent
lowerTangent :: (Num r, Ord r)
             => ConvexPolygon p r -> ConvexPolygon p r -> LineSegment 2 p r
lowerTangent = flip upperTangent


upperTangent'         :: (Ixed' f p r, Num r, Ord r)
                      => (Status f p r, Status f p r) -> LineSegment 2 p r
upperTangent' (pP,pQ)
  | not $ isDone pP   = upperTangent' . refine $ (pP,pQ)
  | not $ isDone pQ   = upperTangent' . refine $ (pQ,pP)
  | otherwise         = ClosedLineSegment (pP ! pP^.end) (pQ ! pQ^.end)

initialize                 :: (Ixed' f p r, Num r, Ord r, Foldable f)
                           => (f (Point 2 r :+ p), f (Point 2 r :+ p))
                           -> (Point 2 r :+ p, Point 2 r :+ p)
                           -> (Status f p r, Status f p r)
initialize (pP,pQ) (p0,q0) = (initialize' pP q0, (initialize' pQ p0)&isCCW .~ False)

initialize'       :: (Ixed' f p r, Num r, Ord r, Foldable f) => f (Point 2 r :+ p)
                  -> Point 2 r :+ p -> Status f p r
initialize' pP q0 = sP
  where
    sP =   Status pP n 0 t e wrapP False
    n     = length pP
    wrapP = not $ q0 `isLeftOf` tang sP 0
    t     = if wrapP then n   else 0
    e     = if wrapP then 2*n else n


data Action = DiscStart | DiscEnd | NoDisc deriving (Show,Eq)


refine         :: (Ixed' f p r, Num r, Ord r)
               => (Status f p r, Status f p r) -> (Status f p r, Status f p r)
refine (pP,pQ) = refineP $ tryCertifyQ
  where
    mid = pP^.tent + (pP^.end - pP^.tent) `div` 2
    pm  = pP ! mid



    tryCertifyQ
      -- Check if we have to certify
      | isRefined pQ && pm `isLeftOf` tang pQ (pQ^.tent) = certifiedQ
      | otherwise                                        = (pP,pQ)

    certifiedQ
        -- certify Q.tent
      | (pP ! 0) `isLeftOf` (pQ ! pQ^.tent, pm)
        `xor` (pQ^.isCCW)                    = (pP,pQ&st .~ pQ^.tent)
        -- revoke the tentative refinement to Q, and instead refine P
      | otherwise                            = ( pP&st .~ pP^.tent
                                               , (pQ&end .~ pQ^.tent)&tent .~ pQ^.st
                                               )


  -- case action of
  --   NoDisc               -> p&tent .~ mid          -- tentatively refeine p at mid
  --   DiscStart | p^.isCCW -> (p&st .~ mid)&st.~ mid -- discard the initial half
  --   _                    -> p&end .~ mid           -- discard the second half
  -- where

  --   -- action = case (p^.wrap && )


refine' (pP,pQ)
    taum `intersectsAfter` p0q0 || (pP^.wrap && mid > pP^.size)
                = DiscEnd --pP&end .~ mid  -- nothing ccw of mid in Ip
    taum `intersectsBefore` p0q0 || (pP^.wrap && mid < pP^.size && pm `isRightOf` p0q0)
                = DiscStart

    taum `intersectsAfter` q0qEnd || taum `intersectsAfter` q0qTent
                = DiscardEnd
    taum `intersectsBefore` q0qEnd || taum `intersectsBefore` q0qTent
                = DiscardStart
     otherwise = NoDiscarc

  where
    mid  = pP^.tent + (pP^.end - pP^.tent) `div` 2
    pm   = pP ! mid
    taum = tang pP mid

    intersectsBefore = undefined
    intersecstAfter = undefined



    -- refine' p q =





xor                :: Bool -> Bool -> Bool
True   `xor`  True = False
False  `xor` False = False
_      `xor` _     = True




isRightOf           :: (Num r, Ord r)
                    => Point 2 r :+ p -> (Point 2 r :+ p', Point 2 r :+ p'') -> Bool
a `isRightOf` (b,c) = ccw' b c a == CW

isLeftOf            :: (Num r, Ord r)
                    => Point 2 r :+ p -> (Point 2 r :+ p', Point 2 r :+ p'') -> Bool
a `isLeftOf` (b,c) = ccw' b c a == CCW

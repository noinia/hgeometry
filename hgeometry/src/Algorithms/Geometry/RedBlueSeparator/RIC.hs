--------------------------------------------------------------------------------
-- |
-- Module      :  Algorithms.Geometry.RedBlueSeparator.RIC
-- Copyright   :  (C) Frank Staals
-- License     :  see the LICENSE file
-- Maintainer  :  Frank Staals
--
-- Given a set of red points and a set of blue points in \(\mathbb{R}^2\) finds
-- a separating line in \(O(n)\) expected time, where \(n\) is the total number
-- of points.
--
--------------------------------------------------------------------------------
module Algorithms.Geometry.RedBlueSeparator.RIC where


import           Algorithms.Geometry.LinearProgramming.LP2DRIC
import           Algorithms.Geometry.LinearProgramming.Types
import           Control.Applicative ((<|>))
import           Control.Lens hiding (below)
import           Control.Monad.Random.Class
import           Data.Ext
import qualified Data.Foldable as F
import           Data.Geometry.HalfSpace
import           Data.Geometry.Line
import           Data.Geometry.Point
import           Data.Geometry.Vector
import           Data.Ord (comparing)
import           Data.Semigroup.Foldable
import           Data.Util

--------------------------------------------------------------------------------

-- -- | Given a set of red points and a set of blue points in \(\mathbb{R}^2\)
-- -- finds a separating line (if it exists). The result is strict in the
-- -- sense that there will not be any points on the line.
-- --
-- --
-- -- running time: \(O(n)\) expected time, where \(n\) is the total number
-- -- of points.
-- strictSeparatingLine = undefined

-- | Given a set of red points and a set of blue points in \(\mathbb{R}^2\)
-- finds a separating line (if it exists). The result is non-strict in the
-- sense that there may be points *on* the line.
--
--
-- running time: \(O(n)\) expected time, where \(n\) is the total number
-- of points.
separatingLine :: (MonadRandom m, Foldable1 f, Foldable1 g, Fractional r, Ord r)
               => f (Point 2 r :+ redData)
               -> g (Point 2 r :+ blueData)
               -> m (Maybe (Line 2 r))
separatingLine reds blues = do l <- separatingLine' reds blues
                               m <- separatingLine' blues reds
                               pure $ l <|> m

-- | Given a set of red points and a set of blue points in \(\mathbb{R}^2\)
-- finds a separating line (if it exists) that has all red points *right* (or
-- on) the line, and all blue points left (or on) the line.
--
-- running time: \(O(n)\) expected time, where \(n\) is the total number
-- of points.
separatingLine' :: (MonadRandom m, Foldable1 f, Foldable1 g, Fractional r, Ord r)
                => f (Point 2 r :+ redData)
                -> g (Point 2 r :+ blueData)
                -> m (Maybe (Line 2 r))
separatingLine' reds blues = case verticalSeparatingLine reds blues of
    SP Nothing ((r:+_),(b :+ _)) -> separatingLine'' r b reds blues
      -- observe that if r and b were vertically above each other then we would
      -- have found a separating line. So r and b are not vertically
      -- aligned. Hence we satisfy the precondition.
    SP ml@(Just _) _             -> pure ml  -- already found a line


-- | given a red and blue point that are *NOT* vertically alligned, and all red
-- and all blue points, try to find a non-vertical separating line.
--
-- running time: \(O(n)\) expected time, where \(n\) is the total number
-- of points.
separatingLine'' :: (MonadRandom m, Foldable1 f, Foldable1 g, Fractional r, Ord r)
                => Point 2 r -- ^ red point r
                -> Point 2 r -- ^ a blue point b
                -> f (Point 2 r :+ redData)
                -> g (Point 2 r :+ blueData)
                -> m (Maybe (Line 2 r))
separatingLine'' r b reds blues = fmap mkLine <$> solveBoundedLinearProgram lp
  where
    lp = LinearProgram c ([mkRed r, mkBlue b] <> hs)

    c = case (r^.xCoord) `compare` (b^.xCoord) of
          LT -> Vector2 (-1) 0  -- minimize a
          GT -> Vector2 1    0  -- maximize a
          EQ -> error "separatingLine'': precondition failed. r and b vertically above each other"

    mkLine (Point2 aa bb) = fromLinearFunction aa bb

    -- red points generate the constraint: ry <= a*rx + b <=> b >= (-rx)a + ry
    mkRed (Point2 rx ry)  = above $ fromLinearFunction ((-1)*rx) ry
    -- blue points generate the constraint: by >= a*bx + b <=> b <= (-bx)a + by
    mkBlue (Point2 bx by) = below $ fromLinearFunction ((-1)*bx) by

    hs = [mkRed rr | (rr :+ _) <- F.toList reds] <> [mkBlue bb | (bb :+ _) <- F.toList blues]


--------------------------------------------------------------------------------
-- * Vertical Separators

-- | Computes a strict vertical separating line, if one exists
strictVerticalSeparatingLine            :: (Foldable1 f, Foldable1 g, Fractional r, Ord r)
                                        => f (Point 2 r :+ redData)
                                        -> g (Point 2 r :+ blueData)
                                        -> Maybe (Line 2 r)
strictVerticalSeparatingLine reds blues = do let (r,b) = extremalPoints reds blues
                                                 rx    = r^.core.xCoord
                                                 bx    = b^.core.xCoord
                                             if bx < rx
                                               then Just . verticalLine $ (rx + bx) / 2
                                               else Nothing -- no vertical separator


-- | Test if there is a vertical separating line that has all red points to its
-- right (or on it) and all blue points to its left (or on it).  This function
-- also returns the two extremal points; in case a line is returned, the line
-- actually goes through the blue (second) point, if there is no line, this
-- pair provides evidence that there is no vertical separating line.
--
-- The line we return actually goes through one blue point.
verticalSeparatingLine            :: (Foldable1 f, Foldable1 g, Num r, Ord r)
                                  => f (Point 2 r :+ redData)
                                  -> g (Point 2 r :+ blueData)
                                  -> SP (Maybe (Line 2 r))
                                        (Point 2 r :+ redData, Point 2 r :+ blueData)
verticalSeparatingLine reds blues = SP ml es
  where
    es@(r,b) = extremalPoints reds blues
    ml = if b^.core.xCoord <= r^.core.xCoord then Just . verticalLine $ (b^.core.xCoord)
                                             else Nothing

-- | Get the the leftmost red point and the rightmost blue point.
extremalPoints            :: (Foldable1 f, Foldable1 g, Ord r)
                          => f (Point 2 r :+ redData)
                          -> g (Point 2 r :+ blueData)
                          -> (Point 2 r :+ redData, Point 2 r :+ blueData)
extremalPoints reds blues = (F.minimumBy (comparing (^.core.xCoord)) reds
                            ,F.maximumBy (comparing (^.core.xCoord)) blues)

--------------------------------------------------------------------------------

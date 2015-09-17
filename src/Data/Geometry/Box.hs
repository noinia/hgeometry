{-# LANGUAGE TemplateHaskell  #-}
{-# LANGUAGE MultiParamTypeClasses  #-}
{-# LANGUAGE ScopedTypeVariables  #-}
{-# LANGUAGE DeriveFunctor  #-}
module Data.Geometry.Box( module Data.Geometry.Box.Internal
                        , topSide, leftSide, bottomSide, rightSide
                        , sides, sides'
                        ) where


import           Control.Applicative
import qualified Data.Traversable as Tr
import           Data.Geometry.Box.Internal
import           Data.Geometry.LineSegment

--------------------------------------------------------------------------------

topSide :: Num r => Rectangle p r -> Maybe (LineSegment 2 p r)
topSide = fmap (\(l,r,_,_) -> ClosedLineSegment l r) . corners

-- | Oriented from *left to right*
bottomSide :: Num r => Rectangle p r -> Maybe (LineSegment 2 p r)
bottomSide = fmap (\(_,_,r,l) -> ClosedLineSegment l r) . corners

--
leftSide  :: Num r => Rectangle p r -> Maybe (LineSegment 2 p r)
leftSide = fmap (\(t,_,_,b) -> ClosedLineSegment b t) . corners

-- | The right side, oriented from *bottom* to top
rightSide :: Num r => Rectangle p r -> Maybe (LineSegment 2 p r)
rightSide = fmap (\(_,t,b,_) -> ClosedLineSegment b t) . corners


-- | The sides of the rectangle, in order (Top, Right, Bottom, Left). The sides
-- themselves are also oriented in clockwise order. If, you want them in the
-- same order as the functions `topSide`, `bottomSide`, `leftSide`, and
-- `rightSide`, use `sides'` instead.
sides :: Num r => Rectangle p r -> Maybe ( LineSegment 2 p r
                                         , LineSegment 2 p r
                                         , LineSegment 2 p r
                                         , LineSegment 2 p r
                                         )
sides = fmap (\(t,r,b,l) -> (t,flipSegment r,flipSegment b,l)) . sides'


-- | The sides of the rectangle. The order of the segments is (Top, Right,
-- Bottom, Left).  Note that the segments themselves, are oriented as described
-- by the functions topSide, bottomSide, leftSide, rightSide (basically: from
-- left to right, and from bottom to top). If you want the segments oriented
-- along the boundary of the rectangle, use the `sides` function instead.
sides'   :: Num r => Rectangle p r -> Maybe ( LineSegment 2 p r
                                            , LineSegment 2 p r
                                            , LineSegment 2 p r
                                            , LineSegment 2 p r
                                            )
sides' r = (\[t,r',b,l] -> (t,r',b,l))
        <$>  Tr.sequence [topSide r, rightSide r, bottomSide r, leftSide r]

module RectangleMode
  ( Rectangle'
  , PartialRectangle(..)
  , extendToRectangle
  ) where

import           Base
import           Control.Lens hiding (view, element)
import           Data.Default.Class
import           Data.List.NonEmpty (NonEmpty(..))
import qualified Data.List.NonEmpty as NonEmpty
import           Data.Maybe (maybeToList)
import qualified Data.Sequence as Sequence
import           HGeometry.Box
import           HGeometry.Point
import           HGeometry.PolyLine
import           HGeometry.Sequence.NonEmpty


--------------------------------------------------------------------------------

-- | Shorthand
type Rectangle' r = Rectangle (Point 2 r)

-- | We store the anchor point of the rectangle
newtype PartialRectangle r = PartialRectangle (Point 2 r)
  deriving (Show,Read,Eq,Ord)

-- | Extends a box to a rectangle
extendToRectangle                         :: (Ord r, Num r)
                                          => PartialRectangle r
                                          -> Maybe (Point 2 r) -> Rectangle' r
extendToRectangle (PartialRectangle p) mq = boundingBox (p :| maybeToList mq)

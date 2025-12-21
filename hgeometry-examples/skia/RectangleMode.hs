module RectangleMode
  ( RectangleModeData(RectangleModeData), currentRect

  , Rectangle'
  , PartialRectangle(..)

  , HasAsRectangleWith(..)

  , startRectangleWith
  ) where

import           Base
import           Control.Lens hiding (view, element)
import           Data.Default
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

class HasAsRectangleWith t r | t -> r where
  -- | Renders the selection range as a rectangle (the first arg is the mouse coords used
  -- to complete the range)
  asRectangleWith :: (Ord r, Num r) => Maybe (Point 2 r) -> t -> Maybe (Rectangle' r)

instance HasAsRectangleWith (PartialRectangle r) r where
  asRectangleWith mq (PartialRectangle p) = Just $ boundingBox (p :| maybeToList mq)

--------------------------------------------------------------------------------

-- | Data we store in Rectangle mode
newtype RectangleModeData = RectangleModeData (Maybe (PartialRectangle R))
  deriving (Show,Read,Eq)

currentRect :: Lens' RectangleModeData (Maybe (PartialRectangle R))
currentRect = coerced

instance Default RectangleModeData where
  def = RectangleModeData Nothing

instance HasAsRectangleWith RectangleModeData R where
  asRectangleWith mousePos (RectangleModeData mpr) = mpr >>= asRectangleWith mousePos


-----------------------------------------------------------------------------------------

startRectangleWith                :: Maybe (Point 2 R) ->RectangleModeData -> RectangleModeData
startRectangleWith mousePos mData = case mousePos of
  Nothing -> mData
  Just p  -> mData&currentRect ?~ PartialRectangle p

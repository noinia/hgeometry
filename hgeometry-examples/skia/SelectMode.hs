{-# LANGUAGE TemplateHaskell            #-}
module SelectMode
  ( SelectModeData(SelectModeData), selectionRange, selection

  , SelectionRange(..), rectangleRange
  , Selection(..)

  , startSelectionWith
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
import           RectangleMode

--------------------------------------------------------------------------------

-- | A rectangle selection may either be incomplete or complete
data RectangleRange r = PartialSelection (PartialRectangle r)
                      | CompleteSelection (Rectangle' r)
                      deriving (Show,Read,Eq)

newtype SelectionRange r = RectangleSelection (RectangleRange r)
  deriving (Show,Read,Eq)

rectangleRange :: Lens' (SelectionRange r) (RectangleRange r)
rectangleRange = coerced

--------------------------------------------------------------------------------


data Selection a = Selection { _primarySelection   :: a
                             , _secondarySelection :: [a]
                             }
                   deriving (Show,Read,Eq)
-- what type are we going to store in these selections ?


--------------------------------------------------------------------------------

data SelectModeData =
  SelectModeData { _selectionRange :: Maybe (SelectionRange R)
                 , _selection      :: Maybe (Selection ())
                 }
  deriving (Show,Read,Eq)
makeLenses ''SelectModeData

instance Default SelectModeData where
  def = SelectModeData Nothing Nothing


instance HasAsRectangleWith SelectModeData R where
  asRectangleWith mousePos mData = (mData^?selectionRange._Just.rectangleRange) >>= \case
    PartialSelection pr -> asRectangleWith mousePos pr
    CompleteSelection r -> Just r




startSelectionWith                :: Maybe (Point 2 R) ->SelectModeData -> SelectModeData
startSelectionWith mousePos mData = case mousePos of
  Nothing -> mData
  Just p  -> let selRange = RectangleSelection . PartialSelection $ PartialRectangle p
             in SelectModeData (Just selRange) Nothing

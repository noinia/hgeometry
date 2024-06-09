{-# LANGUAGE TemplateHaskell            #-}
module SelectMode
  ( SelectModeData(SelectModeData), selectionRange, selection

  , SelectionRange(..), rectangleRange
  , Selection(..)

  , updateSelection
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
import           Miso
import           RectangleMode

--------------------------------------------------------------------------------

-- -- | A rectangle selection may either be incomplete or complete
-- data RectangleRange r = PartialSelection (PartialRectangle r)
--                       | CompleteSelection (Rectangle' r)
--                       deriving (Show,Read,Eq)

newtype SelectionRange r = RectangleSelection (PartialRectangle r)
  deriving (Show,Read,Eq)

rectangleRange :: Lens' (SelectionRange r) (PartialRectangle r)
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
  asRectangleWith mousePos mData = (mData^?selectionRange._Just.rectangleRange) >>= \pr ->
                                     asRectangleWith mousePos pr

-- data ComputeSelectionAction = ComputeSelection (Rectangle' R)
--   deriving (Show,Eq)


updateSelection                                 :: (Rectangle' R -> computeSelectionAction)
                                                -> Maybe (Point 2 R)
                                                -> SelectModeData
                                                -> Effect computeSelectionAction SelectModeData
updateSelection computeSelection mousePos mData = case mousePos of
    Nothing -> noEff mData
    Just p  -> case mData^.selectionRange of
      Nothing -> noEff $ startSelection p
      Just pr -> (mData&selectionRange .~ Nothing) -- reset the selection range
                 <# pure (computeSelection $ extend (pr^.rectangleRange) p) -- send a ComputeSelection action
  where
    startSelection p = SelectModeData (Just $ selRange p) Nothing
    selRange = RectangleSelection . PartialRectangle

    extend (PartialRectangle s) p = boundingBox (s :| [p])

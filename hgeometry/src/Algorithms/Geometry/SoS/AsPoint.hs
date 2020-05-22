module Algorithms.Geometry.SoS.AsPoint where

import           Control.Lens
import           Control.CanAquire
import           Control.Monad.ST.Strict
import           Control.Monad.State.Strict
import           Data.Bifunctor
import           Data.Ext
import           Data.Foldable (toList)
import           Data.Geometry.Point.Class
import           Data.Geometry.Point.Internal
import           Data.Geometry.Properties
import           Data.Geometry.Transformation
import qualified Data.Geometry.Vector as GV
import           Data.Geometry.Vector hiding (imap)
import qualified Data.List as List
import           Data.List.NonEmpty (NonEmpty(..))
import qualified Data.List.NonEmpty as NonEmpty
import qualified Data.Map as Map
import           Data.Maybe
import           Data.Ord (Down(..))
import           Data.Reflection
import           Data.Util
import qualified Data.Vector as V
import qualified Data.Vector.Mutable as MV
import           GHC.TypeNats
import           Linear.Matrix
import           Linear.V2 (V2(..))
import           Linear.V3 (V3(..))
import           Linear.V4 (V4(..))


--------------------------------------------------------------------------------

class AsPoint p where
  asPoint :: p -> Point (Dimension p) (NumType p)

instance AsPoint (Point d r) where
  asPoint = id

instance AsPoint p => AsPoint (p :+ e) where
  asPoint = asPoint . view core
  {-# INLINE asPoint #-}

--------------------------------------------------------------------------------

-- | a P is a 'read only' point in d dimensions
newtype P i d r = P i deriving (HasIndex, Eq, Show)

instance i `CanAquire` (Point d r) => (P i d r) `CanAquire` (Point d r) where
  aquire (P i) = aquire i

type instance NumType   (P i d r) = r
type instance Dimension (P i d r) = d


instance i `CanAquire` (Point d r) => AsPoint (P i d r) where
  asPoint (P i) = aquire i

asPointWithIndex       :: (Arity d, i `CanAquire` Point d r)
                       => P i d r -> Point d (RWithIdx r)
asPointWithIndex (P i) = Point . imap (\j r -> RWithIdx r (indexOf i) j) . toVec $ aquire i

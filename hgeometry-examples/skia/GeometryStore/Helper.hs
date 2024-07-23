module GeometryStore.Helper
  ( lensFieldNamer
  ) where

import           Control.Lens
import           Data.Char (toUpper)


--------------------------------------------------------------------------------

-- | method to generate fieldnames to be used in the store implemention
lensFieldNamer :: FieldNamer
lensFieldNamer = mappingNamer (\(_:c:fld) -> ["geomstore" <> (toUpper c : fld)])
-- Due to GHC's stage restriction this needs to be in a different file than the store
-- itself apparently.

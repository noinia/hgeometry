--------------------------------------------------------------------------------
-- |
-- Module      :  Data.Geometry.PlnarSubdivision.Raw
-- Copyright   :  (C) Frank Staals
-- License     :  see the LICENSE file
-- Maintainer  :  Frank Staals
-- Description :  The 'Raw' building block used in a Planar Subdivision
--
--------------------------------------------------------------------------------
module Data.Geometry.PlanarSubdivision.Raw where

import Control.Lens
import Data.Aeson
import Data.PlaneGraph (FaceId')
import GHC.Generics (Generic)

--------------------------------------------------------------------------------

-- | Helper data type and type family to Wrap a proxy type.
data Wrap' s
type family Wrap (s :: k) :: k where
  Wrap s = Wrap' s

-- | ComponentId type
newtype ComponentId s = ComponentId { unCI :: Int }
  deriving (Show,Eq,Ord,Generic,Bounded,Enum,ToJSON,FromJSON)



-- | Helper type for the data that we store in a planar subdivision
data Raw s ia a = Raw { _compId  :: !(ComponentId s)
                      , _idxVal  :: !ia
                      , _dataVal :: !a
                      } deriving (Eq,Show,Functor,Foldable,Traversable,Generic)

instance (FromJSON ia, FromJSON a) => FromJSON (Raw s ia a)
instance (ToJSON ia, ToJSON a) => ToJSON (Raw s ia a) where
  toEncoding = genericToEncoding defaultOptions

-- | get the dataVal of a Raw
dataVal :: Lens (Raw s ia a) (Raw s ia b) a b
dataVal = lens (\(Raw _ _ x) -> x) (\(Raw c i _) y -> Raw c i y)


-- | A FaceIdx is a pair of componentId and FaceId
data FaceIdx s = RawIdx !(ComponentId s) -- ^ component Id
                        !(FaceId' (Wrap s)) -- ^ Face index inside the component
               deriving (Eq,Show,Generic)

instance FromJSON (FaceIdx s)
instance ToJSON (FaceIdx s) where
  toEncoding = genericToEncoding defaultOptions


data RawFace s a = RawFace { _internalFaceIn :: !(Maybe (FaceIdx s))
                           , _externalFaceIn :: ![FaceIdx s]
                           , _faceDataValue  :: !a
                           } deriving (Eq,Show,Functor,Foldable,Traversable,Generic)

instance FromJSON a => FromJSON (RawFace s a)
instance ToJSON a => ToJSON (RawFace s a) where
  toEncoding = genericToEncoding defaultOptions

-- | get the data value of a raw face.
faceDataVal :: Lens (RawFace s a) (RawFace s b) a b
faceDataVal = lens _faceDataValue (\(RawFace i es _) x -> RawFace i es x)

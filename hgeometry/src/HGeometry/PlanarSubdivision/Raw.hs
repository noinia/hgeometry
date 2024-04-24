{-# LANGUAGE TypeData #-}
--------------------------------------------------------------------------------
-- |
-- Module      :  HGeometry.PlnarSubdivision.Raw
-- Copyright   :  (C) Frank Staals
-- License     :  see the LICENSE file
-- Maintainer  :  Frank Staals
-- Description :  The 'Raw' building block used in a Planar Subdivision
--
--------------------------------------------------------------------------------
module HGeometry.PlanarSubdivision.Raw
  ( Wrap
  , ComponentId(..)
  , Raw(Raw), dataVal
  , RawFace(RawFace), faceIdx, faceDataVal
  , FaceData(FaceData), holes, fData
  ) where

import           Control.Lens hiding (holes)
import           Data.Aeson
import           Data.Kind (Type)
import qualified Data.Sequence as Seq
import           GHC.Generics (Generic)
import           Hiraffe.PlanarGraph (FaceId)
import           Hiraffe.PlanarGraph.Dart (Dart)

--------------------------------------------------------------------------------

-- | Helper data type and type family to Wrap a proxy type.
type data Wrap' (s :: k)
type family Wrap (s :: k) :: Type where
  Wrap s = Wrap' s

-- | ComponentId type
newtype ComponentId s = ComponentId { unCI :: Int }
  deriving (Show,Eq,Ord,Generic,Bounded,Enum,ToJSON,FromJSON)

--------------------------------------------------------------------------------

-- | Helper type for the data that we store in a planar subdivision
data Raw s ia a = Raw { _compId  :: {-# UNPACK #-} !(ComponentId s)
                      , _idxVal  :: !ia
                      , _dataVal :: !a
                      } deriving (Eq,Show,Functor,Foldable,Traversable,Generic)

instance (FromJSON ia, FromJSON a) => FromJSON (Raw s ia a)
instance (ToJSON ia, ToJSON a) => ToJSON (Raw s ia a) where
  toEncoding = genericToEncoding defaultOptions

instance FunctorWithIndex i (Raw ci i) where
  imap f (Raw ci i x) = Raw ci i (f i x)
instance FoldableWithIndex i (Raw ci i) where
  ifoldMap f (Raw _ i x) = f i x
instance TraversableWithIndex i (Raw ci i) where
  itraverse f (Raw ci i x) = Raw ci i <$> f i x


-- | get the dataVal of a Raw
dataVal :: Lens (Raw s ia a) (Raw s ia b) a b
dataVal = lens (\(Raw _ _ x) -> x) (\(Raw c i _) y -> Raw c i y)

--------------------------------------------------------------------------------

-- | The Face data consists of the data itself and a list of holes
data FaceData h f = FaceData { _holes :: Seq.Seq h
                             , _fData :: !f
                             } deriving (Show,Eq,Ord,Functor,Foldable,Traversable,Generic)

-- | lens to access the holes of a face
holes :: Lens (FaceData h f) (FaceData h' f) (Seq.Seq h) (Seq.Seq h')
holes = lens _holes (\fd hs -> fd { _holes = hs })
{-# INLINE holes #-}

-- | Lens to access the actual face data
fData :: Lens (FaceData h f) (FaceData h f') f f'
fData = lens _fData (\fd x -> fd { _fData = x })
{-# INLINE fData #-}

instance Bifunctor FaceData where
  bimap f g (FaceData hs x) = FaceData (fmap f hs) (g x)


instance (FromJSON h, FromJSON f) => FromJSON (FaceData h f)
instance (ToJSON h, ToJSON f)     => ToJSON (FaceData h f) where
  toEncoding = genericToEncoding defaultOptions

--------------------------------------------------------------------------------

-- | Face data, if the face is an inner face, store the component and
-- faceId of it.  If not, this face must be the outer face (and thus
-- we can find all the face id's it correponds to through the
-- FaceData).
data RawFace s f = RawFace { _faceIdx     :: !(Maybe (ComponentId s, FaceId (Wrap s)))
                           , _faceDataVal :: !(FaceData (Dart s) f)
                           } deriving (Eq,Show,Functor,Foldable,Traversable,Generic)

-- TODO: use unpacked/strict values for the faceIdx

-- | Lens to access the faceIx (if it exists)
faceIdx :: Lens' (RawFace s f) (Maybe (ComponentId s, FaceId (Wrap s)))
faceIdx = lens _faceIdx (\rf x -> rf { _faceIdx = x })
{-# INLINE faceIdx #-}

-- | Lens to access the face data
faceDataVal :: Lens (RawFace s f)  (RawFace s f') (FaceData (Dart s) f) (FaceData (Dart s) f')
faceDataVal = lens _faceDataVal (\rf x -> rf { _faceDataVal = x })
{-# INLINE faceDataVal #-}

-- -- | A FaceIdx is a pair of componentId and FaceId
-- data FaceIdx s = RawIdx !(ComponentId s) -- ^ component Id
--                         !(FaceId (Wrap s)) -- ^ Face index inside the component
--                deriving (Eq,Show,Generic)

-- instance FromJSON (FaceIdx s)
-- instance ToJSON (FaceIdx s) where
--   toEncoding = genericToEncoding defaultOptions

-- data RawFace s a = RawFace { _internalFaceIn :: !(Maybe (FaceIdx s))
--                            , _externalFaceIn :: ![FaceIdx s]
--                            , _faceDataValue  :: !a
--                            } deriving (Eq,Show,Functor,Foldable,Traversable,Generic)

-- instance FromJSON f => FromJSON (RawFace s f)
-- instance ToJSON f => ToJSON (RawFace s f) where
--   toEncoding = genericToEncoding defaultOptions

-- -- | get the data value of a raw face.
-- faceDataVal :: Lens (RawFace s a) (RawFace s b) a b
-- faceDataVal = lens _faceDataValue (\(RawFace i es _) x -> RawFace i es x)

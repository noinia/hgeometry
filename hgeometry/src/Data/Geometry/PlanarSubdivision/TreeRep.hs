--------------------------------------------------------------------------------
-- |
-- Module      :  Data.Geometry.PlanarSubdivision.TreeRep
-- Copyright   :  (C) Frank Staals
-- License     :  see the LICENSE file
-- Maintainer  :  Frank Staals
--
-- Data types to represent a PlanarSubdivision using adjacency representations
--
--------------------------------------------------------------------------------
module Data.Geometry.PlanarSubdivision.TreeRep
  ( PlanarSD(..), InnerSD
  , Gr(..), Vtx(..),
  , myTreeRep
  ) where

-- FIXME; uncomment myTreeRep

import Data.Aeson
import Data.PlaneGraph.AdjRep (Vtx(..), Gr(..))
import GHC.Generics (Generic)
import Data.Geometry.Point
import Data.RealNumber.Rational
import Data.Traversable
import Data.Bitraversable

--------------------------------------------------------------------------------

-- | Specify the planar subdivison as a tree of components
data PlanarSD v e f r = PlanarSD
  { outerFace  :: f           -- ^ outer face
  , components :: [InnerSD v e f r] -- ^ for each component, an inner
  } deriving (Show,Eq,Generic)

-- | InnerSD is essentially a component
type InnerSD v e f r = Gr (Vtx v e r) (Face v e f r)

instance Functor (PlanarSD v e f) where
  fmap = fmapDefault
instance Foldable (PlanarSD v e f) where
  foldMap = foldMapDefault
instance Traversable (PlanarSD v e f) where
  traverse f (PlanarSD x cs) = PlanarSD x <$> traverse (traverseInnerSD f) cs


instance (ToJSON r,   ToJSON v, ToJSON e, ToJSON f)     => ToJSON   (PlanarSD v e f r) where
  toEncoding = genericToEncoding defaultOptions
instance (FromJSON r, FromJSON v, FromJSON e, FromJSON f) => FromJSON (PlanarSD v e f r)


-- | traverse for InnerSD.
traverseInnerSD   :: Applicative g => (r -> g r') -> InnerSD v e f r -> g (InnerSD v e f r')
traverseInnerSD f = bitraverse (traverse f) (traverse f)


data Face v e f r = Face { incidentEdge :: (Int,Int)
                           -- ^ an edge (u,v) s.t. the face
                            -- is right from (u,v)
                         , fData        :: !f
                         , holes        :: [InnerSD v e f r]
                         } deriving (Generic, Show, Eq)

instance Functor (Face v e f) where
  fmap = fmapDefault
instance Foldable (Face v e f) where
  foldMap = foldMapDefault
instance Traversable (Face v e f) where
  traverse f (Face e x hs) = Face e x <$> traverse (traverseInnerSD f) hs


instance (ToJSON r,   ToJSON v, ToJSON e, ToJSON f)     => ToJSON   (Face v e r f) where
  toEncoding = genericToEncoding defaultOptions
instance (FromJSON r, FromJSON v, FromJSON e, FromJSON f) => FromJSON (Face v e r f)



--------------------------------------------------------------------------------

-- | This represents the following Planar subdivision. Note that the
-- graph is undirected, the arrows are just to indicate what the
-- Positive direction of the darts is.
--
-- ![mySubDiv](docs/Data/Geometry/PlanarSubdivision/mySubDiv.jpg)
myTreeRep :: PlanarSD Int () String (RealNumber 3)
myTreeRep = PlanarSD "f_infty" [ Gr ads1 fs1
                               , Gr ads2 fs2
                               ]
  where
    -- hole 1 in the outer face
    ads1 = [ vtx 9 (Point2 4    (-4)) [e 10, e 11]
           , vtx 10 (Point2 8   (-4)) [e 11, e 9]
           , vtx 11 (Point2 11  (-2)) [e 10, e 12]
           , vtx 12 (Point2 7   (-1)) [e 9, e 11]
           ]
    fs1= [ Face (10,11) "f_1" []]

    -- hole 2 in the outer face
    ads2 = [ vtx 0 (Point2 0    0)    [e 1, e 4]
           , vtx 1 (Point2 10   2)    [e 0, e 5]
           , vtx 2 (Point2 9    9)    [e 1, e 7, e 3]
           , vtx 3 (Point2 0    10)   [e 2, e 4]
           , vtx 4 (Point2 (-4) 5)    [e 0, e 3]
           , vtx 5 (Point2 15   3)    [e 1, e 6]
           , vtx 6 (Point2 20   6)    [e 5, e 7]
           , vtx 7 (Point2 10   14)   [e 2, e 6, e 8]
           , vtx 8 (Point2 4    13)   [e 7, e 3]
           ]
    fs2 = [ Face (0,4) "f_2" [f5, f6]
          , Face (3,8) "f_3" []
          , Face (2,7) "f_4" [f7]
          ]

    f5 = Gr [ vtx 16 (Point2 3    8) [e 17, e 18]
            , vtx 17 (Point2 0    7) [e 16, e 18]
            , vtx 18 (Point2 (-1) 4) [e 16, e 17]
            ] [Face (18,17) "f_5" []]

    f6 = Gr [ vtx 15 (Point2 3   3) [e 14, e 13]
            , vtx 13 (Point2 6   4) [e 14, e 15]
            , vtx 14 (Point2 3   6) [e 13, e 15]
            ] [Face (15,14) "f_6" []]

    f7 = Gr [ vtx 19 (Point2 0   9) [e 20, e 23]
            , vtx 20 (Point2 0   4) [e 19, e 21]
            , vtx 21 (Point2 15  2) [e 20, e 22]
            , vtx 22 (Point2 17  5) [e 21, e 23]
            , vtx 23 (Point2 15  8) [e 19, e 22]
            ] [Face (20,19) "f_7" [f8]]

    f8 = Gr [ vtx 24 (Point2 14  6) [e 25, e 26]
            , vtx 25 (Point2 13  8) [e 24, e 26]
            , vtx 26 (Point2 12  5) [e 24, e 25]
            ] [Face (26,25) "f_8" []]

    e i = (i,())

    vtx i p as = Vtx i p as i

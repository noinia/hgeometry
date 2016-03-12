{-# LANGUAGE TemplateHaskell #-}
module Data.PlanarGraph where

import Data.Permutation
import Data.Maybe
import Control.Monad(join, forM)
import Control.Lens
import qualified Data.Vector as V
import qualified Data.Vector.Generic as GV
import qualified Data.Vector.Unboxed.Mutable as UMV
import qualified Data.CircularList as C

--------------------------------------------------------------------------------

newtype Arc = Arc { _unArc :: Int } deriving (Show,Read,Eq,Ord,Enum,Bounded)

data Direction = Negative | Positive deriving (Show,Eq,Ord,Read,Bounded,Enum)

rev          :: Direction -> Direction
rev Negative = Positive
rev Positive = Negative

data Dart = Dart !Arc !Direction deriving (Show,Eq,Ord,Read)

twin            :: Dart -> Dart
twin (Dart a d) = Dart a (rev d)






instance Enum Dart where
  toEnum x
    | even x    = Dart (Arc $ x `div` 2)       Positive
    | otherwise = Dart (Arc $ (x `div` 2) + 1) Negative
  -- get the back edge by adding one

  fromEnum (Dart (Arc i) d) = case d of
                                Positive -> 2*i
                                Negative -> 2*i + 1


newtype VertexId = VertexId { _unVertexId :: Int } deriving (Show,Eq,Ord)





newtype PlanarGraph = PlanarGraph { _permutation :: Permutation Dart }
                      deriving (Show,Eq)
makeLenses ''PlanarGraph


tailOf     :: Dart -> PlanarGraph -> VertexId
tailOf d g = VertexId . fst $ lookupIdx (g^.permutation) d

headOf   :: Dart -> PlanarGraph -> VertexId
headOf d = tailOf (twin d)

incidentEdges              :: VertexId -> PlanarGraph -> V.Vector Dart
incidentEdges (VertexId v) g = g^.permutation.orbits.ix' v


dual   :: PlanarGraph -> PlanarGraph
dual g = let perm = g^.permutation
         in PlanarGraph $ cycleRep (elems perm) (apply perm)


newtype FaceId = FaceId { _unF :: VertexId } deriving (Show,Eq,Ord)









testPerm = let (a:b:c:d:e:g:_) = take 6 [Arc 0..]
           in toCycleRep 12 [ [ Dart a Negative
                              , Dart c Positive
                              , Dart b Positive
                              , Dart a Positive
                              ]
                            , [ Dart e Negative
                              , Dart b Negative
                              , Dart d Negative
                              , Dart g Positive
                              ]
                            , [ Dart e Positive
                              , Dart d Positive
                              , Dart c Negative
                              ]
                            , [ Dart g Negative
                              ]
                            ]

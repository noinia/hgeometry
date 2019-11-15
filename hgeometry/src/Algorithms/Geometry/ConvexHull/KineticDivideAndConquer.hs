{-# LANGUAGE TemplateHaskell #-}
module Algorithms.Geometry.ConvexHull.KineticDivideAncConquer where

import           Control.Lens
import           Control.Monad.ST
import           Data.Ext
import           Data.Geometry.Point
import qualified Data.Vector as V
import qualified Data.Vector.Mutable as MV

type ConvexHull d p r = [Point 3 r :+ p]




convexHull :: [Point 3 r :+ p] -> ConvexHull 3 p r
convexHull = undefined


type Vertex = Int


data Event r = Event { eventTime :: !r
                     , eventKind :: !EventKind
                     } deriving (Show,Eq)

data EventKind = Insert !Vertex !Vertex -- ^ first new then old
               | Delete !Vertex
               deriving (Show,Eq,Ord)


data HullVertex = HullVertex { _prev :: !(Maybe Vertex)
                             , _next :: !(Maybe Vertex)
                             } deriving (Show,Eq,Ord)
makeLenses ''HullVertex

newtype Hull s r = Hull (MV.MVector s HullVertex)

deleteVertex            :: Vertex -> Hull s r -> ST s ()
deleteVertex v (Hull a) = do HullVertex mu mw <- MV.read a v
                             f next mw mu -- set mu^.next to mw
                             f prev mw mu
  where
    f l x = \case
      Nothing -> pure ()
      Just u  -> MV.modify a (&l .~ x) u


modify       :: MV.MVector s a -> (a -> a) -> Int -> ST s a
modify v f i = do x <- MV.read v i
                  let x' = f x
                  MV.write v i x'
                  pure x

insertVertexAfter :: Vertex -> Vertex -> Hull s r -> ST s ()
insertVertexAfter u v (Hull a) = do HullVertex _ ms <- modify a (&next .~ Just v) u
                                    MV.write a v (HullVertex (Just u) ms)
                                    case ms of
                                      Nothing -> pure ()
                                      Just s -> MV.modify a (&prev .~ Just v) s

data MergeStatus r = Merge { initialHull :: [Vertex]
                           , events      :: [Event r]
                           }

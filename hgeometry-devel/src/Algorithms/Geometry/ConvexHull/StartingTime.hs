{-# LANGUAGE TemplateHaskell #-}
module Algorithms.Geometry.ConvexHull.Movie where

import           Algorithms.DivideAndConquer
import           Algorithms.Geometry.ConvexHull.Helpers (groupOn)
import           Control.Arrow ((>>>))
import           Control.Lens
import           Control.Monad.State.Class (get, put)
import           Control.Monad.State.Strict (evalStateT)
import           Data.Bifoldable
import           Data.Bifunctor
import           Data.Bitraversable
import           Data.Coerce
import           Data.Ext
import qualified Data.Foldable as F
import           Data.Functor.Identity
import           Data.Geometry.Point
import           Data.Geometry.Vector
import qualified Data.List as List
import           Data.List.NonEmpty (NonEmpty(..))
import qualified Data.List.NonEmpty as NonEmpty
import qualified Data.Map as Map
import           Data.Maybe (catMaybes, fromMaybe)
import           Data.Ord (comparing)
import           Data.Semigroup
import           Data.Sequence (Seq(..))
import           Data.Typeable (TypeRep, Typeable, typeOf)
import           Data.Util
import           GHC.TypeNats
-- import           Data.Witherable.Class
import           Prelude hiding (Either(..))
import           Refined (Refined, unrefine, Predicate(..), throwRefine, RefineException(..))


--------------------------------------------------------------------------------


 -- proof that there is a vertical facet + slope to use
data StartingSlope p r = Vertical p p p [p] r -- use Refined (SizeGreaterThan 2) [p] ?
                       | NonVertical r

-- we need at least three points actually
-- startingSlope :: [p] -> StartingSlope p r
-- startingSlope = \case

-- data Proper3D

-- [Point d r] -> Point d [r]

data NotAllTheSame

data NotAllTheSameInDim (i :: Nat)

data All' (preds :: [*])
data Any (preds :: [*])


-- type Propper (d :: Nat) =

-- type Proper3D = All' [ NotAllTheSameInDim 1
--                      , NotAllTheSameInDim 2
--                      , NotAllTheSameInDim 3
--                      , NonEmpty
--                      ]



instance (Foldable f, Eq r) => Predicate NotAllTheSame (f r) where
  validate prf = F.toList >>> \case
    []                     -> pure ()
    (x:xs) | any (/= x) xs -> pure ()
           | otherwise     -> throwRefine $ RefineOtherException (typeOf prf)
                              "All elements are the same"




-- instance Foldable f => Predicate Proper3D (f (Point 3 r)) where
--   validate prf pts = all

--     sequenceA (coerce @[Point 3 r] @[Vector 3 r] pts)

--   pts' = case F.toList pts' of
--       pts@(_:_) | all extent $ -> pure ()
--       _  -> throwRefine $ RefineOtherException (typeOf prf) undefined
--     where
--       extent xs = minimum xs /= maximum xs

-- steepest     :: -- Refined (SizeGreaterThan 2, Proper3D Pointset)
--                 [Point 2 r] -> StartingSlope [Point 2 r] r
-- steepest pts = case extractMinimaBy (comparing (^.xCoord)) (unrefine pts) of
--                  (xs :+ rest) -> case minimaOn (^.yCoord) xs of
--                    []          -> error "nonsense; only one point "
--                    (p:q:r:xs') -> Vertical p q r xs' (steepestFrom p rest)
--                    (p:_)       -> NonVertical (steepestFrom p rest)
--                      -- I guess rest may even be empty! by the Proper3D constraint the
--                      -- the input should be proper


-- | Move this to JarvisMarch
steepestFrom   :: (Ord r, Num r)
               => (Point 2 r :+ a) -> NonEmpty (Point 2 r :+ b)  -> Point 2 r :+ b
steepestFrom p = List.minimumBy (ccwCmpAroundWith (Vector2 0 (-1)) p)

{-# LANGUAGE UndecidableInstances #-}
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
import           Refined (Refined, unrefine, Predicate(..), throwRefine, RefineException(..))
import qualified Refined

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



data Sized (l :: k) (u :: k')

data Void
-- data Ops = Void :> Void
--          | Void :>= Void



data Exact
data L
data U

instance (Predicate (Refined.SizeEqualTo n) t, KnownNat n)
       => Predicate (Sized Exact n) t where
  validate _ = validate @(Refined.SizeEqualTo n) undefined

instance ( Predicate (Refined.SizeGreaterThan l') t
         , KnownNat l, KnownNat l'
         , l ~ (1 + l')
         )
       => Predicate (Sized l U) t where
  validate _ = validate @(Refined.SizeGreaterThan l') undefined

instance (Predicate (Refined.SizeLessThan (1+u)) t, KnownNat u)
       => Predicate (Sized L u) t where
  validate _ = validate @(Refined.SizeLessThan (1+u)) undefined

instance ( Predicate (Refined.SizeGreaterThan l')    t, KnownNat l, KnownNat l', l ~ (1+l')
         , Predicate (Refined.SizeLessThan    (1+u)) t, KnownNat u)
         => Predicate (Sized l u) t where
  validate _ = validate @(Refined.And (Refined.SizeGreaterThan l')
                                      (Refined.SizeLessThan (1+u))
                         ) undefined


test :: Either RefineException (Refined (Sized 3 10) [Int])
test = Refined.refine $ [1..10]

test2 :: Either RefineException (Refined (Sized 3 10) [Int])
test2 = Refined.refine $ [1..3]

test3 :: Either RefineException (Refined (Sized 3 10) [Int])
test3 = Refined.refine $ [1..100]

test4 :: Either RefineException (Refined (Sized 3 10) [Int])
test4 = Refined.refine $ [1..2]


data Size
data a <=. b
data a >=. b


instance ( Predicate (Refined.SizeGreaterThan l') t
         , KnownNat l, KnownNat l'
         , l ~ (1 + l')
         )
       => Predicate (Size >=. l) t where
  validate _ = validate @(Refined.SizeGreaterThan l') undefined

instance (Predicate (Refined.SizeLessThan (1+u)) t, KnownNat u)
       => Predicate (Size <=. u) t where
  validate _ = validate @(Refined.SizeLessThan (1+u)) undefined


test5 :: Either RefineException (Refined (Size >=. 20) [Int])
test5 = Refined.refine $ [1..10]

test6 :: Either RefineException (Refined (Size <=. 4) [Int])
test6 = Refined.refine $ [1..10]

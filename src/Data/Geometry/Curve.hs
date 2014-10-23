{-# LANGUAGE UnicodeSyntax #-}
module Data.Geometry.Curve where

import Data.Geometry.Point
import Data.Vinyl hiding (Nat)

import qualified Data.Vinyl.TypeLevel as TV


import GHC.TypeLits

newtype PPoint d r elF rs = PP (Point d elF rs r)

type PointList d r elF = Rec (PPoint d r elF)

data CurveFields u = CurveField
                   | CExt u

type family CExtF d r plElF uElF :: * where
  CExtF CurveField = PointList d r plElF
  CExtF (CExt f)   = Apply uElF f

newtype CurveAttr d r plElF uElF = CurveAttr { _unCurveAttr :: CExtF d r plElF uElF }


newtype Curve d r elF rs = Curve (Rec (CurveAttr d r )





-- type Height = Int

data SkewTree a = Empty
                | InternalNode !(SkewTree a) a !(SkewTree a)
                | TopNode      !(SkewTree a) a !(SkewTree a) !(SkewTree a)
                deriving (Show,Eq)

leaf a = InternalNode Empty a Empty

height Empty = 0
height (InternalNode l _ _) = 1 + height l
height (TopNode l _ _ _)    = 1 + height l


splitToInternal (TopNode l x r n) = (InternalNode l x r, n)
splitToInternal Empty               = error "splitToInternal: Empty"
splitToInternal _                   = error "splitToInternal: InternalNode"

isEmpty Empty = True
isEmpty _     = False

insert x Empty = TopNode Empty x Empty Empty
insert x t     = let (l,t') = splitToInternal t
                     (r,n)  = splitToInternal t'
                 in if (not $ isEmpty t' ) && height l == height r
                    then TopNode l     x r     n
                    else TopNode Empty x Empty t



lookup' x Empty = False
lookup' x (InternalNode l y r) = or [x == y, lookup' x l, lookup' x r]
lookup' x (TopNode l y r n)    = or [x == y, lookup' x l, lookup' x r, lookup' x n]
















data SkewRec (elF :: u -> *) (rt :: SkewTree u) where
  SEmpty        ::                     SkewRec elF Empty
  SInternalNode :: !(SkewRec elF l)
                -> !(elF fld)
                -> !(SkewRec elF r) -> SkewRec elF (InternalNode l fld r)
  STopNode      :: !(SkewRec elF l)
                -> !(elF fld)
                -> !(SkewRec elF r)
                -> !(SkewRec elF n) -> SkewRec elF (TopNode l fld r n)

type family Height (t :: SkewTree a) :: TV.Nat where
  Height Empty                  = TV.Z
  Height (InternalNode l x r)   = TV.S (Height l)
  Height (TopNode      l x r n) = TV.S (Height l)

type family SkewInsert (x :: a) (t :: SkewTree a) :: SkewTree a where
  SkewInsert x Empty = TopNode Empty x Empty Empty



class InsertSkew (rt :: SkewTree u) where
  insertSkew :: elF fld -> SkewRec elF rt -> SkewRec elF (SkewInsert fld rt)


-- insertSR :: elF fld -> SkewRec elF rt





-- startPoint :: "start" ::: Point d r p
-- startPoint = Field :: "start" ::: Point d r p

-- endPoint :: "end" ::: Point d r p
-- endPoint = Field :: "end" ::: Point d r p


-- class (HasDimension c, HasNumType c) => HasParametrization c where
--   atTime :: (d ~ Dimension c, r ~ NumType c) =>
--             c -> r -> Point d elF rs r




-- data CField u = CurvePoint Nat | CExt u



-- newtype Curve (d :: Nat) (elF :: TyFun u * -> *)


--                     (elF :: )

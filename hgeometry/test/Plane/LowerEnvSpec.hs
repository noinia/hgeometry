{-# LANGUAGE UndecidableInstances #-}
module Plane.LowerEnvSpec
  -- ( spec
  -- ) where
  where

import           Data.Semigroup
import           Control.Lens
import           HGeometry.Point
import qualified Data.Foldable as F
import           Data.Foldable1
import           Data.List.NonEmpty (NonEmpty(..))
import qualified Data.List.NonEmpty as NonEmpty
import qualified Data.Map as Map
import           Data.Map (Map)
import           Data.Set (Set)
import qualified Data.Set as Set
import           HGeometry.HyperPlane
import           HGeometry.Instances ()
import           HGeometry.Intersection
import           HGeometry.Matrix
import           HGeometry.Number.Real.Rational
import           HGeometry.Plane.LowerEnvelope.Connected (VertexForm
                                                           , intersectionPoint
                                                           , definers
                                                         )
import qualified HGeometry.Plane.LowerEnvelope.Connected.BruteForce as BruteForce
import qualified HGeometry.Plane.LowerEnvelope.Connected.Randomized as Randomized
import           HGeometry.Vector
import           System.Random
import           Test.Hspec
import           Test.Hspec.QuickCheck
import           Test.QuickCheck
import           Test.QuickCheck.Instances ()

import           HGeometry.Combinatorial.Util
import           Debug.Trace
--------------------------------------------------------------------------------

type R = RealNumber 5


newtype NonDegenerate plane = NonDegenerate (NonEmpty plane)
  deriving newtype (Show,Eq,Foldable,Functor,Foldable1)

setOf     :: Ord a => Gen a -> Gen (Set a)
setOf gen = Set.fromList <$> listOf gen

instance Arbitrary (NonDegenerate (Plane R)) where
  arbitrary = do h0 <- arbitrary
                 h1 <- arbitrary `suchThat` nonParallelWith h0
                 h2 <- arbitrary `suchThat` \h -> det (mkMatrix h0 h1 h) /= 0
                   -- \h' -> nonParallelWith h0 h' && nonParallelWith h1 h'
                 rest <- setOf (arbitrary  `suchThat` (`notElem` [h0,h1,h2]))
                 pure . NonDegenerate $ h0 :| (h1 : h2 : Set.toList rest)
  -- according to
  -- https://www.mathspanda.com/A2FM/Lessons/Intersections_of_planes_LESSON.pdf
  -- the three planes intersect in a point only when their determinant is non-zero.

mkMatrix :: Plane R -> Plane R -> Plane R -> Matrix 3 3 R
mkMatrix (Plane a1 b1 c1) (Plane a2 b2 c2) (Plane a3 b3 c3) =
  matrixFromRows (Vector3 (Vector3 a1 b1 c1) (Vector3 a2 b2 c2) (Vector3 a3 b3 c3))

-- instance Arbitrary (NonDegenerate (Plane R)) where
--   arbitrary = fmap f <$> arbitrary
--     where
--       f :: Plane Int -> Plane R
--       f = fmap fromIntegral


nonParallelWith      :: Plane R -> Plane R -> Bool
nonParallelWith h h' = case h `intersect` h' of
                         Just (Plane_x_Plane_Line _) -> True
                         _                           -> False




newtype Same = Same (VertexForm Map R (Plane R))
  deriving newtype (Show)

instance Eq Same where
  (Same env) == (Same env') = and
                            $ zipWith (<=>) (Map.toAscList env) (Map.toAscList env')
    where
      (p,defs) <=> (q,defs') = p == q
                               &&
                               Set.fromList (F.toList defs) == Set.fromList (F.toList defs')


spec :: Spec
spec = xdescribe "Lower Envelope tests" $ do
         it "manual" $
           let seed   = 0
               planes = NonDegenerate $ NonEmpty.fromList [ Plane 1    0 2
                                                          , Plane (-1) 0 3
                                                          , Plane 0    1 10
                                                          ]
           in Same (Randomized.computeVertexForm (mkStdGen seed) planes)
              `shouldBe`
              Same (BruteForce.computeVertexForm planes)

         describe "bug cmpPlanesAround" $ do
           let
               env = traceShowWith ("env",) $ Randomized.computeVertexForm (mkStdGen 0) buggyPlanes
           prop "bug" $
             all (`BruteForce.belowAll` buggyPlanes) $ Map.keys env


         -- FIXME: Somehow The above indicates a bug in 'mergeDefiners'
         -- where we call cmpPlanes with three planes that apparently don't intersect in
          -- a point. In particular with:

-- *** Exception: cmpPlanesAround: precondition failed(NonVerticalHyperPlane [-1.0,-1.0,1.0],NonVerticalHyperPlane [-1.0,0.0,0.0],NonVerticalHyperPlane [-1.0,1.0,-1.0])

         -- this is due to :
         -- trying to sort the list
         -- [NonVerticalHyperPlane [-1.0,0.0,0.0],NonVerticalHyperPlane [-1.0,1.0,-1.0],NonVerticalHyperPlane [1.0,1.0,0.0]]

          -- where h0 = NonVerticalHyperPlane [-1.0,-1.0,1.0]


         -- FIXME: I think there may just be an issue with how we generate the planes
         -- since raising this to 50 seems not to work
         modifyMaxSize (const 20) $ do
           prop "every vertex is valid" $
             \seed (planes :: NonDegenerate (Plane R)) ->
               all (`BruteForce.belowAll` planes) $
                 Map.keys (Randomized.computeVertexForm (mkStdGen seed) planes)

           prop "same as brute force" $
             \seed (planes :: NonDegenerate (Plane R)) ->
               Same (Randomized.computeVertexForm (mkStdGen seed) planes)
               ===
               Same (BruteForce.computeVertexForm planes)


buggyPlanes = NonEmpty.fromList
                   [ NonVerticalHyperPlane $ fromList' [1,1,0]

                   , NonVerticalHyperPlane $ fromList' [-1,1,-1]

                   , NonVerticalHyperPlane $ fromList' [-1,0,0]
                   , NonVerticalHyperPlane $ fromList' [-1,-1,1]
                   ]

defs0 =  NonVerticalHyperPlane (fromList' [-1.0,0.0,0.0]) :|
        [ NonVerticalHyperPlane (fromList' [1.0,1.0,0.0])
        , NonVerticalHyperPlane (fromList' [-1.0,1.0,-1.0])
        ]

defs1 = NonVerticalHyperPlane (fromList' [-1.0,-1.0,1.0]) :|
       [NonVerticalHyperPlane (fromList' [1.0,1.0,0.0])
       ,NonVerticalHyperPlane (fromList' [-1.0,1.0,-1.0])]




verifyAllContainV = it "allContain" $
                      all (\h -> 0.5 == evalAt (Point2 (-0.5) 1) h) buggyPlanes
                      -- all contain the point (-0.5, 1, 0.5)

-- (evalAt $ Point2 (-0.5) (1 + 1)) buggyPlanes

isLowestAbove = it "isLowestAbove" $
         minimumOn (evalAt $ Point2 (-0.5) (1 + 1)) buggyPlanes
                 `shouldBe` (buggyPlanes ^? ix 3)



                  -- ,Definers (NonVerticalHyperPlane [-1.0,0.0,0.0] :| [NonVerticalHype

foo = let f (a :| [b,c]) = definers (Three a b c)
      in (f defs0, f defs1)

-- myDefs = let (a :| [b,c,d]) = buggyPlanes
--          in intersectionPoint $ Three d b c

myDefs = let (a :| [b,c,d]) = buggyPlanes
         in definers $ Three d b c

-- rest = let h0 = buggyPlanes ^?! ix 3
--         in map (definers h0) [NonVerticalHyperPlane $ fromList' [-1.0,0.0,0.0]  -- h
--                              ,NonVerticalHyperPlane $ fromList' [-1.0,1.0,-1.0] -- h'
--                              ,NonVerticalHyperPlane $ fromList' [1.0,1.0,0.0]
--                              ]



-- ,NonVerticalHyperPlane [-1.0,0.0,0.0],NonVerticalHyperPlane [-1.0,1.0,-1.0]

-- ("h0",NonVerticalHyperPlane [-1.0,-1.0,1.0],", + ",

--   )

-- FIXME:
--
 -- cmpPlanesAround: precondition failed(NonVerticalHyperPlane [-1,-1,1],NonVerticalHyperPlane [-1,0,0],NonVerticalHyperPlane [-1,1,-1])
 --       (after 42 tests and 1 shrink)
 --         0
 --

fromList' [a,b,c] = Vector3 a b c
fromList' _ = error "fromList'"

minimumOn   :: (Ord b, Foldable f) => (a -> b) -> f a -> Maybe a
minimumOn f = fmap (\(Min (Arg _ x)) -> x) . foldMap (\x -> Just $ Min $ Arg (f x) x)

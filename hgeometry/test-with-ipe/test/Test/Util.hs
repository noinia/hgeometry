{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE OverloadedStrings #-}
module Test.Util
  ( difference, diffBy
  , NaiveSet(..)

  , ipeCounterExample

  , Input3(..)
  , assignColors

  , PointInTriangle(..)
  , barrycentric
  ) where

import           Control.Lens
import qualified Data.List as List
import Data.Map.Monoidal qualified as MonoidalMap
-- import qualified Data.Set as Set
import           HGeometry.Ext
import           HGeometry.Point
import           HGeometry.Vector
import           HGeometry.Line
import           HGeometry.Box
import           HGeometry.Polygon.Simple.PossiblyDegenerate
import           HGeometry.HalfSpace
import           HGeometry.Intersection
import           HGeometry.Triangle
import           GHC.Generics (Generic)
import           Ipe
import           Ipe.Color
import           Test.QuickCheck
import qualified Data.ByteString.Lazy.Char8 as B
import           Ipe.Draw
import           Ipe
import           HGeometry.Properties
import qualified Data.Set.NonEmpty as NESet
import           HGeometry.Instances ()
import           R

--------------------------------------------------------------------------------

-- | Computes all elements on which the two lists differ
difference :: Eq a => [a] -> [a] -> [a]
difference xs ys = (xs List.\\ ys) ++ (ys List.\\ xs)

-- differenceBy :: (a -> a -> Bool) -> [a] -> [a] -> [a]


diffBy :: (a -> a -> Bool) -> [a] -> [a] -> [a]
diffBy p xs ys = foldr (List.deleteBy p) ys xs

-- | \(O(n^2)\) set that ignores duplicates and order
newtype NaiveSet a = NaiveSet [a] deriving (Show)

instance Eq a => Eq (NaiveSet a) where
  (NaiveSet xs) == (NaiveSet ys) = List.null $ difference xs ys


--------------------------------------------------------------------------------
-- * QuickCheck Utils

-- | Utility to attach a drawing in the form of an ipeselection to a QuickCheck test
ipeCounterExample   :: forall prop a r.
                       ( Testable prop
                       , IsDrawable (Ipe r) a, NumType a ~ r
                       , IpeWriteText r
                       )
                    => a -> prop -> Property
ipeCounterExample x = case toIpeSelectionXML (draw @(Ipe r) [] x) of
                        Nothing -> property
                        Just b  -> counterexample (B.unpack b)

-- | Adds the item as an ipe-selection xml to the output (so that we
-- can easily copy paste it into ipe).
withIpeSelection   :: (IpeWrite t, Testable prop) => [t] -> prop -> Property
withIpeSelection g = counterexample (maybe err show $ toIpeSelectionXML g)
  where
    err = "error showing as ipe !?"

-- --------------------------------------------------------------------------------

-- qcReadShow1 :: forall s. (Arbitrary1 s, Read1 s, Eq1 s, Show1 s) => Proxy s -> Property
-- qcReadShow1 Proxy =
--     runner @Double           (\v -> read1 (show1 v) ==== v) .&&.
--     runner @Rational         (\v -> read1 (show1 v) ==== v) .&&.
--     runner @(Double, Double) (\v -> read1 (show1 v) ==== v) .&&.
--     runner @(Maybe Int)      (\v -> read1 (show1 v) ==== v) .&&.
--     runner @Int              (\v ->
--       case readf1 (showf1 (Just v)) of
--         Nothing -> counterexample "nested Read/Show failure" False
--         Just v' -> v ==== v')
--   where
--     runner :: (Show a, Arbitrary a) => (s a -> Property) -> Property
--     runner = forAll1 @s

-- forAll1 :: (Show1 s, Show a, Arbitrary1 s, Arbitrary a) => (s a -> Property) -> Property
-- forAll1 = forAllShow arbitrary1 show1

-- show1 :: (Show1 s, Show a) => s a -> String
-- show1 x = liftShowsPrec showsPrec showList 0 x ""

-- showf1 :: (Show1 f, Show1 s, Show a) => f (s a) -> String
-- showf1 x = liftShowsPrec (liftShowsPrec showsPrec showList) (liftShowList showsPrec showList) 0 x ""

-- read1 :: (Read1 s, Read a) => String -> s a
-- read1 inp =
--   case liftReadsPrec readsPrec readList 0 inp of
--     [(val,_rest)] -> val
--     []            -> error "no parse"
--     _             -> error "ambiguous parse"

-- readf1 :: (Read1 f, Read1 s, Read a) => String -> f (s a)
-- readf1 inp =
--   case liftReadsPrec (liftReadsPrec readsPrec readList) (liftReadList readsPrec readList) 0 inp of
--     [(val,_rest)] -> val
--     []            -> error "no parse"
--     _             -> error "ambiguous parse"

-- infix 4 ====
-- (====) :: (Eq1 s, Show1 s, Eq a, Show a) => s a -> s a -> Property
-- x ==== y =
--   counterexample (show1 x ++ interpret res ++ show1 y) res
--   where
--     res = x `eq1` y
--     interpret True  = " == "
--     interpret False = " /= "

-- --------------------------------------------------------------------------------

-- newtype ZeroToOne = ZeroToOne Rational

-- instance Show ZeroToOne where
--   show (ZeroToOne r) = show r

-- instance Arbitrary ZeroToOne where
--   arbitrary = do
--     k <- chooseInteger (0, granularity)
--     pure $ ZeroToOne $ k % granularity
--     where
--       granularity = 1000000
--   shrink (ZeroToOne 1) = []
--   shrink (ZeroToOne 0) = []
--   shrink (ZeroToOne r) = [ ZeroToOne $ div (numerator r) 2 % div (denominator r) 2]

-- --------------------------------------------------------------------------------

-- byStrokeColour :: (Stroke ∈ ats, Ord (Apply f Stroke))
--                => [a :+ Attributes f ats] -> [[a :+ Attributes f ats]]
-- byStrokeColour = map (map fst) . List.groupBy ((==) `on` snd) . List.sortOn snd
--                . map (\x -> (x,lookup' x))
--   where
--     lookup' (_ :+ ats) = lookupAttr (Proxy :: Proxy Stroke) ats



-- -- | Given a file with some file contents and a procedure that produces a
-- -- bytestring. Verify that the bytestring that we produce is the same as the
-- -- one stored in the file. If not, the output is stored in a temporary file so
-- -- that we can later look at the details.
-- runOnFile             :: String    -- ^ the description
--                       -> FilePath  -- ^ the expected output file
--                       -> IO B.ByteString -- ^ the algorithm to run.
--                       -> Spec
-- runOnFile s expFP alg = runOnFile' s expFP (\h -> alg >>= B.hPut h)


-- data Res = Res Bool FilePath FilePath
--          | True' deriving (Show)

-- instance Eq Res where
--   Res b _ _ == _ = b
--   True'     == _ = True

-- -- | Given a file with some file contents and a procedure that produces a
-- -- bytestring. Verify that the bytestring that we produce is the same as the
-- -- one stored in the file. If not, the output is stored in a temporary file so
-- -- that we can later look at the details.
-- runOnFile'                 :: String    -- ^ the description
--                            -> FilePath  -- ^ the expected output file
--                            -> (Handle -> IO ()) -- ^ the algorithm to run.
--                            -> Spec
-- runOnFile' descr expFP alg = it descr $ do
--                                runAlgo `shouldReturn` True'
--   where
--     runAlgo = do
--                 dir <- getTemporaryDirectory
--                 outFP <- bracket (openTempFile dir outFPName)
--                                  (hClose . snd)
--                                  (\(fp,h) -> do
--                                      alg h
--                                      pure fp)
--                 res <- sameFile expFP outFP
--                 when res $ removeFile outFP
--                 pure $ Res res expFP outFP
--     outFPName = "hgeometry_runOnFile_algo" <> takeExtension expFP


-- -- | Test if two files are the same. Warning: uses lazy IO.
-- sameFile       :: FilePath -> FilePath -> IO Bool
-- sameFile fa fb = do a <- LB.readFile fa
--                     b <- LB.readFile fb
--                     pure $ a == b


-- --------------------------------------------------------------------------------

-- -- | Generates a set of n elements (all being different), using the
-- -- given generator.
-- setOf    :: Ord a => Int -> Gen a -> Gen (Set.Set a)
-- setOf n g = buildSet mempty <$> do sz <- getSize
--                                    infiniteListOf (resize (max sz n) g)
--   where
--     buildSet s (x:xs) | length s == n = s
--                       | otherwise     = let s' = Set.insert x s in buildSet s' xs
--     buildSet _  _                     = error "setOf: absurd"



--------------------------------------------------------------------------------

-- | An input with at least three distinct values
newtype Input3 plane = Input3 (NESet.NESet plane)
                          deriving (Show,Eq)

instance (Arbitrary plane, Ord plane) => Arbitrary (Input3 plane) where
  arbitrary = do s  <- arbitrary
                 s' <- grow s
                 pure $ Input3 s'
    where
      grow s
        | length s >= 3 = pure s
        | otherwise     = do x <- arbitrary `suchThat` (\y -> y `NESet.notMember` s)
                             grow $ NESet.insert x s
  shrink (Input3 s) = [ Input3 s'
                      | s' <- shrinkSt ((>= 3) . length) s
                      ]
    where
      shrinkSt p = filter p . shrink


--------------------------------------------------------------------------------

-- | Assign distinct colors in some reasonable manner
assignColors :: NESet.NESet (Point 2 r) -> NESet.NESet (Point 2 r :+ IpeColor r)
assignColors = snd . mapAccumLStrictlyMonotonic f (cycle basicNamedColors)
  where
    f cs p = case cs of
      (c:colors') -> (colors', p :+ c)
      _           -> error "absurd"

-- | mapAccumL; assuming the function is stritly monotonic; thus producing no duplicates.
mapAccumLStrictlyMonotonic      :: (s -> a -> (s, b)) -> s -> NESet.NESet a -> (s, NESet.NESet b)
mapAccumLStrictlyMonotonic f s0 = fmap NESet.fromDistinctAscList
                                . List.mapAccumL f s0 . NESet.toList



--------------------------------------------------------------------------------

data PointInTriangle = PointInTriangle (Triangle (Point 2 R)) (Point 2 R)
                     deriving (Show,Eq,Generic)

instance Arbitrary PointInTriangle where
  arbitrary = do t <- arbitrary
                 v <- arbitrary `suchThat` all (> 0) -- barrycentric coordinates of a point
                 pure $ PointInTriangle t (barrycentric t v)


-- | Given a triangle and a vector of coefficients, use it to produce a point inside
-- the triangle
barrycentric :: Triangle (Point 2 R) -> Vector 3 R -> Point 2 R
barrycentric (Triangle (Point a) (Point b) (Point c)) (normalize -> Vector3 x y z) =
    Point $ (x *^ a) ^+^ (y *^ b) ^+^ (z *^ c)

-- | Normalize the vector w.r.t the sum of the coefficients.
normalize   :: Vector 3 R -> Vector 3 R
normalize v = let s = sum v in (/s) <$> v


--------------------------------------------------------------------------------

-- | I don't think I really want this one; but just for debugging purposes it seems ok
type instance NumType (a,b) = NumType b
type instance NumType (a,b,c) = NumType c
type instance NumType (a,b,c,d) = NumType d
-- more numtype instances just for debugging/testing purposes
type instance NumType (NESet.NESet a) = NumType a
type instance NumType (MonoidalMap.MonoidalMap k a) = NumType a


instance ( IsDrawable (Ipe r) a
         , IsDrawable (Ipe r) b
         , NumType a ~ r, NumType b ~ r
         , HasCommonAttributes (AttrOf (Ipe r) a) r Maybe
         , HasCommonAttributes (AttrOf (Ipe r) b) r Maybe
         ) => IsDrawable (Ipe r) (a,b) where
  type AttrOf (Ipe r) (a,b) = CommonAttributes r Maybe
  draw ats (a,b) = draw @(Ipe r) [ commonAttributes %~ apply ats ] a
                <> draw @(Ipe r) [ commonAttributes %~ apply ats ] b


instance ( IsDrawable (Ipe r) a
         , IsDrawable (Ipe r) b
         , IsDrawable (Ipe r) c
         , NumType a ~ r, NumType b ~ r, NumType c ~ r
         , HasCommonAttributes (AttrOf (Ipe r) a) r Maybe
         , HasCommonAttributes (AttrOf (Ipe r) b) r Maybe
         , HasCommonAttributes (AttrOf (Ipe r) c) r Maybe
         ) => IsDrawable (Ipe r) (a,b,c) where
  type AttrOf (Ipe r) (a,b,c) = CommonAttributes r Maybe
  draw ats (a,b,c) = draw @(Ipe r) [ commonAttributes %~ apply ats ] a
                  <> draw @(Ipe r) [ commonAttributes %~ apply ats ] b
                  <> draw @(Ipe r) [ commonAttributes %~ apply ats ] c

instance ( IsDrawable (Ipe r) a
         , IsDrawable (Ipe r) b
         , IsDrawable (Ipe r) c
         , IsDrawable (Ipe r) d
         , NumType a ~ r, NumType b ~ r, NumType c ~ r, NumType c ~ r
         , HasCommonAttributes (AttrOf (Ipe r) a) r Maybe
         , HasCommonAttributes (AttrOf (Ipe r) b) r Maybe
         , HasCommonAttributes (AttrOf (Ipe r) c) r Maybe
         , HasCommonAttributes (AttrOf (Ipe r) d) r Maybe
         ) => IsDrawable (Ipe r) (a,b,c,d) where
  type AttrOf (Ipe r) (a,b,c,d) = CommonAttributes r Maybe
  draw ats (a,b,c,d) = mconcat
      [ draw @(Ipe r) [ commonAttributes %~ apply ats ] a
      , draw @(Ipe r) [ commonAttributes %~ apply ats ] b
      , draw @(Ipe r) [ commonAttributes %~ apply ats ] c
      , draw @(Ipe r) [ commonAttributes %~ apply ats ] d
      ]

instance IsDrawable backend g => IsDrawable backend (NESet.NESet g) where
  type AttrOf backend (NESet.NESet g) = AttrOf backend g
  draw ats = foldMap (draw @backend ats)

instance IsDrawable backend g => IsDrawable backend (MonoidalMap.MonoidalMap k g) where
  -- ^ Draws the values; not the keys
  type AttrOf backend (MonoidalMap.MonoidalMap k g) = AttrOf backend g
  draw ats = foldMap (draw @backend ats)

instance IsDrawable (Ipe R) (Point 2 R :+ IpeColor R) where
  type AttrOf (Ipe R) (Point 2 R :+ IpeColor R)  = AttrOf (Ipe R) (Point 2 R)
  draw ats (p :+ c) = draw @(Ipe R) ((stroke ?~ c) : ats) p

-- | Helper function to apply attributes
apply       :: [at -> at] -> at -> at
apply ats a = foldl' (flip ($)) a ats



instance ( Ord r, Fractional r
         , IsDrawable (Ipe r) line, AttrOf (Ipe r) line ~ PathAttributes r
         , HalfSpaceF line `IsIntersectableWith` Rectangle (Point 2 r)
         ) => IsDrawable (Ipe r) (HalfSpaceF line :+ Maybe (IpeColor r)) where
  type AttrOf (Ipe r) (HalfSpaceF line :+ Maybe (IpeColor r)) = PathAttributes r
  draw ats (h :+ c) = case h `intersect` defaultBox of
      Nothing -> []
      Just is -> case is of
        ActualPolygon interior -> mconcat [ draw @(Ipe r)
                                                 ( [ fill .~ c ]
                                                   <> ats <>
                                                   [ opacity ?~ Named "20%"
                                                   , stroke  .~ Nothing
                                                   ]
                                                 ) interior
                                          , boundary
                                          ]
        _                      -> boundary
    where
      boundary = draw @(Ipe r) ([stroke .~ c] <> ats <> [fill .~ Nothing]) (h^.boundingHyperPlane)

instance ( Ord r, Fractional r
         , IsDrawable (Ipe r) line, AttrOf (Ipe r) line ~ PathAttributes r
         , HalfSpaceF line `IsIntersectableWith` Rectangle (Point 2 r)
         ) => IsDrawable (Ipe r) (HalfSpaceF line) where
  type AttrOf (Ipe r) (HalfSpaceF line) = PathAttributes r
  draw ats h = case h `intersect` defaultBox of
      Nothing -> []
      Just is -> case is of
        ActualPolygon interior -> mconcat [ draw @(Ipe r)
                                                 (ats <>
                                                   [ opacity ?~ Named "20%"
                                                   , stroke  .~ Nothing
                                                   ]
                                                 ) interior
                                          , boundary
                                          ]
        _                      -> boundary
    where
      boundary = draw @(Ipe r) (ats <> [fill .~ Nothing]) (h^.boundingHyperPlane)

instance ( Ord r, Fractional r
         ) => IsDrawable (Ipe r) (LinePV 2 r) where
  type AttrOf (Ipe r) (LinePV 2 r) = PathAttributes r
  draw ats l = case l `intersect` defaultBox of
                 Nothing -> []
                 Just is -> case is of
                   Line_x_Box_LineSegment seg -> draw @(Ipe r) ats seg
                   _                          -> [] -- don't draw singleton points

instance ( Ord r, Fractional r
         ) => IsDrawable (Ipe r) (VerticalOrLineEQ r) where
  type AttrOf (Ipe r) (VerticalOrLineEQ r) = PathAttributes r
  draw ats = draw @(Ipe r) ats . convert
    where
      convert = \case
        VerticalLineThrough x -> LinePV (Point2 x 0) (Vector2 0 1)
        NonVertical l         -> fromLineEQ l

module Test.Util where

-- import           Control.Exception.Base (bracket)
-- import           Control.Monad (when)
-- import qualified Data.ByteString as B
-- import qualified Data.ByteString.Lazy as LB
-- import           Data.Function (on)
-- import           Data.Functor.Classes
import qualified Data.List as List
-- import           Data.Proxy
-- import           Data.Ratio
-- import qualified Data.Set as Set
-- import           Data.Singletons (Apply)
-- import           Data.Vinyl
-- import           HGeometry.Ext
-- import           Ipe
-- import           System.Directory (getTemporaryDirectory, removeFile)
-- import           System.FilePath (takeExtension)
-- import           System.IO (Handle, hClose, openTempFile)
-- import           Test.Hspec
-- import           Test.QuickCheck

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

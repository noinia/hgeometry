module Main where

import           Control.Lens
import           Data.Fixed
import qualified Data.Foldable as F
import qualified Data.List as List
import           Data.List.NonEmpty (NonEmpty(..))
import qualified Data.List.NonEmpty as NonEmpty
import           HGeometry.ConvexHull.GrahamScan
import           HGeometry.Ext
import           HGeometry.Point
import           HGeometry.Polygon.Class

{-

Credits: http://2012.bapc.eu/

Problem Statement
=================

The quickly shooting, maiden saving, gunslinging cowboy Luke finds himself in
the den of his archenemy: the Dalton gang. He is trying to escape but is in
constant danger of being shot. Fortunately his excellent marksmanship and quick
reflexes give him the upper hand in a firefight: the gang members are all too
scared to move, let alone draw their guns. That is, as long as Luke can see
them. If he cannot see one of the thugs, then that Dalton will immediately fire
upon Luke and kill the cowboy without fear of retaliation. Luke’s amazing
eyesight allows him to cover a field of view of 180 degrees all the time. While
doing so he can move around freely, even walking backward if necessary.  Luke’s
goal is to walk to the escape hatch in the den while turning in such a way that
he will not be shot. He does not want to shoot any of the Daltons because that
will surely result in a big fire fight. The Daltons all have varying heights,
but you may assume that all the people and the hatch are of infinitesimal size.

Input
-----

On the first line one positive number: the number of test cases, at most
100. After that per test case:

* one line with two space-separated integers xL and yL: Luke’s starting position.
* one line with two space-separated integers xE and yE : the position of the escape hatch.
* one line with an integer n (1 <= n <= 1 000): the number of Dalton gang members.
* n lines with two space-separated integers xi and yi: the position of the i-th Dalton.

All x and y are in the range −10000 <= x,y <= 10000. Luke, the escape hatch and
all Daltons all have distinct positions.

Output
-----

Per test case:

* one line with the length of the shortest path Luke can take to the escape
hatch without dying, rounded to three decimal places, or “IMPOSSIBLE” if no
such path exists.  The test cases are such that an absolute error of at most
10^−6 in the final answer does not influence the result of the rounding.


Solution
========

Compute the convex hull of Luke, the hatch, and the daltons. If luke and the
hatch are on the convex hull, Luke can safely reach it. The length of the
shortest path is the lenght of walking along the convex hull (in one of the two
directions). If luke or the hatch is not on the Convex Hull, escaping is
impossible.

Running time: O(n log n)

-}

data Answer = Possible Double | Impossible deriving (Eq,Ord)

instance Show Answer where
  show Impossible   = "IMPOSSIBLE"
  show (Possible l) = showFixed False  . roundToMili $ l
    where
      roundToMili :: Real a => a -> Milli
      roundToMili = realToFrac


data Kind = Luke | Hatch | Dalton deriving (Show,Eq)

data Input = Input { _luke    :: Point 2 Int
                   , _hatch   :: Point 2 Int
                   , _daltons :: [Point 2 Int]
                   } deriving (Show,Eq)

-- TODO: THis only works if there are no colinear points. I.e. if Luke or the
-- hatch lie on the hull, but in the interior of some edge, they are not
-- contained in the hull.
escape                :: Input -> Answer
escape (Input l h ds) = case findIndexOf outerBoundary (\v -> v^.extra == Luke) ch of
    Nothing      -> Impossible
    Just lukeIdx -> distanceToHatch (toNonEmptyOf (cwOuterBoundaryFrom lukeIdx) ch)
                   `min`
                    distanceToHatch (toNonEmptyOf (ccwOuterBoundaryFrom lukeIdx) ch)
  where
    ch = convexHull $ (l :+ Luke) :| (h :+ Hatch) : map (:+ Dalton) ds

toHatch    :: NonEmpty (p :+ Kind) -> Maybe (NonEmpty (p :+ Kind))
toHatch xs = let (ys,rest) = NonEmpty.break (\p -> p^.extra == Hatch) xs
             in case rest of
               []    -> Nothing
               (h:_) -> Just . NonEmpty.fromList $ ys ++ [h]

distanceAlong     :: NonEmpty (Point 2 Int :+ k) -> Double
distanceAlong xs' = sum $ zipWith euclideanDist (F.toList xs) (NonEmpty.tail xs)
  where
    xs :: NonEmpty (Point 2 Double)
    xs = over coordinates fromIntegral . (^.core) <$> xs'

-- Distance from luke, at the head of the list, to the hatch while walking
-- along the points in the list.
distanceToHatch :: NonEmpty (Point 2 Int :+ Kind) -> Answer
distanceToHatch = maybe Impossible (Possible . distanceAlong) . toHatch

readPoint   :: String -> Point 2 Int
readPoint s = case map read . words $ s of
                [x,y] -> Point2 x y
                _     -> error "readPoint: wrong number of args"

readInput                 :: [String] -> [Input]
readInput []              = []
readInput (ls:hs:ns:rest) = let n            = read ns
                                (daltons,ys) = List.splitAt n rest
                            in Input (readPoint ls)
                                     (readPoint hs)
                                     (map readPoint daltons)
                               : readInput ys
readInput _                = error "readInput: wrong number of args"

gunslinger :: String -> String
gunslinger = unlines . map (show . escape) . readInput . drop 1 . lines

main :: IO ()
main = interact gunslinger

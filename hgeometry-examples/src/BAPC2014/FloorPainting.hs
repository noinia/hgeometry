module BAPC2014.FloorPainting where

-- The museum of Bizarre Argentinean Pocket Calculators (BAPC) has found a great
-- painter to make a nice floor painting for in the museum. All walls in the
-- museum are straight, and any two adjacent walls meet at a right angle. All
-- non-adjacent walls are pairwise disjoint. Furthermore, the room in which the
-- painting is supposed to come has no pillars. Hence, it is a rectilinear simple
-- polygon.
--
-- The design for the painting is square. The museum wants the painting to be
-- as large as possible.  Furthermore, it should be placed such that the edges
-- of the square are parallel to the walls. Find the maximum possible width for
-- the painting.
--
-- Input:
-- On the first line one positive number: the number of test cases, at
-- most 100. After that per test case:
-- * one line with a single integer $n$ ($4 \leq n \leq 1\,000$): the number
--   of corners in the room.
-- * $n$ lines, each with two space-separated integers $x$ and $y$
--   ($0 \leq x,y \leq 100\,000$): the coordinates of each corner of the room.
--
-- The corners are given in clockwise order.
--
-- Output:
-- Per test case:
--
--  one line with a single integer: the maximum width of a
--   square painting that fits on the floor of the museum.



--
--
-- main idea: Observe that there is an optimal square incident to three sides.
--
-- assume that these sides are left, top, right
--
-- Sweep a vertical line downward. Maintain the lowest segment hit at any point
--
-- just before we insert a new segment: sweep left to right, over the endpoints
-- in the tree, and compute th







-- readInput           :: [String] -> [RectPolygon]
-- readInput []        = []
-- readInput (ns:rest) = let n       = read ns
--                           (xs,ys) = L.splitAt n rest
--                       in readPolygon xs : readInput ys

-- readPolygon    :: [String] -> RectPolygon
-- readPolygon ss = RectPolygon (sorted $ lefts segs) (sorted $ rights segs)
--   where
--     vertices = map readVertex ss
--     segs     = zipWith toSegment vertices (tail vertices ++ vertices)
--     sorted :: [Segment o] -> Sorted Array (Segment o)
--     sorted = Sorted . listArray . toList . L.sortBy (comparing elevation)

--     sortByElev = S.unstableSortBy (compare `on` elevation)


-- readVertex   :: String -> Point
-- readVertex s = let [x,y] = map read . words $ s
--                in Point (x,y)

-- toSegment :: Point -> Point -> Either (Segment Horizontal) (Segment Vertical)
-- toSegment (Point (px,py)) (Point (qx,qy))
--   | py == qy  = Left  $ Segment py px qx
--   | otherwise = Right $ Segment px py qy


-- main :: IO ()
-- main = interact $
--          unlines . map (show . maxSquareWidth) . readInput . tail . lines


-- mainF = readFile "testdata.in"
--       >>= putStr . unlines . map (show . maxSquareWidth) . readInput . tail . lines

module Algorithms.Geometry.ConvexHull.Debug where

import           Algorithms.DivideAndConquer
import           Algorithms.Geometry.ConvexHull.Helpers
import           Algorithms.Geometry.ConvexHull.Types
import           Control.Applicative ((<|>))
import           Control.Lens ((^.))
import           Control.Monad (replicateM)
import           Data.Ext
import           Data.Geometry.LineSegment
import           Data.Geometry.Point
import           Data.Geometry.PolyLine
import           Data.IndexedDoublyLinkedList
import           Data.List.NonEmpty (NonEmpty(..))
import qualified Data.List.NonEmpty as NonEmpty
import           Data.Maybe
import           Data.Ord (comparing)
import           Data.Semigroup (sconcat)
import           Data.UnBounded
import qualified Data.Vector as V

import           Data.Geometry.Ipe
import           Data.Geometry.Ipe.Color


import           Control.Monad.Reader.Class
import           System.IO.Unsafe
import           System.Random


import           Debug.Trace


debugHull :: Num r1 => MergeStatus r2 -> MergeStatus r3 -> DLListMonad s (Point 3 r1) ([Char], Index, [Char], Index, [Char], Index, [Char], Index)
debugHull l r = (\a b c d -> ( "MERGING ", hd l, "-",lst l, " WITH "
                             , hd r, "-", lst r
                                          -- , a
                                          -- , b
                                   -- , c
                                   -- , d
                                   -- , events l
                                   -- , events r
                                   )
                      )
                      <$> (toListFrom $ hd l) <*> (toListFrom $ hd r)
                      <*> listHull l <*> listHull r
  where

      listHull s = do xs <- NonEmpty.toList <$> toListFrom (hd s)
                      mapM (atTime t) xs
         where
           t = (-10000000)



renderMovieIO :: (Show r, Fractional r, Ord r, IpeWriteText r) => [Char] -> MergeStatus r
                    -> [r :+ NonEmpty (Existing Action)] -> DLListMonad s (Point 3 r) [Char]
renderMovieIO s ms esIn = do pgs <- renderMovie ms ((^.core) <$> esIn)
                             pure $ unsafePerformIO $
                               do fp <- (\s1 -> "/tmp/out" <> s <> "_" <> s1 <> ".ipe") <$> randomS
                                  writeIpeFile fp . IpeFile Nothing [basicIpeStyle] $ pgs
                                  pure fp
  where
    randomS :: IO String
    randomS = replicateM 10 $ randomRIO ('a','z')



drawDebug             :: (IpeWriteText r, Ord r, Fractional r)
                      => String
                      -> Bottom r
                      -> NonEmpty Index
                      -> Bridge
                      -> V.Vector (Point 3 r)
                      -> FilePath
drawDebug s t' h blr pts = unsafePerformIO $
                       do fp <- (\s1 -> "/tmp/out" <> s <> "_" <> s1 <> ".ipe") <$> randomS
                          drawAllAt fp t h blr pts
                          pure fp
  where
    randomS :: IO String
    randomS = replicateM 10 $ randomRIO ('a','z')

    t = case t' of
          Bottom   -> -1000
          ValB t'' -> t''



drawAllAt :: (IpeWriteText r, Fractional r, Ord r) => FilePath -> r -> NonEmpty Index -> Bridge -> V.Vector (Point 3 r) -> IO ()
drawAllAt fp t h blr pts = writeIpeFile fp . IpeFile Nothing [basicIpeStyle] $ draw t h blr pts


draw                       :: (Fractional r, Ord r, IpeWriteText r)
                           => r
                           -> NonEmpty Index
                           -> Bridge
                           -> V.Vector (Point 3 r) -> NonEmpty (IpePage r)
draw t h blr pts = fmap (\t' -> drawAt t' h blr pts)
                 $ NonEmpty.fromList [t-eps,t,t+eps]
  where
    eps = (1/10)


-- draw          :: (Fractional r, Ord r, IpeWriteText r)
--               => MergeStatus r -> Bridge -> V.Vector (Point 3 r) -> NonEmpty (IpePage r)
-- draw ms b pts = fmap (\t -> drawAt t ms b pts) times
--   where
--     times = NonEmpty.fromList . (initT :)
--           . concatMap (\e -> let t = eventTime e in [t-eps,t,t+eps]) $ events ms
--     eps = (1/1000)

--     initT = (-10000000)
--     -- we should recompute the bridge I guess

drawAt                       :: (Num r, Ord r, IpeWriteText r)
                             => r
                             -> NonEmpty Index
                             -> Bridge
                             -> V.Vector (Point 3 r) -> IpePage r
drawAt t h (Bridge l r) pts = fromContent $
   pts' <> [drawHull h pts2, drawBridge l r pts2, time]
  where
    pts2 = fmap (atTime' t) pts
    pts' = V.toList . V.imap (\i p -> iO $ labelled id defIO (p :+ i)) $ pts2
    time = iO $ ipeLabel ((fromJust' $ ipeWriteText t) :+ Point2 (-50) (-50))

    fromJust' = fromMaybe (error "drawAt?")

    -- lh = NonEmpty.head lh' NonEmpty.<| lh'
    -- rh = NonEmpty.head rh' NonEmpty.<| rh'



drawHull       :: NonEmpty Index -> V.Vector (Point 2 r) -> IpeObject r
drawHull h pts = case h of
    (i :| []) -> iO' (pts V.! i)
    _         -> iO . ipePolyLine . fromPointsUnsafe $ [ (pts V.! i) :+ () | i <- NonEmpty.toList h ]

drawBridge         :: Index -> Index -> V.Vector (Point 2 r) -> IpeObject r
drawBridge l r pts | l == r    = iO'' (pts V.! l) (attr SStroke red)
                   | otherwise = let s = ClosedLineSegment (ext $ pts V.! l) (ext $ pts V.! r)
                                 in iO $ ipeLineSegment s ! attr SStroke red

getPoints :: DLListMonad s x (V.Vector x)
getPoints = asks values









mergeEvents'       :: (Ord r, Show r)
                  => [Event r] -> [r] -> [r :+ Maybe (NonEmpty Action)]
mergeEvents' ls rs = map combine . groupOn (^.core)
                   $ mergeSortedListsBy (comparing (^.core)) (wrap <$> ls) ((:+ Nothing) <$> rs)
  where
    wrap (t :+ acts) = t :+ Just acts
    combine xs@((t :+ _):|_) =
      t :+ (NonEmpty.nonEmpty . concatMap NonEmpty.toList
             . mapMaybe (^.extra) . NonEmpty.toList $ xs)


-- | Reports all the edges on the CH
renderMovie    :: (Show r, Fractional r, Ord r, IpeWriteText r)
               => MergeStatus r -> [r]
               -> HullM s r (NonEmpty (IpePage r))
-- renderMovie ms | traceShow ("output: ", events ms) False = undefined
renderMovie ms es = do h0 <- toListFrom $ traceShow ("events: ", show (events ms)) $ hd ms
                       res <- traceShow ("h0",h0) renderMovie'
                       writeList h0 -- restore the original list and state
                       pure $ res
  where
    renderMovie' = combine <$> mapM handle (mergeEvents' (events ms) es)
    combine xs = case NonEmpty.nonEmpty xs of
      Nothing  -> fromContent [] :| []
      Just pgs -> sconcat pgs

    handle (t :+ macts) = case macts of
      Nothing   -> do pts <- getPoints
                      hAfter <- toListContains (hd ms)
                      let pages' = drawMovie t ((hd ms):|[]) hAfter pts
                      pure pages'
      Just acts -> do pts <- getPoints
                      (_,v0) <- dump
                      i <- fromEvent . NonEmpty.head $ acts
                      hBefore <- toListContains i
                      j <- applyEvent (t :+ acts)
                      (_,v) <- dump
                      hAfter <- toListContains j
                      let pages' = drawMovie t hBefore hAfter pts
                      pure pages'

fromEvent :: Action -> DLListMonad s b Index
fromEvent act = case act of
    InsertAfter i _  -> pure i
    InsertBefore i _ -> pure i
    Delete j         -> (\a b -> fromJust' $ a <|> b) <$> getPrev j <*> getNext j
  where
    fromJust' = fromMaybe (error $ "no events?? " <> show act)

drawMovie             :: (Fractional r, Ord r, IpeWriteText r)
                      => r -> NonEmpty Index -> NonEmpty Index -> V.Vector (Point 3 r)
                      -> NonEmpty (IpePage r)
drawMovie t h0 h1 pts = NonEmpty.fromList
    [ --fromContent $ d pts0 <> [ drawHull h0 pts0, time $ t - eps  ]
     fromContent $ d pts2 <> [ drawHull h1 pts2, time $ t        ]
--    , fromContent $ d pts1 <> [ drawHull h1 pts1, time $ t + eps  ]
    ]
  where
    pts0 = fmap (atTime' $ t - eps ) $ pts
    pts1 = fmap (atTime' $ t + eps ) $ pts
    pts2 = fmap (atTime' t) $ pts

    time x = iO $ ipeLabel ((fromJust $ ipeWriteText x) :+ Point2 (-50) (-50))

    eps = (1/10)


    d pts' = V.toList
           -- . V.slice s (1 + tt - s)
           . V.map (^.extra)
           . V.filter (\(x :+ _) -> x `inRange` xRange)
           . V.imap (\i p -> (p^.xCoord) :+ (iO $ labelled id defIO (p :+ i)))
           $ pts'

    hpts = [ (pts V.! i)^.xCoord | i <- NonEmpty.toList h1]
    -- hpts = map (^.xCoord) $ V.toList pts

    xRange = ClosedRange (minimum hpts) (maximum hpts)
    -- (s,tt) = (NonEmpty.head h0, NonEmpty.last h0)




    -- atts =

{-# LANGUAGE TemplateHaskell #-}
module Algorithms.Geometry.ConvexHull.KineticDivideAncConquer where

import           Algorithms.DivideAndConquer
import           Control.Lens ((^.), bimap)
import           Control.Monad.Reader
import           Control.Monad.ST
import           Control.Monad.Writer (Writer)
import           Data.Ord (comparing)
-- import qualified Data.DoublyLinkedList as DLList
-- import           Data.DoublyLinkedList (DLList)
import           Data.Maybe
import           Data.Ext
import           Data.Geometry.Point
import qualified Data.List as List
import qualified Data.List.NonEmpty as NonEmpty
import           Data.List.NonEmpty (NonEmpty(..), (<|))
import           Data.Util
import qualified Data.Vector as V
import qualified Data.Vector.Mutable as MV

--------------------------------------------------------------------------------0

-- type ConvexHull p r = [Two (Point 3 r :+ p)]


type ConvexHull d p r = V.Vector [Vertex] -- [Edge]--[Two (Point 3 r :+ p)]


-- for the time being, return a list of edges

data Env p r = Env { _pts    :: V.Vector (Point 3 r)
                   , _extras :: V.Vector p
                   }


convexHull :: (Ord r, Fractional r) => [Point 3 r :+ p] -> ConvexHull 3 p r
convexHull = maybe mempty convexHull' . NonEmpty.nonEmpty

convexHull'      :: (Ord r, Fractional r) => NonEmpty (Point 3 r :+ p) -> ConvexHull 3 p r
convexHull' pts' = output
                 . flip runReader pts
                 . runMerge
                 . divideAndConquer1 mkM
                 $ NonEmpty.fromList [0..(n-1)]
  where
    n = V.length pts
    fromNonEmpty = V.fromList . NonEmpty.toList
    (pts,exts) = bimap V.fromList V.fromList . unExt . NonEmpty.sortWith (^.core.xCoord) $ pts'
    unExt = foldr (\(p :+ e) (ps,es) -> (p:ps,e:es)) ([],[])


-- produce adjacencylist from the merge status
output :: MergeStatus r -> V.Vector [Vertex]
output = undefined


-- writeEdge :: Vertex -> Vertex ->





type Vertex = Int
type Edge = SP Int Int






data Event r = Event { eventTime :: !r
                     , eventKind :: !EventKind
                     } deriving (Show,Eq)

data EventKind = InsertAfter  !Vertex !Vertex -- ^ old then new
               | InsertBefore !Vertex !Vertex
               | Delete !Vertex
               deriving (Show,Eq,Ord)



data MergeStatus s r = MergeStatus { initialHull :: LList s
                                   , events      :: [Event r]
                                   }

type Merger r = Reader (V.Vector (Point 3 r))

newtype Merge r = M { runMerge :: Merger r (MergeStatus r)
                    }

instance (Ord r, Fractional r) => Semigroup (Merge r) where
  (M l) <> (M r) = M $ do MergeStatus lh le <- l
                          MergeStatus rh re <- r
                          STR h u v <- joinHullsAt (t le re) lh rh
                          runSimulation h le re
                          pure $ MergeStatus h es
    where
      t le re = head' le `min` head' re
      head' = \case
        []    -> 0 -- if there are no events, pick any time we like
        (x:_) -> eventTime x




joinHuls ls rs = do



mkM   :: Vertex -> Merge r
mkM v = M . pure $ MergeStatus (v :| []) []





askPoint   :: Vertex -> Merger r (Point 3 r)
askPoint i = asks (\v -> v V.! i)

atTime     :: Num r => r -> Vertex -> Merger r (Point 2 r)
atTime t i = (\(Point3 x y z) -> Point2 x (z - t*y)) <$> askPoint i


--------------------------------------------------------------------------------

-- | Computes the new lower hull at time t. Returns the new hull, as well as the
-- two bridge vertices
joinHullsAt         :: (Ord r, Num r) => r -> NonEmpty Vertex -> NonEmpty Vertex
                    -> Merger r (STR (NonEmpty Vertex) Vertex Vertex)
joinHullsAt t lh@(l0:| _) rh = do pl       <- atTime t l0
                                  SP rh' r <- walkAt t (leftOf pl) rh
                                  pr       <- atTime t r
                                  SP lh' l <- walkAt t (rightOf pr) lh
                                  pure $ STR (NonEmpty.reverse lh' <> rh') l r
  where
    rightOf r s l' = ccw l' s r == CCW
    leftOf  l' s r = ccw r s l' == CW

-- Helper to create the new hull
walkAt            :: Num r => r
                  -> (Point 2 r -> Point 2 r -> Bool) -- ^ wether or not to keep the first arg
                  -> NonEmpty Vertex
                  -> Merger r (SP (NonEmpty Vertex) Vertex)
walkAt t keep ls0 = go ls0
  where
    singleton x = x :| []
    go (l:|ls') = case NonEmpty.nonEmpty ls' of
                    Nothing          -> pure $ SP (singleton l) l
                    Just ls@(s :| _) -> do pl <- atTime t l
                                           ps <- atTime t s
                                           if keep ps pl
                                             then (\(SP xs x) -> SP (l <| xs) x) <$> go ls
                                             else pure $ SP (singleton l) l


-- -- | computes the list that we traversed and a copy of its last element
-- walkAt             :: (Ord r, Num r) => r -> NonEmpty Vertex -> Point 2 r
--                    -> Merger r (SP (NonEmpty Vertex) Vertex)
-- walkAt t keep ls0 r = go ls0
--   where
--     rightOf s l r = ccw l s r == CCW
--     singleton x = x :| []

--     go (l:|ls') = case NonEmpty.nonEmpty ls' of
--                     Nothing          -> pure $ SP (singleton l) l
--                     Just ls@(s :| _) -> do pl <- atTime t l
--                                            ps <- atTime t s
--                                            if rightOf ps pl r
--                                              then (\(SP xs x) -> SP (l <| xs) x) <$> go ls
--                                              else pure $ SP (singleton l) l

--------------------------------------------------------------------------------



runSimulation         :: (Ord r, Fractional r)
                      => NonEmpty Vertex -> [Event r] -> [Event r] -> Merger r [Event r]
runSimulation h le re = undefined
-- FIXME: I think we will need to simulate the old events as well, so that means
-- we need fast O(1) access into the old list


-- | First event involving the bridge, if any. Also returns the new bridge pair
bridgeTime         :: (Ord r, Fractional r)
                   => r -> Vertex -> Vertex
                   -> Simulation s r (Maybe (Event r, Two Vertex))
bridgeTime now l r = do Two a b <- neighbours l
                        Two u v <- neighbours r
                        [pa,pl,pb,pu,pr,pv] <- mapM getP [a,l,b,u,r,v]

                        let me    = minimumOn (\(t',_,_) -> t')
                                      [ e | e@(t',_,_) <- cands, t' >= now]
                            cands = [ (nextTime pa pl pr, Two a r, Delete l)
                                    , (nextTime pl pb pr, Two b r, InsertAfter  l b)
                                    , (nextTime pl pu pr, Two l u, InsertBefore u r)
                                    , (nextTime pl pr pv, Two l v, Delete r)
                                    ]
                        pure $ (\(t,br,e) -> (Event t e, br)) <$> me

minimumOn   :: Ord b => (a -> b) -> [a] -> Maybe a
minimumOn f = \case
    [] -> Nothing
    xs -> Just $ List.minimumBy (comparing f) xs



-- compute the time at which r becomes colinear with the line throuh p and l.
--
nextTime  :: (Ord r, Fractional r) => Point 3 r -> Point 3 r -> Point 3 r -> r
nextTime (Point3 px py pz) (Point3 lx ly lz) (Point3 rx ry rz) = t
  where
    ux = lx - px
    vx = rx - px

    a = (-1)*ux*(rz - pz)  - vx*(lz - pz)
    b =      ux*(py - ry)  - vx*(py - ly)
    t = a / b
    -- by unfolding the def of ccw



-- type EventQueue = Map t

data SimEnv s r = SimEnv { pts   :: V.Vector (Point 3 r)
                         , llist :: LList s
                         }

type Simulation s r = ReaderT (SimEnv s r) (ST s)

getP   :: Vertex -> Simulation s r (Point 3 r)
getP i = (\v -> v V.! i) <$> asks pts

getPT     :: Num r => r -> Vertex -> Simulation s r (Point 2 r)
getPT t i = (\(Point3 x y z) -> Point2 x (z - t*y)) <$> getP i


neighbours   :: Vertex -> Simulation s r (Two Vertex)
neighbours i = do v        <- asks llist
                  Cell p s <- fromJust <$> (lift $ MV.read v i)
                  pure $ Two p s


--------------------------------------------------------------------------------

data Cell = Cell !Vertex
                 !Vertex deriving (Show,Eq)


type LList s = MV.MVector s (Maybe Cell)

toLListN             :: Int -> NonEmpty (Vertex) -> ST s (LList s)
toLListN n l@(x:|xs) = do v <- MV.replicate n Nothing
                          forM_ ys $ \(p,y,s) ->
                            MV.write v y $ Just (Cell p s)
                          pure v
  where
    ys = zip3' (x <| l) l (NonEmpty.fromList $ xs ++ [NonEmpty.last l])

    zip3' (a :| as) (b:|bs) (c:|cs) = (a,b,c) :| List.zip3 as bs cs


-- | If this cell has a next cell, return it
getNext   :: Vertex -> Simulation s r (Maybe Vertex)
getNext i = do v <- asks llist
               ms <- lift $ MV.read v i
               pure $ ms >>= \(Cell _ s) -> if s /= i then Just s else Nothing

getPrev   :: Vertex -> Simulation s r (Maybe Vertex)
getPrev i = do v <- asks llist
               mp <- lift $ MV.read v i
               pure $ mp >>= \(Cell p _) -> if p /= i then Just p else Nothing

-- | 'insertAfter c y' inserts y after c in the linked list. Returns the
-- new cell containing y.
insertAfter     :: Vertex -> Vertex -> Simulation s r ()
insertAfter i j = do v <- asks llist
                     Cell p s <- fromJust <$> (lift $ MV.read v i)
                     let s' = if i /= s then s else j
                     lift $ MV.write v j $ Just (Cell i s')
                     lift $ MV.write v i $ Just (Cell p j)


data DeletionResult = Empty        -- ^ the list is now empty
                    | Last !Vertex -- ^ pointer to the last element in the list
                    | Succ !Vertex -- ^ pointer to the
                      -- element that was the
                                     -- the successor of the
                                     -- element that we
                                     -- removed.

delete   :: Vertex -> Simulation s r DeletionResult
delete i = do v  <- asks llist
              mp <- getPrev i
              ms <- getNext i
              lift $ MV.write v i Nothing
              case (mp,ms) of
                (Nothing,Nothing) -> pure Empty
                (Nothing,Just s)  -> do lift $ modify v s $ setPrev s
                                        pure $ Succ s
                (Just p, Nothing) -> do lift $ modify v p $ setNext p
                                        pure $ Last p
                (Just p, Just s)  -> do lift $ modify v s $ setPrev p
                                        lift $ modify v p $ setNext s
                                        pure $ Succ s
  where
    setPrev p = fmap (\(Cell _ s) -> Cell p s)
    setNext s = fmap (\(Cell p _) -> Cell p s)

    modify v i f = MV.modify v f i



--------------------------------------------------------------------------------







runTimed'             :: t -> Timed s t r -> r
runTimed' t (Timed f) = f t

-- | Runs a Timed computation at the given time.
runTimed     :: forall t r. t -> (forall s. Reifies s t => Timed s t r) -> r
runTimed t k = reify t $ \px -> runTimed' (reflect px) (f px)
  where
    f   :: Reifies s t => Proxy s -> Timed s t r
    f _ = k

-- | Implicitly timed things
type ITimed (s :: *) = Timed s (Proxy s)

-- | Runs an Implicitly timed computation
runITimed      :: t -> (forall s. Reifies s t => ITimed s r) -> r
runITimed t tk = reify t $ \prx -> case tk of Timed k -> k prx




-- | Given a time t, a structure that was built at time s0, and a computation
-- that should run at a "compatible" time s; run the computation on the structure.
runAt1       :: forall s0 t k r f.
             t
            -> f (Timed s0 t k)
            -> (forall s. Reifies s t => f (Timed s t k) -> r)
            -> r
runAt1 t m f = reify t $ \prx -> f (coerceTo prx m)
  where
    coerceTo   :: proxy s -> f (Timed s' t k) -> f (Timed s t k)
    coerceTo _ = unsafeCoerce

-- | Runs a 2 parameter computation at a given time.
runAt2       :: forall s0 t k r f v.
               t
            -> f (Timed s0 t k) v
            -> (forall s. Reifies s t => f (Timed s t k) v -> r)
            -> r
runAt2 t m f = reify t $ \prx -> f (coerceTo prx m)
  where
    coerceTo   :: proxy s -> f (Timed s' t k) v -> f (Timed s t k) v
    coerceTo _ = unsafeCoerce






--------------------------------------------------------------------------------
unTimed1 :: f (Timed s t k) -> f (Timed () t k)
unTimed1 = unsafeCoerce



-- | Forget about the 's' tag.
unTimed2 :: f (Timed s t k) v -> f (Timed () t k) v
unTimed2 = unsafeCoerce


--------------------------------------------------------------------------------

newtype MyLine = L (Double -> Double, String)

instance Show MyLine where
  show (L (_,s)) = s

line1 :: MyLine
line1 = L $ (\x -> x, "Line 1")

line2 :: MyLine
line2 = L $ (\x -> 10 - x, "Line 2")



-- initialMap =
newtype Tagged (s :: *) a = Tagged a




myIT :: Reifies s Double => ITimed s (Map (Timed s Double Double) MyLine)
myIT = Timed $ construct


construct   :: forall (s :: *). Reifies s Double
            => Proxy s -> Map (Timed s Double Double) MyLine
construct _ = let f l@(L (ll,_)) = (ll <$> currentTime, l)
              in Map.fromList . map f $ [line1, line2]


construct0 :: forall (s :: *). Reifies s Double
           => Proxy s -> Map (Timed () Double Double) MyLine
construct0 = unTimed2 . construct



testz = reify 0 construct0





wrapMap :: Map (Timed s t k) v -> Timed s t (Map (Timed s t k) v)
wrapMap = pure

initial = runTimed 0 $ Map.elems <$> wrapMap (construct Proxy)


with     :: r -> Map (Timed s t k) v -> Timed s t r
with x _ = pure x




query'   ::  forall s.Reifies s Double => Double -> Timed s Double (Maybe MyLine)
query' y = (fmap snd . Map.lookupGE (with y construct00)) <$> wrapMap construct00
  where
    construct00 = construct0 (Proxy :: Proxy s)

-- query       :: (Double,Double) -> Maybe MyLine
-- query (x,y) = runTimed x $ query' y



-- initial = runTimed 0 $ construct




-- timedMap :: Num t => Timed s t [(t,String)]
-- timedMap = sequence $ [ pure (10,"ten"), (,"timed") <$> currentTime, undefined ]


-- qry    :: Eq t => [(t,v)] -> Timed s t (Maybe v)
-- qry xs = flip lookup xs <$> currentTime


-- computation :: Reifies s Int => Timed s Int (Maybe String)
-- computation = timedMap >>= qry



-- runC = runTimed 5 computation


-- getTime :: Timed s Int Int
-- getTime = currentTime


-- testComputation   :: forall (s:: *) t proxy. (Reifies s t, Ord t) => t -> proxy s -> Bool
-- testComputation i = \prx -> currentTime < constT prx i

-- testComputation2 i = (< i) <$> currentTime


-- test1 i = reify 5 $ testComputation i


-- -- testz :: Timed s Int [Int]
-- -- testz = Map.fromList <$> sequence [ pure (10,, getTime ]




-- test3 t = reify t $ \prx -> query $ testzz prx

-- -- test3  :: Int -> Maybe String
-- -- test3 t = reify t $ \(prx :: Proxy s) -> (testz ::Map (Timed s Int Int) String)

-- testzz :: forall s. Reifies s Int => Proxy s -> Map (Timed s Int Int) String
-- testzz _ = testz1

-- testz1 :: forall s. (Reifies s Int) => Map (Timed s Int Int) String
-- testz1 = testz

-- testz :: (Reifies s t, Num t, Ord t) => Map (Timed s t t) String
-- testz = Map.fromList [(pure 10,"ten"), (currentTime,"timed")]


-- -- queryz     :: (Reifies s t, Num t, Ord t) => proxy s -> t -> String
-- -- queryz _ t = Map.lookupGE (pure t) testz

-- -- test5 t = reify t queryz




-- test2M   :: Reifies s Int => proxy s -> Map (Timed s Int Int) String
-- test2M p = Map.fromList [ (constT p 10, "ten")
--                         , (getTime, "timed")
--                         ]


-- query :: forall s v. Ord (Timed s Int Int)
--       => Map (Timed s Int Int) v -> Maybe v
-- query = fmap snd . Map.lookupGE (constT (Proxy :: Proxy s) 4)


-- test2   :: Int -> Maybe String
-- test2 t = runAt t m query
--   where
--     m :: Map (Timed () Int Int) String
--     m = reify 0 $ \p -> unTagged $ test2M p





-- -- test2 = reify 0 $ \p0 ->
-- --                     let m = unTagged $ test2M p0
-- --                     in runAt 10 m Map.lookup



-- -- newtype Key s a b = Key { getKey :: a -> b }

-- -- instance (Eq b, Reifies s a) => Eq (Key s a b) where
-- --   (Key f) == (Key g) = let x = reflect (Proxy :: Proxy s)
-- --                        in f x == g x

-- -- instance (Ord b, Reifies s a) => Ord (Key s a b) where
-- --   Key f `compare` Key g = let x = reflect (Proxy :: Proxy s)
-- --                           in f x `compare` g x


-- -- -- | Query the sweep
-- -- queryAt       :: a
-- --               -> (forall (s :: *). Reifies s a => Map (Key s a b) v -> res)
-- --               -> Map (a -> b) v -> res
-- -- queryAt x f m = reify x (\p -> f . coerceKeys p $ m)

-- -- updateAt      :: a
-- --               -> (forall (s :: *). Reifies s a =>
-- --                    Map (Key s a b) v -> Map (Key s a b) v')
-- --               -> Map (a -> b) v
-- --               -> Map (a -> b) v'
-- -- updateAt x f m = reify x (\p -> uncoerceKeys . f . coerceKeys p $ m)


-- -- combineAt            :: a
-- --                      -> (forall (s :: *). Reifies s a =>
-- --                            Map (Key s a b) v -> Map (Key s a b) v
-- --                            -> Map (Key s a b) v)
-- --                      -> Map (a -> b) v
-- --                      -> Map (a -> b) v
-- --                      -> Map (a -> b) v
-- -- combineAt x uF m1 m2 = reify x (\p -> uncoerceKeys $
-- --                                         coerceKeys p m1 `uF` coerceKeys p m2)


-- -- splitLookupAt       :: Ord b
-- --                     => a
-- --                     -> (a -> b)
-- --                     -> Map (a -> b) v
-- --                     -> (Map (a -> b) v, Maybe v, Map (a -> b) v)
-- -- splitLookupAt x k m = reify x (\p -> let (l,mv,r) = Map.splitLookup (Key k)
-- --                                                   $ coerceKeys p m
-- --                                    in (uncoerceKeys l, mv, uncoerceKeys r))


-- -- --------------------------------------------------------------------------------

-- -- coerceKeys   :: proxy s -> Map (a -> b) v -> Map (Key s a b) v
-- -- coerceKeys _ = unsafeCoerce

-- -- uncoerceKeys :: Map (Key s a b) v -> Map (a -> b) v
-- -- uncoerceKeys = unsafeCoerce


-- -- --------------------------------------------------------------------------------


-- -- data Node a = Node2 a a
-- --             | Node3 a a a

-- -- data FT a = Single a
-- --           | Deep (FT (Node a)) a (FT (Node a))

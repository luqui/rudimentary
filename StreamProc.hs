{-# LANGUAGE GeneralizedNewtypeDeriving, LambdaCase, BangPatterns, TupleSections #-}

module StreamProc where

import Control.Monad (liftM, ap, (>=>), join)
import Control.Arrow (first)

newtype TimeDiff = TimeDiff Double
    deriving (Eq, Ord, Num, Show)

infinity :: TimeDiff
infinity = TimeDiff (1/0)

data Future i a 
    = Wait TimeDiff (Maybe (TimeDiff, i) -> Future i a)
    | Return a

instance Monad (Future i) where
    return = Return
    Return a >>= t = t a
    Wait d f >>= t = Wait d (f >=> t)

instance Functor (Future i) where fmap = liftM
instance Applicative (Future i) where pure = return; (<*>) = ap

mapFilterIF :: (i' -> Maybe i) -> Future i a -> Future i' a
mapFilterIF f (Wait d f') = Wait d $ mapFilterIF f . \case
    Nothing -> f' Nothing
    Just (t,x) | Just x' <- f x -> f' (Just (t,x'))
               | otherwise      -> Wait (d-t) f'
mapFilterIF f (Return x) = Return x

waitF :: TimeDiff -> Future i (Maybe i)
waitF d = Wait d (return . fmap snd)

waitTimeF :: TimeDiff -> Future i ()
waitTimeF d = Wait d $ \case
    Nothing -> return ()
    Just (d',_) -> waitTimeF (d-d')

waitForeverF :: Future i i
waitForeverF = Wait infinity (\(Just (_, x)) -> return x)

tagTimeF :: Future i a -> Future i (TimeDiff, a)
tagTimeF (Return x) = Return (0, x)
tagTimeF (Wait d f) = Wait d $ \case
    Nothing -> fmap (first (+d)) (tagTimeF (f Nothing))
    i@(Just (d',_)) -> fmap (first (+d')) (tagTimeF (f i))

chooseFirstF :: Future i a -> Future i b -> Future i (Either (a, Future i b) (Future i a, b))
chooseFirstF (Return x) f = Return (Left (x, f))
chooseFirstF f (Return y) = Return (Right (f, y))
chooseFirstF (Wait d f) (Wait d' f') = Wait (min d d') $ \case 
    i@(Just _) -> chooseFirstF (f i) (f' i)
    Nothing | d <= d'   -> chooseFirstF (f Nothing) (Wait (d'-d) f')
            | otherwise -> chooseFirstF (Wait (d-d') f) (f' Nothing)


data StreamProc i o a 
    = Input (Future i (StreamProc i o a))
    | Output o (StreamProc i o a)
    | Caboose a

instance Monad (StreamProc i o) where
    return = Caboose
    Input f >>= t = Input (fmap (>>= t) f)
    Output o s >>= t = Output o (s >>= t)
    Caboose a >>= t = t a

instance Functor (StreamProc i o) where fmap = liftM
instance Applicative (StreamProc i o) where pure = return; (<*>) = ap

mapFilterIO :: (i' -> Maybe i) -> (o -> Maybe o') -> StreamProc i o a -> StreamProc i' o' a
mapFilterIO ifun ofun (Input c) = Input (fmap (mapFilterIO ifun ofun) (mapFilterIF ifun c))
mapFilterIO ifun ofun (Output o p) = maybe id Output (ofun o) (mapFilterIO ifun ofun p)
mapFilterIO ifun ofun (Caboose x) = Caboose x

mapO :: (o -> o') -> StreamProc i o a -> StreamProc i o' a
mapO f = mapFilterIO Just (Just . f)

fromFuture :: Future i a -> StreamProc i o a
fromFuture = Input . fmap return

wait :: TimeDiff -> StreamProc i o (Maybe i)
wait = fromFuture . waitF

waitTime :: TimeDiff -> StreamProc i o ()
waitTime = fromFuture . waitTimeF

waitForever :: StreamProc i o i
waitForever = fromFuture waitForeverF

output :: o -> StreamProc i o ()
output x = Output x (Caboose ())

mergeInner :: StreamProc i o a -> StreamProc i o a -> StreamProc i o a
mergeInner (Caboose x) _ = Caboose x
mergeInner _ (Caboose y) = Caboose y
mergeInner (Output o xp) yp = Output o (mergeInner xp yp)
mergeInner xp (Output o yp) = Output o (mergeInner xp yp)
mergeInner (Input x) (Input y) = Input $ fmap switch (chooseFirstF x y)
    where
    switch (Left (s,f)) = mergeInner s (Input f)
    switch (Right (f,s)) = mergeInner (Input f) s

mergeOuter :: StreamProc i o a -> StreamProc i o b -> StreamProc i o (a,b)
mergeOuter (Caboose x) yp = fmap (x,) yp
mergeOuter xp (Caboose y) = fmap (,y) xp
mergeOuter (Output o xp) yp = Output o (mergeOuter xp yp)
mergeOuter xp (Output o yp) = Output o (mergeOuter xp yp)
mergeOuter (Input x) (Input y) = Input $ fmap switch (chooseFirstF x y)
    where
    switch (Left (s,f)) = mergeOuter s (Input f)
    switch (Right (f,s)) = mergeOuter (Input f) s


identity :: StreamProc i i a
identity = waitForever >>= \x -> output x >> identity

tagTimeHelper :: StreamProc i o a -> StreamProc i (TimeDiff, o) (TimeDiff, a)
tagTimeHelper = go 0
    where   
    go !accum (Input f) = Input (fmap (\(t,p) -> go (accum+t) p) (tagTimeF f))
    go !accum (Output o p) = Output (accum,o) (go accum p)
    go !accum (Caboose a) = Caboose (accum,a)

tagTime :: StreamProc i o a -> StreamProc i o (TimeDiff, a)
tagTime = mapO snd . tagTimeHelper

tagTimeOutputs :: StreamProc i o a -> StreamProc i (TimeDiff, o) a
tagTimeOutputs = fmap snd . tagTimeHelper

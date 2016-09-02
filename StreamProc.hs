{-# LANGUAGE GeneralizedNewtypeDeriving, LambdaCase, BangPatterns #-}

import Control.Monad (liftM, ap, (>=>))
import Control.Arrow (first)

newtype TimeDiff = TimeDiff Double
    deriving (Eq, Ord, Num)

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

mapIF :: (i' -> i) -> Future i a -> Future i' a
mapIF f (Wait d f') = Wait d (mapIF f . f' . (fmap.fmap) f)

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

mapI :: (i' -> i) -> StreamProc i o a -> StreamProc i' o a
mapI f (Input c) = Input (fmap (mapI f) (mapIF f c))
mapI f (Output o p) = Output o (mapI f p)
mapI _ (Caboose a) = Caboose a

mapO :: (o -> o') -> StreamProc i o a -> StreamProc i o' a
mapO f (Input c) = Input (fmap (mapO f) c)
mapO f (Output o p) = Output (f o) (mapO f p)
mapO _ (Caboose a) = Caboose a


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

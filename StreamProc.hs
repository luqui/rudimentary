{-# LANGUAGE GeneralizedNewtypeDeriving #-}

import Control.Monad (liftM, ap, (>=>))

newtype TimeDiff = TimeDiff Double
    deriving (Eq, Ord, Num)

data Future i a 
    = Wait TimeDiff (Maybe (TimeDiff, i) -> Future i a)
    | Return a

instance Monad (Future i) where
    return = Return
    Return a >>= t = t a
    Wait d f >>= t = Wait d (f >=> t)

instance Functor (Future i) where fmap = liftM
instance Applicative (Future i) where pure = return; (<*>) = ap


data Train f o a 
    = Car (f (o, Train f o a))
    | Caboose a

instance (Functor f) => Monad (Train f o) where
    return = Caboose
    Caboose a >>= t = t a
    Car f >>= t = Car ((fmap.fmap) (>>= t) f)

instance (Functor f) => Functor (Train f o) where fmap = liftM
instance (Functor f) => Applicative (Train f o) where pure = return; (<*>) = ap


type StreamProc i o a = Train (Future i) o a

{-# LANGUAGE GeneralizedNewtypeDeriving #-}

module Params where
-- A monad for tracking parameterized computations (e.g. a dynamically branching web form)

import Control.Monad.Trans.Reader
import Control.Monad.Trans.Writer
import Data.Monoid
import Control.Arrow (second)
import Control.Monad.Trans.Class (lift)
import qualified Data.DList as DList
import qualified Data.Map as Map

-- l : label
-- w : widget
-- v : value

type ParamsInput l v = Map.Map l v

newtype ParamsOutput l w v = ParamsOutput { getParamsOutput :: DList.DList (l, w, v) }
    deriving (Monoid)

newtype Params l w v a = Params { 
        unParams :: ReaderT (ParamsInput l v) (Writer (ParamsOutput l w v)) a }
    deriving (Functor, Applicative, Monad)

param :: (Ord l) => l -> w -> v -> Params l w v v
param label widget def = Params $ do
    input <- ask
    let value = maybe def id (Map.lookup label input)
    lift $ tell (ParamsOutput (DList.singleton (label, widget, value)))
    return value

runParams :: Params l w v a -> Map.Map l v -> (a, [(l, w, v)])
runParams p input = second (DList.toList . getParamsOutput) (runWriter (runReaderT (unParams p) input))

outputToInput :: (Ord l) => [(l, w, v)] -> Map.Map l v
outputToInput outs = Map.fromList [ (l,v) | (l,w,v) <- outs ]

data SelectWidget = SelectWidget {
    swTitle :: String,
    swOptions :: [(String, String)]   -- title, description
  }

selectWidget :: (Ord l) => l -> SelectWidget -> Params l SelectWidget String String
selectWidget l w = param l w (fst.head.swOptions $ w)

select :: (Ord l) => l -> String -> [(String, String, a)] -> Params l SelectWidget String a
select l title options = getValue <$> selectWidget l (SelectWidget title (map withoutValue options))
    where
    withoutValue (a,b,c) = (a,b)
    getValue s = head [ x | (v,_,x) <- options, v == s ]

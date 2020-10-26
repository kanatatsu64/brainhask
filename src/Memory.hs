{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}

module Memory (
    Memory,
    MemoryStateT,
    load,
    read,
    sread,
    write,
    swrite,
    forward,
    sforward,
    backward,
    sbackward,
    dump
) where

import Prelude hiding (init, read)
import Control.Monad.State
import Control.Monad.Except

import Initial (Initial(init))
import StateC (WithState(..))

data Memory a = Memory [a] [a]
newtype MemoryStateT a m b = MemoryStateT (StateT (Memory a) m b)
    deriving (
        Functor,
        Applicative,
        Monad,
        MonadState (Memory a),
        MonadIO,
        MonadError e
    )

instance Initial (Memory a) where
    init = Memory init init

instance WithState (MemoryStateT a) (Memory a) where
    runT (MemoryStateT s) = s
    restoreT = MemoryStateT

outOfRangeMsg :: (Show a) => Memory a -> String
outOfRangeMsg (Memory ls rs) = "memory out of range: " <>
                               foldl (<>) "" (map show (reverse ls)) <>
                               "|" <>
                               foldl (<>) "" (map show rs)

load :: (Monad m) => [a] -> MemoryStateT a m ()
load (x:xs) = put $ Memory [x] xs
load [] = put $ Memory [] []

read :: (Monad m, Initial a) => MemoryStateT a m a
read = state $ \case
    Memory ls@(l:_) rs -> (l, Memory ls rs)
    Memory [] rs -> (init, Memory [init] rs)

sread :: (MonadError String m, Show a) => MemoryStateT a m a
sread = restoreM $ \case
    Memory ls@(l:_) rs -> return (l, Memory ls rs)
    m -> throwError $ outOfRangeMsg m

write :: (Monad m) => a -> MemoryStateT a m ()
write x = modify $ \case
    Memory (_:ls) rs -> Memory (x:ls) rs
    Memory [] rs -> Memory [x] rs

swrite :: (MonadError String m, Show a) => a -> MemoryStateT a m ()
swrite x = restoreM $ \case
    Memory (_:ls) rs -> return ((), Memory (x:ls) rs)
    m -> throwError $ outOfRangeMsg m

forward :: (Monad m, Initial a) => MemoryStateT a m ()
forward = modify $ \case
    Memory ls (r:rs) -> Memory (r:ls) rs
    Memory ls [] -> Memory (init:ls) []

sforward :: (MonadError String m, Show a) => MemoryStateT a m ()
sforward = restoreM $ \case
    Memory ls (r:rs) -> return ((), Memory (r:ls) rs)
    m -> throwError $ outOfRangeMsg m

backward :: (Monad m, Initial a) => MemoryStateT a m ()
backward = modify $ \case
    Memory (l:ls) rs -> Memory ls (l:rs)
    Memory [] rs -> Memory [] (init:rs)

sbackward :: (MonadError String m, Show a) => MemoryStateT a m ()
sbackward = restoreM $ \case
    Memory (l:ls) rs -> return ((), Memory ls (l:rs))
    m -> throwError $ outOfRangeMsg m

dump :: (Monad m) => MemoryStateT a m [a]
dump = state $ \case
    m@(Memory ls rs) -> (reverse ls ++ rs, m)
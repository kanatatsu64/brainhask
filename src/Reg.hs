{-# LANGUAGE FlexibleContexts #-}

module Reg (
    inc,
    dec,
    input,
    output
) where

import Control.Monad.IO.Class
import Control.Monad.Except

import Initial (Initial)
import Memory (MemoryStateT)
import qualified Memory
import qualified IO

inc :: (Monad m) => MemoryStateT Int m ()
inc = do
    x <- Memory.read
    Memory.write (x+1)

dec :: (Monad m) => MemoryStateT Int m ()
dec = do
    x <- Memory.read
    Memory.write (x-1)

input :: (MonadIO m, MonadError String m, Read a) => MemoryStateT a m ()
input = do
    x <- IO.input
    Memory.write x

output :: (MonadIO m, Show a, Initial a) => MemoryStateT a m ()
output = do
    x <- Memory.read
    IO.output x
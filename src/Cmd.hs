{-# LANGUAGE FlexibleContexts #-}

module Cmd (
    jump
) where

import Control.Monad.State
import Control.Monad.Except

import Memory (MemoryStateT)
import qualified Memory

jump :: (MonadError String m, Show a) => Int -> MemoryStateT a m ()
jump n
    | n > 0 = do
        Memory.sforward
        jump (n - 1)
    | n < 0 = do
        Memory.sbackward
        jump (n + 1)
    | n == 0 = modify id
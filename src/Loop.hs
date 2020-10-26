{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE FlexibleContexts #-}

module Loop (
    Stack,
    StackStateT,
    pop,
    push,
    apply,
    inc,
    dec,
    dump,
    isEmpty
) where

import Prelude hiding (init)
import Control.Monad.State
import Control.Monad.Except

import Initial(Initial(init))
import StateC(WithState(..))

newtype Stack a = Stack [a]
newtype StackStateT a m b = StackStateT (StateT (Stack a) m b)
    deriving (
        Functor,
        Applicative,
        Monad,
        MonadState (Stack a),
        MonadError e
    )

instance Initial (Stack a) where
    init = Stack init

instance WithState (StackStateT a) (Stack a) where
    runT (StackStateT s) = s
    restoreT = StackStateT

emptyStackMsg :: String
emptyStackMsg = "stack is empty"

pop :: (MonadError String m) => StackStateT a m a
pop = do
    Stack xs <- get
    case xs of
        (y:ys) -> do
            put $ Stack ys
            return y
        [] -> throwError emptyStackMsg

push :: (Monad m) => a -> StackStateT a m ()
push x = modify (\(Stack xs) -> Stack (x:xs))

apply :: (MonadError String m) => (a -> a) -> StackStateT a m ()
apply f = do
    Stack xs <- get
    case xs of
        x:xs -> put $ Stack (f x:xs)
        [] -> throwError emptyStackMsg

inc :: (Num a, MonadError String m) => StackStateT a m ()
inc = apply (+1)

dec :: (Num a, MonadError String m) => StackStateT a m ()
dec = apply (subtract 1)

dump :: Monad m => StackStateT a m [a]
dump = do
    Stack xs <- get
    return xs

isEmpty :: Monad m => StackStateT a m Bool
isEmpty = do
    Stack xs <- get
    return $ length xs == 0
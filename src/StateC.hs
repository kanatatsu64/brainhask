{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FunctionalDependencies #-}

module StateC (
    WithState(..),
    liftStateT,
    liftFirstStateT,
    liftSecondStateT,
    runFirstStateT,
    runSecondStateT,
    swapState
) where

import Control.Monad.State
import Control.Arrow

class WithState t s | t -> s where
    runT :: t m a -> StateT s m a
    restoreT :: StateT s m a -> t m a

    runM :: t m a -> s -> m (a, s)
    runM = runStateT . runT

    restoreM :: (s -> m (a, s)) -> t m a
    restoreM = restoreT . StateT

instance WithState (StateT s) s where
    runT = id
    restoreT = id

_liftWithState :: (WithState t1 s1, WithState t2 s2) =>
    (StateT s1 m a1 -> StateT s2 m a2) -> t1 m a1 -> t2 m a2
_liftWithState f = restoreT . f . runT

liftStateT :: (WithState t p, WithState u c, Monad m) => (p -> c) -> (p -> c -> p) -> u m a -> t m a
liftStateT run restore s = restoreT . StateT $ \p -> second (restore p) <$> runStateT (runT s) (run p)

liftFirstStateT :: (WithState t s1, WithState u (s1, s2), Monad m) => t m a1 -> u m a1
liftFirstStateT s = restoreM $ \(s1, s2) -> do
    (a1, s1') <- runM s s1
    return (a1, (s1', s2))

liftSecondStateT :: (WithState t s2, WithState u (s1, s2), Monad m) => t m a2 -> u m a2
liftSecondStateT s = restoreM $ \(s1, s2) -> do
    (a2, (s2', s1')) <- runStateT (liftFirstStateT s) (s2, s1)
    return (a2, (s1', s2'))

swapState :: (WithState t (s1, s2), WithState u (s2, s1), Monad m) => t m a -> u m a
swapState = _liftWithState _swapState

_swapState :: (Monad m) => StateT (s1, s2) m a -> StateT (s2, s1) m a
_swapState s = StateT $ \(s2, s1) -> do
    (a, (s1', s2')) <- runStateT s (s1, s2)
    return (a, (s2', s1'))

runFirstStateT :: (WithState t (s1, s2), WithState u s2, Monad m) => t m a -> s1 -> u m a
runFirstStateT s s1 = _liftWithState (\s -> _runFirstStateT s s1) s

_runFirstStateT :: (Monad m) => StateT (s1, s2) m a -> s1 -> StateT s2 m a
_runFirstStateT s s1 = StateT $ \s2 -> do
    (a, (_, s2')) <- runStateT s (s1, s2)
    return (a, s2')

runSecondStateT :: (WithState t (s1, s2), WithState u s1, Monad m) => t m a -> s2 -> u m a
runSecondStateT s s2  = _liftWithState (\s -> _runSecondStateT s s2) s

_runSecondStateT :: (Monad m) => StateT (s1, s2) m a -> s2 -> StateT s1 m a
_runSecondStateT s s2 = runFirstStateT (_swapState s) s2
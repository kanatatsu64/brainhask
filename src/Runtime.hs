{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE MultiParamTypeClasses #-}

module Runtime (
   load,
   run,
   Runtime,
   RuntimeStateT
) where

import Prelude hiding (init, read)
import Control.Monad.State
import Control.Monad.Except

import Initial (Initial(init))
import StateC (liftStateT, liftFirstStateT, liftSecondStateT, runSecondStateT, WithState(..))
import Memory (MemoryStateT )
import Loop (Stack, StackStateT)
import qualified Loop
import qualified Memory
import qualified Reg
import qualified Cmd

data Runtime = Runtime { reg :: Memory.Memory Int, cmd :: Memory.Memory Char, loop :: Stack Int }
newtype RuntimeStateT m a = RuntimeStateT (StateT Runtime m a)
    deriving (
       Functor,
       Applicative,
       Monad,
       MonadState Runtime,
       MonadIO
    )

instance Initial Runtime where
   init = Runtime init init init

instance WithState RuntimeStateT Runtime where
   runT (RuntimeStateT s) = s
   restoreT = RuntimeStateT

liftReg :: Monad m => MemoryStateT Int m a -> RuntimeStateT m a
liftReg = liftStateT reg (\p c -> p { reg = c })

liftCmd :: Monad m => MemoryStateT Char m a -> RuntimeStateT m a
liftCmd = liftStateT cmd (\p c -> p { cmd = c })

liftLoop :: Monad m => StackStateT Int m a -> RuntimeStateT m a
liftLoop = liftStateT loop (\p c -> p { loop = c })

load :: Monad m => [Char] -> RuntimeStateT m ()
load xs = liftCmd $ Memory.load xs

forward :: Monad m => RuntimeStateT m ()
forward = liftReg Memory.forward

backward :: Monad m => RuntimeStateT m ()
backward = liftReg Memory.backward

inc :: Monad m => RuntimeStateT m ()
inc = liftReg Reg.inc

dec :: Monad m => RuntimeStateT m ()
dec = liftReg Reg.dec

input :: (MonadIO m, MonadError String m) => RuntimeStateT m ()
input = liftReg Reg.input

output :: MonadIO m => RuntimeStateT m ()
output = liftReg Reg.output

addSF :: Monad m => RuntimeStateT m ()
addSF = liftLoop $ Loop.push init

delSF :: MonadError String m => RuntimeStateT m ()
delSF = liftLoop $ do
   v <- Loop.pop
   cond <- Loop.isEmpty
   if not cond then Loop.apply (+v) else return ()

resetSF :: MonadError String m => RuntimeStateT m Int
resetSF = do
   d <- liftLoop Loop.pop
   addSF
   return d

incSF :: MonadError String m => RuntimeStateT m ()
incSF = liftLoop Loop.inc

jumpf :: MonadError String m => RuntimeStateT m ()
jumpf = do
   v <- liftReg Memory.read
   case v of
      v | v == 0 -> jump_
        | otherwise -> addSF
   where jump_ = runSecondStateT forward_ 1
         forward_ :: (MonadError String m) => StateT (Runtime, Int) m ()
         forward_ = do
            liftFirstStateT next
            c <- liftFirstStateT read
            p <- liftSecondStateT $ check_ c
            case p of
               p | p == 0 -> return ()
                 | otherwise -> forward_
         check_ :: (Monad m) => Char -> StateT Int m Int
         check_ c = do
               case c of
                  '[' -> modify (+1)
                  ']' -> modify (subtract 1)
                  _ -> return ()
               get

jumpb :: (MonadIO m, MonadError String m) => RuntimeStateT m ()
jumpb = do
   v <- liftReg Memory.read
   case v of
      v | v == 0 -> delSF
        | otherwise -> jump_
   where jump_ = do
            d <- resetSF
            jump (-d)

nop :: Monad m => RuntimeStateT m ()
nop = modify id

data RET = CONT | QUIT

cont :: Monad m =>RuntimeStateT m RET
cont = return CONT

quit :: Monad m => RuntimeStateT m RET
quit = return QUIT

exec :: (MonadIO m, MonadError String m) => Char -> RuntimeStateT m RET
exec '>' = forward >> cont
exec '<' = backward >> cont
exec '+' = inc >> cont
exec '-' = dec >> cont
exec '.' = output >> cont
exec ',' = input >> cont
exec '[' = jumpf >> cont
exec ']' = jumpb >> cont
exec ';' = quit
exec _  = nop >> cont

next :: MonadError String m => RuntimeStateT m ()
next = do
   liftCmd Memory.sforward
   cond <- liftLoop Loop.isEmpty
   if not cond then incSF else return ()

read :: MonadError String m => RuntimeStateT m Char
read = liftCmd Memory.sread

jump :: MonadError String m => Int -> RuntimeStateT m ()
jump n = liftCmd $ Cmd.jump n

run :: (MonadIO m, MonadError String m) => RuntimeStateT m ()
run = do
    c <- read
    v <- exec c
    case v of
       CONT -> do
          next
          run
       QUIT -> return ()
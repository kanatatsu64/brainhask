{-# LANGUAGE FlexibleContexts #-}

module Main where

import Prelude hiding (init)
import Control.Monad.Except

import Initial (Initial(init))
import StateC (WithState(..))
import Runtime (run, load, Runtime, RuntimeStateT)

type MainState a = RuntimeStateT (ExceptT String IO) a

{-
 Operators
    >: increment the data pointer
    <: decrement the data pointer
    +: increment the byte at the data pointer
    -: decrement the byte at the data pointer
    .: out put the byte at the data pointer
    ,: accept one byte of input, storing its value in the byte at the data pointer
    [: if the byte at the data pointer is zero, then instead of moving the instruction pointer forward to the next
       command, jump it forward to the command after the matching ] command.
    ]: if the byte at the data pointer is nonzero, then instead of moving the instruction pointer forward to the next
       command, jump it back to the ommand after the matching [ command.
-}

runMainState :: MainState a -> Runtime -> IO (Either String (a, Runtime))
runMainState s runtime = runExceptT $ runM s runtime

runString :: String -> MainState ()
runString str = do
   load str
   run

main :: IO ()
main = do
   program <- getLine
   v <- runMainState (runString program) init
   case v of
      Left err -> print err
      Right _ -> return ()

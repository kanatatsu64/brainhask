{-# LANGUAGE FlexibleContexts #-}

module IO (
    input,
    output
) where

import Control.Monad.IO.Class
import Control.Monad.Except
import Text.Read (readMaybe)

input :: (MonadIO m, MonadError String m, Read a) => m a
input = do
    str <- liftIO $ getLine
    case (readMaybe str) of
        Just x -> return x
        Nothing -> throwError $ "failed to parse the input: " <> str

output :: (MonadIO m, Show a) => a -> m ()
output = liftIO . print
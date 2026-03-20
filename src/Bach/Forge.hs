module Bach.Forge
    ( ForgeHandle (..)
    , HasForgeHandle (..)
    , MonadForge (..)
    ) where

import Bach.Prelude
import Bach.Types

data ForgeHandle = ForgeHandle
    { forgeFetchPR :: PRIdentifier -> IO PullRequest
    , forgeMarkDraft :: Int -> IO ()
    , forgeMarkReady :: Int -> IO ()
    }

class HasForgeHandle env where
    forgeHandleL :: Lens' env ForgeHandle

class (Monad m) => MonadForge m where
    fetchPR :: PRIdentifier -> m PullRequest
    markDraft :: Int -> m ()
    markReady :: Int -> m ()

instance (HasForgeHandle env) => MonadForge (RIO env) where
    fetchPR prid = do
        fh <- view forgeHandleL
        liftIO $ forgeFetchPR fh prid
    markDraft n = do
        fh <- view forgeHandleL
        liftIO $ forgeMarkDraft fh n
    markReady n = do
        fh <- view forgeHandleL
        liftIO $ forgeMarkReady fh n

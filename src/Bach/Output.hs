module Bach.Output
    ( outputHuman
    , outputJson
    , outputGhActions
    ) where

import Bach.Prelude
import Bach.Types
import Data.Aeson (encode)
import qualified Data.Text as T
import qualified Data.Text.Encoding as TE
import qualified RIO.ByteString as BS
import qualified RIO.ByteString.Lazy as LBS

outputHuman :: (MonadIO m) => FugueResults -> m ()
outputHuman results = do
    forM_ results.frBaseConflicts $ \baseConflicts -> do
        putLine ""
        putLine
            $ mconcat ["=== NEEDS REBASE (", tshow (length baseConflicts), ") ==="]
        forM_ baseConflicts $ \pr ->
            putLine $ mconcat ["  #", tshow pr.prNumber, " ", pr.prTitle]

    forM_ results.frConflictPairs $ \conflictPairs -> do
        putLine ""
        putLine "=== CONFLICT PAIRS ==="
        forM_ conflictPairs $ \cp -> do
            putLine
                $ mconcat ["  #", tshow cp.cpLeft, " conflicts with #", tshow cp.cpRight]
            unless (null cp.cpFiles)
                $ putLine
                $ "    files: "
                <> T.intercalate ", " cp.cpFiles

    putLine ""
    putLine
        $ mconcat ["=== READY TO MERGE (", tshow (length results.frReady), ") ==="]
    if null results.frReady
        then putLine "  (none)"
        else forM_ results.frReady $ \pr ->
            putLine $ mconcat ["  #", tshow pr.prNumber, " ", pr.prTitle]

    unless (null results.frDeferred) $ do
        putLine ""
        putLine
            $ mconcat ["=== DEFERRED (", tshow (length results.frDeferred), ") ==="]
        forM_ results.frDeferred $ \pr ->
            putLine $ mconcat ["  #", tshow pr.prNumber, " ", pr.prTitle]

outputJson :: (MonadIO m) => FugueResults -> m ()
outputJson results = liftIO $ LBS.hPut stdout (encode results <> "\n")

outputGhActions :: (MonadIO m) => FugueResults -> m ()
outputGhActions results = do
    forM_ results.frBaseConflicts $ \baseConflicts -> do
        putLine
            $ mconcat ["# === NEEDS REBASE (", tshow (length baseConflicts), ") ==="]
        forM_ baseConflicts $ \pr ->
            putLine $ mconcat ["# gh pr checkout ", tshow pr.prNumber, "  # ", pr.prTitle]
        putLine ""

    putLine
        $ mconcat ["# === READY TO MERGE (", tshow (length results.frReady), ") ==="]
    forM_ results.frReady $ \pr ->
        putLine $ "gh pr ready " <> tshow pr.prNumber
    putLine ""

    unless (null results.frDeferred) $ do
        putLine
            $ mconcat ["# === DEFERRED (", tshow (length results.frDeferred), ") ==="]
        forM_ results.frDeferred $ \pr ->
            putLine $ "# gh pr ready --undo " <> tshow pr.prNumber
        putLine ""

putLine :: (MonadIO m) => Text -> m ()
putLine t = liftIO $ do
    BS.hPut stdout (TE.encodeUtf8 t)
    BS.hPut stdout "\n"

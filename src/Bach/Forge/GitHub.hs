module Bach.Forge.GitHub
    ( mkGitHubForgeHandle
    , GhCommandFailed (..)
    , GhParseFailed (..)
    ) where

import Bach.Forge (ForgeHandle (..))
import Bach.Prelude
import Bach.Types
import Data.Aeson (eitherDecodeStrict')
import qualified Data.Text as T
import qualified RIO.ByteString.Lazy as LBS
import System.Process.Typed (proc, readProcess)

data GhCommandFailed = GhCommandFailed
    { gcfArgs :: ![String]
    , gcfStderr :: !Text
    }
    deriving stock (Show, Eq)

instance Exception GhCommandFailed where
    displayException (GhCommandFailed args err) =
        "gh " <> unwords args <> " failed: " <> T.unpack err

data GhParseFailed = GhParseFailed !Text
    deriving stock (Show, Eq)

instance Exception GhParseFailed where
    displayException (GhParseFailed err) =
        "Failed to parse gh JSON output: " <> T.unpack err

-- | Create a ForgeHandle that uses the @gh@ CLI to talk to GitHub.
mkGitHubForgeHandle :: RepoContext -> ForgeHandle
mkGitHubForgeHandle ctx =
    ForgeHandle
        { forgeFetchPR = ghFetchPR ctx
        , forgeMarkDraft = ghMarkDraft ctx
        , forgeMarkReady = ghMarkReady ctx
        }

ghFetchPR :: RepoContext -> PRIdentifier -> IO PullRequest
ghFetchPR ctx prid = do
    let
        repo = repoOwner ctx <> "/" <> repoName ctx
        prRef = case prid of
            PRById n -> tshow n
            PRByBranch b -> b
        args =
            [ "pr"
            , "view"
            , T.unpack prRef
            , "--repo"
            , T.unpack repo
            , "--json"
            , "number,title,headRefName,baseRefName,isDraft,url"
            ]
    (exitCode, out, err) <- readProcess (proc "gh" args)
    case exitCode of
        ExitFailure _ ->
            throwIO $ GhCommandFailed args (tshow err)
        ExitSuccess ->
            case eitherDecodeStrict' (LBS.toStrict out) of
                Left e -> throwIO $ GhParseFailed (T.pack e)
                Right pr -> pure pr

ghMarkDraft :: RepoContext -> Int -> IO ()
ghMarkDraft ctx n = do
    let
        repo = repoOwner ctx <> "/" <> repoName ctx
        args =
            [ "pr"
            , "ready"
            , show n
            , "--repo"
            , T.unpack repo
            , "--undo"
            ]
    (exitCode, _, err) <- readProcess (proc "gh" args)
    case exitCode of
        ExitFailure _ ->
            throwIO $ GhCommandFailed args (tshow err)
        ExitSuccess -> pure ()

ghMarkReady :: RepoContext -> Int -> IO ()
ghMarkReady ctx n = do
    let
        repo = repoOwner ctx <> "/" <> repoName ctx
        args =
            [ "pr"
            , "ready"
            , show n
            , "--repo"
            , T.unpack repo
            ]
    (exitCode, _, err) <- readProcess (proc "gh" args)
    case exitCode of
        ExitFailure _ ->
            throwIO $ GhCommandFailed args (tshow err)
        ExitSuccess -> pure ()

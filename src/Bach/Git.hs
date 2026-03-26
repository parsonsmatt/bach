module Bach.Git
    ( detectRepoContext
    , gitFetch
    , gitMergeTree
    , gitCommitTree
    , parseConflictFiles
    , GitCommandFailed (..)
    , RemoteUrlUnparseable (..)
    ) where

import Bach.Prelude
import Bach.Types
import Data.List (splitAt)
import qualified Data.Text as T
import qualified Data.Text.Encoding as TE
import qualified RIO.ByteString.Lazy as LBS
import System.Process.Typed (proc, readProcess, setWorkingDir)

data GitCommandFailed = GitCommandFailed ![String]
    deriving stock (Show, Eq, Typeable)

instance Exception GitCommandFailed where
    displayException (GitCommandFailed args) =
        "git " <> unwords args <> " failed"

data RemoteUrlUnparseable = RemoteUrlUnparseable !Text
    deriving stock (Show, Eq, Typeable)

instance Exception RemoteUrlUnparseable where
    displayException (RemoteUrlUnparseable url) =
        "Cannot parse remote URL: " <> T.unpack url

-- | Run a git command in the given working directory.
runGit :: (MonadIO m) => FilePath -> [String] -> m (ExitCode, Text, Text)
runGit dir args = do
    let
        p = setWorkingDir dir $ proc "git" args
    (exitCode, out, err) <- readProcess p
    pure
        ( exitCode
        , T.strip . TE.decodeUtf8 . LBS.toStrict $ out
        , T.strip . TE.decodeUtf8 . LBS.toStrict $ err
        )

-- | Run a git command, throwing GitError on failure.
runGit_ :: (MonadIO m) => FilePath -> [String] -> m Text
runGit_ dir args = do
    (exitCode, out, _) <- runGit dir args
    case exitCode of
        ExitSuccess -> pure out
        ExitFailure _ ->
            throwIO $ GitCommandFailed args

-- | Detect repository context from the current git working directory.
detectRepoContext :: (MonadIO m) => FilePath -> m RepoContext
detectRepoContext dir = do
    remoteUrl <- runGit_ dir ["remote", "get-url", "origin"]
    (owner, name) <- parseRemoteUrl remoteUrl
    defaultBase <- detectDefaultBranch dir
    pure
        RepoContext
            { repoOwner = owner
            , repoName = name
            , repoDefaultBase = defaultBase
            , repoLocalPath = dir
            }

-- | Parse owner/repo from a GitHub remote URL.
parseRemoteUrl :: (MonadIO m) => Text -> m (Text, Text)
parseRemoteUrl url = do
    let
        stripped = T.stripSuffix ".git" url & fromMaybe url
        parts = case T.splitOn ":" stripped of
            [_, ownerRepo]
                | not ("/" `T.isPrefixOf` ownerRepo) ->
                    T.splitOn "/" ownerRepo
            _ ->
                reverse . take 2 . reverse $ T.splitOn "/" stripped
    case parts of
        [owner, repo] -> pure (owner, repo)
        _ -> throwIO $ RemoteUrlUnparseable url

-- | Detect the default branch from origin/HEAD.
detectDefaultBranch :: (MonadIO m) => FilePath -> m Text
detectDefaultBranch dir = do
    (exitCode, out, _) <- runGit dir ["symbolic-ref", "refs/remotes/origin/HEAD"]
    case exitCode of
        ExitSuccess ->
            pure . lastDef "main" . T.splitOn "/" $ out
        ExitFailure _ ->
            pure "main"

lastDef :: a -> [a] -> a
lastDef def [] = def
lastDef _ [x] = x
lastDef def (_ : xs) = lastDef def xs

-- | Fetch specific refspecs from origin, in chunks of 20.
gitFetch
    :: (MonadIO m, MonadReader env m, HasLogFunc env) => FilePath -> [Text] -> m ()
gitFetch dir refs = do
    let
        chunks = chunksOf 20 refs
    forM_ chunks $ \chunk -> do
        let
            args = ["fetch", "origin", "--quiet", "--"] <> map T.unpack chunk
        (exitCode, _, errText) <- runGit dir args
        case exitCode of
            ExitSuccess -> pure ()
            ExitFailure code ->
                logWarn
                    $ "git fetch failed (exit "
                    <> displayShow code
                    <> "): "
                    <> display errText

-- | Test mergeability in-memory via @git merge-tree --write-tree@.
gitMergeTree :: (MonadIO m) => FilePath -> Text -> Text -> m MergeResult
gitMergeTree dir ref1 ref2 = do
    (exitCode, outText, _) <-
        runGit dir ["merge-tree", "--write-tree", "--", T.unpack ref1, T.unpack ref2]
    pure $ case exitCode of
        ExitSuccess -> MergeOk outText
        ExitFailure _ -> MergeConflict (parseConflictFiles outText)

-- | Create a temporary commit from a tree SHA for use as a merge base.
gitCommitTree
    :: (MonadIO m) => FilePath -> Text -> Text -> Text -> m (Maybe Text)
gitCommitTree dir treeSha parent1 parent2 = do
    (exitCode, outText, _) <-
        runGit
            dir
            [ "commit-tree"
            , T.unpack treeSha
            , "-p"
            , T.unpack parent1
            , "-p"
            , T.unpack parent2
            , "-m"
            , "temp"
            ]
    pure $ case exitCode of
        ExitSuccess -> Just outText
        ExitFailure _ -> Nothing

-- | Extract conflicting file paths from @git merge-tree@ output.
parseConflictFiles :: Text -> [Text]
parseConflictFiles output = mapMaybe extractFile (T.lines output)
  where
    marker = "Merge conflict in "
    extractFile line = case T.breakOn marker line of
        (_, rest)
            | not (T.null rest) -> Just $ T.strip $ T.drop (T.length marker) rest
        _ -> Nothing

chunksOf :: Int -> [a] -> [[a]]
chunksOf _ [] = []
chunksOf size xs =
    let
        (hd, tl) = splitAt size xs
     in
        hd : chunksOf size tl

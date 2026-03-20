module TestHelpers
    ( mkPR
    , git
    , withTestRepo
    , createBranch
    , withLog
    ) where

import Bach.Prelude
import Bach.Types
import RIO.FilePath ((</>))
import System.Process.Typed (proc, runProcess_, setWorkingDir)
import Prelude (writeFile)

mkPR :: Int -> Text -> PullRequest
mkPR n branch =
    PullRequest
        { prNumber = n
        , prTitle = "PR #" <> tshow n
        , prHeadRef = branch
        , prBaseRef = "main"
        , prIsDraft = False
        , prUrl = ""
        }

git :: FilePath -> [String] -> IO ()
git dir args = runProcess_ $ setWorkingDir dir $ proc "git" args

-- | Set up a test repo with a base commit, then run the action.
withTestRepo :: (FilePath -> IO a) -> IO a
withTestRepo action = withSystemTempDirectory "bach-test-" \dir -> do
    git dir ["init", "-b", "main"]
    git dir ["config", "user.email", "test@test.com"]
    git dir ["config", "user.name", "Test"]
    writeFile (dir </> "file.txt") "line1\nline2\nline3\nline4\nline5\n"
    writeFile (dir </> "other.txt") "aaa\nbbb\nccc\n"
    git dir ["add", "."]
    git dir ["commit", "-m", "initial"]
    git dir ["update-ref", "refs/remotes/origin/main", "refs/heads/main"]
    git dir ["symbolic-ref", "refs/remotes/origin/HEAD", "refs/remotes/origin/main"]
    action dir

-- | Create a branch from main with the given file changes, plus a fake origin ref.
createBranch :: FilePath -> String -> [(String, String)] -> IO ()
createBranch dir name changes = do
    git dir ["checkout", "-b", name, "main"]
    forM_ changes \(file, content) ->
        writeFile (dir </> file) content
    git dir ["add", "."]
    git dir ["commit", "-m", name]
    git dir ["update-ref", "refs/remotes/origin/" <> name, "refs/heads/" <> name]
    git dir ["checkout", "main"]

withLog :: (LogFunc -> IO a) -> IO a
withLog action = do
    logOpts <- logOptionsHandle stderr False
    withLogFunc (setLogMinLevel LevelError logOpts) action

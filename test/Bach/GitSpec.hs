module Bach.GitSpec (spec) where

import Bach.Conflicts (findConflicts, partitionBase)
import Bach.Git (gitMergeTree)
import Bach.Prelude
import Bach.Types
import Data.These (These (..))
import RIO.FilePath ((</>))
import Test.Hspec
import TestHelpers
import Prelude (writeFile)

spec :: Spec
spec = do
    describe "gitMergeTree" do
        it "returns MergeOk for non-conflicting branches" $ withTestRepo \dir -> do
            createBranch dir "feat-a" [("file-a.txt", "new file a\n")]
            createBranch dir "feat-b" [("file-b.txt", "new file b\n")]
            result <- gitMergeTree dir "origin/feat-a" "origin/feat-b"
            case result of
                MergeOk _ -> pure ()
                MergeConflict files ->
                    expectationFailure $ "Expected clean merge, got conflict in: " <> show files

        it "returns MergeConflict for conflicting branches" $ withTestRepo \dir -> do
            createBranch dir "change-x" [("file.txt", "XXX\nline2\nline3\nline4\nline5\n")]
            createBranch dir "change-y" [("file.txt", "YYY\nline2\nline3\nline4\nline5\n")]
            result <- gitMergeTree dir "origin/change-x" "origin/change-y"
            case result of
                MergeConflict files -> files `shouldNotBe` []
                MergeOk _ -> expectationFailure "Expected conflict, got clean merge"

        it "returns MergeOk when merging branch against base" $ withTestRepo \dir -> do
            createBranch dir "feat" [("file.txt", "modified\nline2\nline3\nline4\nline5\n")]
            result <- gitMergeTree dir "origin/main" "origin/feat"
            case result of
                MergeOk _ -> pure ()
                MergeConflict _ -> expectationFailure "Expected clean merge against base"

    describe "partitionBase" do
        it "classifies clean PRs as valid" $ withTestRepo \dir -> withLog \lf -> do
            createBranch dir "clean" [("new-file.txt", "content\n")]
            let
                pr = mkPR 1 "clean"
            result <- runRIO lf $ partitionBase dir "main" (pr :| [])
            case result of
                That good -> length good `shouldBe` 1
                other -> expectationFailure $ "Expected That, got: " <> show other

        it "classifies conflicting PRs as base conflicts" $ withTestRepo \dir -> withLog \lf -> do
            createBranch
                dir
                "old-branch"
                [("file.txt", "BRANCH\nline2\nline3\nline4\nline5\n")]
            git dir ["checkout", "main"]
            writeFile (dir </> "file.txt") "MAIN\nline2\nline3\nline4\nline5\n"
            git dir ["add", "file.txt"]
            git dir ["commit", "-m", "move main"]
            git dir ["update-ref", "refs/remotes/origin/main", "refs/heads/main"]
            let
                pr = mkPR 1 "old-branch"
            result <- runRIO lf $ partitionBase dir "main" (pr :| [])
            case result of
                This bad -> length bad `shouldBe` 1
                other -> expectationFailure $ "Expected This, got: " <> show other

    describe "findConflicts" do
        it "finds no conflicts between independent branches" $ withTestRepo \dir -> withLog \lf -> do
            createBranch dir "ind-a" [("file-a.txt", "aaa\n")]
            createBranch dir "ind-b" [("file-b.txt", "bbb\n")]
            let
                prs = mkPR 1 "ind-a" :| [mkPR 2 "ind-b"]
            conflicts <- runRIO lf $ findConflicts dir "main" prs
            conflicts `shouldBe` Nothing

        it "detects a pairwise conflict" $ withTestRepo \dir -> withLog \lf -> do
            createBranch dir "cx" [("file.txt", "XXX\nline2\nline3\nline4\nline5\n")]
            createBranch dir "cy" [("file.txt", "YYY\nline2\nline3\nline4\nline5\n")]
            let
                prs = mkPR 1 "cx" :| [mkPR 2 "cy"]
            conflicts <- runRIO lf $ findConflicts dir "main" prs
            case conflicts of
                Just (cp :| []) -> do
                    cp.cpLeft `shouldBe` 1
                    cp.cpRight `shouldBe` 2
                _ -> expectationFailure $ "Expected Just (1 conflict), got " <> show conflicts

        it "detects multiple conflicts in a triangle" $ withTestRepo \dir -> withLog \lf -> do
            createBranch dir "ta" [("file.txt", "AAA\nline2\nline3\nline4\nline5\n")]
            createBranch dir "tb" [("file.txt", "BBB\nline2\nline3\nline4\nline5\n")]
            createBranch dir "tc" [("file.txt", "CCC\nline2\nline3\nline4\nline5\n")]
            let
                prs = mkPR 1 "ta" :| [mkPR 2 "tb", mkPR 3 "tc"]
            conflicts <- runRIO lf $ findConflicts dir "main" prs
            fmap length conflicts `shouldBe` Just 3

        it "only flags the conflicting pairs, not clean ones" $ withTestRepo \dir -> withLog \lf -> do
            createBranch dir "pa" [("file.txt", "AAA\nline2\nline3\nline4\nline5\n")]
            createBranch dir "pb" [("file.txt", "BBB\nline2\nline3\nline4\nline5\n")]
            createBranch dir "pc" [("other.txt", "new content\n")]
            let
                prs = mkPR 1 "pa" :| [mkPR 2 "pb", mkPR 3 "pc"]
            conflicts <- runRIO lf $ findConflicts dir "main" prs
            case conflicts of
                Just (cp :| []) -> do
                    cp.cpLeft `shouldBe` 1
                    cp.cpRight `shouldBe` 2
                _ -> expectationFailure $ "Expected Just (1 conflict), got " <> show conflicts

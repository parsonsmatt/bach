module Bach.FugueSpec (spec) where

import Bach.Fugue (validateBatch)
import Bach.Prelude
import Bach.Types
import Test.Hspec
import TestHelpers

spec :: Spec
spec = describe "validateBatch" do
    it "passes a batch of non-conflicting PRs unchanged" $ withTestRepo \dir -> withLog \lf -> do
        createBranch dir "va" [("file-a.txt", "aaa\n")]
        createBranch dir "vb" [("file-b.txt", "bbb\n")]
        (ready, evicted) <-
            runRIO lf $ validateBatch dir "main" [mkPR 1 "va", mkPR 2 "vb"]
        length ready `shouldBe` 2
        evicted `shouldBe` []

    it "evicts a conflicting PR" $ withTestRepo \dir -> withLog \lf -> do
        createBranch dir "ea" [("file.txt", "AAA\nline2\nline3\nline4\nline5\n")]
        createBranch dir "eb" [("file.txt", "BBB\nline2\nline3\nline4\nline5\n")]
        (ready, evicted) <-
            runRIO lf $ validateBatch dir "main" [mkPR 1 "ea", mkPR 2 "eb"]
        length ready `shouldBe` 1
        length evicted `shouldBe` 1

    it "keeps first PR and evicts later conflicting ones" $ withTestRepo \dir -> withLog \lf -> do
        createBranch dir "fa" [("file.txt", "AAA\nline2\nline3\nline4\nline5\n")]
        createBranch dir "fb" [("file.txt", "BBB\nline2\nline3\nline4\nline5\n")]
        createBranch dir "fc" [("file.txt", "CCC\nline2\nline3\nline4\nline5\n")]
        (ready, evicted) <-
            runRIO lf $ validateBatch dir "main" [mkPR 1 "fa", mkPR 2 "fb", mkPR 3 "fc"]
        length ready `shouldBe` 1
        map (\pr -> pr.prNumber) ready `shouldBe` ([1] :: [Int])
        length evicted `shouldBe` 2

    it "keeps non-conflicting PRs even after an eviction" $ withTestRepo \dir -> withLog \lf -> do
        createBranch dir "ga" [("file.txt", "AAA\nline2\nline3\nline4\nline5\n")]
        createBranch dir "gb" [("file.txt", "BBB\nline2\nline3\nline4\nline5\n")]
        createBranch dir "gc" [("other.txt", "new content\n")]
        (ready, evicted) <-
            runRIO lf $ validateBatch dir "main" [mkPR 1 "ga", mkPR 2 "gb", mkPR 3 "gc"]
        -- ga merges clean, gb conflicts with ga so evicted, gc is independent so clean
        length ready `shouldBe` 2
        map (\pr -> pr.prNumber) ready `shouldBe` ([1, 3] :: [Int])
        length evicted `shouldBe` 1
        map (\pr -> pr.prNumber) evicted `shouldBe` ([2] :: [Int])

module Bach.BatchingSpec (spec) where

import Bach.Batching (buildBatches)
import Bach.Conflicts (nPairs)
import Bach.Prelude
import qualified RIO.Map as Map
import qualified RIO.Set as Set
import Test.Hspec
import TestHelpers (mkPR)

spec :: Spec
spec = do
    describe "nPairs" do
        it "0 items = 0 pairs" $ nPairs 0 `shouldBe` 0
        it "1 item = 0 pairs" $ nPairs 1 `shouldBe` 0
        it "2 items = 1 pair" $ nPairs 2 `shouldBe` 1
        it "3 items = 3 pairs" $ nPairs 3 `shouldBe` 3
        it "10 items = 45 pairs" $ nPairs 10 `shouldBe` 45

    describe "buildBatches" do
        let
            prA = mkPR 1 "a"
            prB = mkPR 2 "b"
            prC = mkPR 3 "c"
            prD = mkPR 4 "d"

        it "puts all PRs in batch 1 with no conflicts" do
            let
                batches = buildBatches [prA, prB, prC] Set.empty
            Map.size batches `shouldBe` 1
            Map.lookup 1 batches `shouldBe` Just [prA, prB, prC]

        it "separates a conflicting pair into 2 batches" do
            let
                conflicts = Set.singleton (1, 2)
                batches = buildBatches [prA, prB] conflicts
            Map.size batches `shouldBe` 2
            Map.lookup 1 batches `shouldBe` Just [prA]
            Map.lookup 2 batches `shouldBe` Just [prB]

        it "handles a triangle (all three pairwise conflict)" do
            let
                conflicts = Set.fromList [(1, 2), (1, 3), (2, 3)]
                batches = buildBatches [prA, prB, prC] conflicts
            Map.size batches `shouldBe` 3

        it "handles a chain: A-B conflict, B-C conflict, A-C clean" do
            let
                conflicts = Set.fromList [(1, 2), (2, 3)]
                batches = buildBatches [prA, prB, prC] conflicts
            Map.size batches `shouldBe` 2
            Map.lookup 1 batches `shouldBe` Just [prA, prC]
            Map.lookup 2 batches `shouldBe` Just [prB]

        it "handles two independent conflict pairs" do
            let
                conflicts = Set.fromList [(1, 2), (3, 4)]
                batches = buildBatches [prA, prB, prC, prD] conflicts
            Map.size batches `shouldBe` 2
            Map.lookup 1 batches `shouldBe` Just [prA, prC]
            Map.lookup 2 batches `shouldBe` Just [prB, prD]

        it "returns empty for empty input" do
            let
                batches = buildBatches [] Set.empty
            Map.size batches `shouldBe` 0

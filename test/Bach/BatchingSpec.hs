module Bach.BatchingSpec (spec) where

import Bach.Batching (NoBatchEligible (..), buildBatches, selectBatch)
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

    describe "selectBatch" do
        let
            prA = mkPR 1 "a"
            prB = mkPR 2 "b"
            prC = mkPR 3 "c"
            prD = mkPR 4 "d"
            prE = mkPR 5 "e"
            prF = mkPR 6 "f"
            prG = mkPR 7 "g"

        it "picks the largest batch with no must-include" do
            -- A conflicts with D,E,F,G. Batch 1 = [A,B,C], batch 2 = [D,E,F,G]
            -- Largest is batch 2 (4 PRs > 3 PRs)
            let
                conflicts = Set.fromList [(1, 4), (1, 5), (1, 6), (1, 7)]
                batches = buildBatches [prA, prB, prC, prD, prE, prF, prG] conflicts
            selectBatch Set.empty batches
                `shouldBe` Right ([prD, prE, prF, prG], [prA, prB, prC])

        it "picks the largest batch containing must-include PR" do
            -- Same setup. With must-include A, only batch 1 is eligible.
            let
                conflicts = Set.fromList [(1, 4), (1, 5), (1, 6), (1, 7)]
                batches = buildBatches [prA, prB, prC, prD, prE, prF, prG] conflicts
            selectBatch (Set.singleton 1) batches
                `shouldBe` Right ([prA, prB, prC], [prD, prE, prF, prG])

        it "picks batch 1 when it is the largest" do
            -- A-B conflict. Batch 1 = [A,C,D], batch 2 = [B].
            let
                conflicts = Set.singleton (1, 2)
                batches = buildBatches [prA, prB, prC, prD] conflicts
            fmap fst (selectBatch Set.empty batches)
                `shouldBe` Right [prA, prC, prD]

        it "errors when must-include PRs are in different batches" do
            let
                conflicts = Set.singleton (1, 2)
                batches = buildBatches [prA, prB] conflicts
            selectBatch (Set.fromList [1, 2]) batches
                `shouldBe` Left (NoBatchEligible (Set.fromList [1, 2]))

        it "returns empty for empty batches" do
            selectBatch Set.empty Map.empty
                `shouldBe` Right ([], [])

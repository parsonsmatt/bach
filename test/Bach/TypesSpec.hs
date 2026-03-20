module Bach.TypesSpec (spec) where

import Bach.Git (parseConflictFiles)
import Bach.Prelude
import Bach.Types
import Test.Hspec

spec :: Spec
spec = do
    describe "parsePRIdentifier" do
        it "parses integers as PRById"
            $ parsePRIdentifier "123"
            `shouldBe` PRById 123

        it "parses text as PRByBranch"
            $ parsePRIdentifier "my-feature"
            `shouldBe` PRByBranch "my-feature"

        it "parses zero as PRById"
            $ parsePRIdentifier "0"
            `shouldBe` PRById 0

        it "parses negative as PRById (readMaybe parses it)"
            $ parsePRIdentifier "-5"
            `shouldBe` PRById (-5)

        it "parses branch with slashes"
            $ parsePRIdentifier "matt/my-feature"
            `shouldBe` PRByBranch "matt/my-feature"

    describe "parseConflictFiles" do
        it "extracts a single conflict path"
            $ parseConflictFiles "some header\nMerge conflict in foo.txt\ntrailer"
            `shouldBe` ["foo.txt"]

        it "extracts multiple conflict paths"
            $ parseConflictFiles "Merge conflict in a.hs\nstuff\nMerge conflict in b.hs\n"
            `shouldBe` ["a.hs", "b.hs"]

        it "returns empty for no conflicts"
            $ parseConflictFiles "Already up to date.\n"
            `shouldBe` []

        it "returns empty for empty input"
            $ parseConflictFiles ""
            `shouldBe` []

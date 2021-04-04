module Terpod.EpisodeSpec (spec) where

import qualified Hedgehog.Gen        as Gen
import qualified Hedgehog.Range      as Range
import           Prelude
import           Terpod.Episode      (sanitise)
import           Test.Hspec
import           Test.Hspec.Hedgehog (forAll, hedgehog, (===))

spec :: Spec
spec = describe "Terpod.Episode" $ do
  describe "sanitise" $ do
    it "produces nothing other than lowercase chars, numbers, and hyphens" $ hedgehog $ do
      str <- forAll $ Gen.list (Range.linear 1 99) Gen.ascii
      all (flip elem $ ['a'..'z'] <> ['0'..'9'] <> ['-']) (sanitise str) === True


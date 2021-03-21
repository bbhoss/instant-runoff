import Test.Hspec
import Control.Exception (evaluate)
import Lib

main :: IO ()
main = hspec $ do
  describe "Lib.tally" $ do
    it "returns an empty result when no ballots were cast" $ do
      tally [] `shouldBe` []
    it "returns the candidates with their first round vote tallies" $ do
      tally simpleMajorityPreston `shouldBe` [(preston, 3), (hannah, 1)]

preston = Candidate "Preston"
hannah = Candidate "Hannah"
simpleMajorityPreston =
  [
    Ballot [preston],
    Ballot [preston],
    Ballot [preston],
    Ballot [hannah]
  ]
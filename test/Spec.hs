import Test.Hspec
import Control.Exception (evaluate)
import Lib

main :: IO ()
main = hspec $ do
  describe "Lib.tally" $ do
    it "returns an empty result when no ballots were cast" $ do
      tally [] `shouldBe` []
    it "returns the candidates with their first round vote tallies" $ do
      tally simpleMajorityPreston `shouldBe` [(preston, [3]), (hannah, [1])]
    it "redistributes votes from losing candidates when no candidate has a majority" $ do
      tally instantRunoffPreston `shouldBe` [(preston, [4]), (hannah, [3]), (ruby, [1])]

preston = Candidate "Preston"
hannah = Candidate "Hannah"
ruby = Candidate "Ruby"
simpleMajorityPreston =
  [
    Ballot [preston],
    Ballot [preston],
    Ballot [preston],
    Ballot [hannah]
  ]

instantRunoffPreston =
  [
    Ballot [preston],
    Ballot [preston],
    Ballot [preston],
    Ballot [hannah],
    Ballot [hannah],
    Ballot [hannah],
    Ballot [ruby, preston]
  ]
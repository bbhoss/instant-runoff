module Lib
    (
        Ballot,
        Candidate,
        tally
    ) where

newtype Candidate = Candidate {name :: String} deriving (Show)
newtype Ballot = Ballot { votes :: [Candidate] }
type Election = [Ballot]
type ElectionResult = [(Candidate, Int)]

tally :: Election -> ElectionResult
tally e =
    -- TODO: Count all the ballots
    [(Candidate "Preston", 100)]
-- someFunc :: IO ()
-- someFunc = putStrLn "someFunc"

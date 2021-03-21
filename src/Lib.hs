module Lib
  (
    Ballot(..),
    Candidate(..),
    tally,
  ) where

import Data.List
import Data.Ord

newtype Candidate = Candidate {name :: String} deriving (Show, Eq)
newtype Ballot = Ballot { votes :: [Candidate] }
type Election = [Ballot]
type CandidateTally = (Candidate, Int)
type ElectionResult = [CandidateTally]

tally :: Election -> ElectionResult
tally e =
  sortOn (Data.Ord.Down . voteCount) talliedResults
  where
    talliedResults = countBallots e []

countBallots :: Election -> ElectionResult -> ElectionResult
countBallots [] runningTally = runningTally
countBallots (Ballot{votes=(firstCandidateVote:_)}:ballots) runningTally =
  case findCandidateResult firstCandidateVote runningTally of
    Nothing -> countBallots ballots ((firstCandidateVote, 1):filteredRunningTally)
    Just (firstCandidateVote, votes) -> countBallots ballots ((firstCandidateVote, votes+1):filteredRunningTally)
    where
      filteredRunningTally = filter (not . isCandidateTally firstCandidateVote) runningTally


findCandidateResult :: Candidate -> ElectionResult -> Maybe CandidateTally
findCandidateResult candidate = find (isCandidateTally candidate)

isCandidateTally :: Candidate -> CandidateTally -> Bool
isCandidateTally soughtCandidate (candidate, _) = soughtCandidate == candidate

voteCount :: CandidateTally -> Int
voteCount (_, count) = count
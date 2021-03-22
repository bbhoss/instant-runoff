module Lib
  (
    Ballot(..),
    Candidate(..),
    tally,
  ) where

import Data.List
import Data.Ord
import Debug.Trace

newtype Candidate = Candidate {name :: String} deriving (Show, Eq)
newtype Ballot = Ballot { votes :: [Candidate] }
type Election = [Ballot]
type CandidateTally = (Candidate, [Int])
type ElectionResult = [CandidateTally]

tally :: Election -> ElectionResult
tally [] = []
tally e =
  case sortedResults of
    results | leaderVotes >= requiredForMajorityCount -> results
    results -> trace ("IRV condition detected"++ show results) results
  where
    talliedResults = countBallots e []
    sortedResults = sortOn (Data.Ord.Down . latestRoundVoteCount) talliedResults
    totalVotes = sum (map latestRoundVoteCount sortedResults)
    requiredForMajorityCount = ceiling (fromIntegral totalVotes * 0.5)
    leaderVotes = latestRoundVoteCount (head sortedResults)


countBallots :: Election -> ElectionResult -> ElectionResult
countBallots [] runningTally = runningTally
countBallots (Ballot{votes=(firstCandidateVote:_)}:ballots) runningTally =
  case findCandidateResult firstCandidateVote runningTally of
    Nothing -> countBallots ballots ((firstCandidateVote, [1]):filteredRunningTally)
    Just (firstCandidateVote, latestRoundVotes:priorRounds) -> countBallots ballots ((firstCandidateVote, latestRoundVotes+1:priorRounds):filteredRunningTally)
    where
      filteredRunningTally = filter (not . isCandidateTally firstCandidateVote) runningTally


findCandidateResult :: Candidate -> ElectionResult -> Maybe CandidateTally
findCandidateResult candidate = find (isCandidateTally candidate)

isCandidateTally :: Candidate -> CandidateTally -> Bool
isCandidateTally soughtCandidate (candidate, _) = soughtCandidate == candidate

latestRoundVoteCount :: CandidateTally -> Int
latestRoundVoteCount (_, count:_) = count
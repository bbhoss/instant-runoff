module Lib
  (
    Ballot(..),
    Candidate(..),
    Winner(..),
    tally,
  ) where

import Data.List
import Data.Ord
import Debug.Trace

newtype Candidate = Candidate {name :: String} deriving (Show, Eq)
newtype Ballot = Ballot { votes :: [Candidate] }
data Winner = Winner { candidate :: Candidate } | Tie deriving (Show, Eq)
type Election = [Ballot]
type CandidateTally = (Candidate, [Int])
type ElectionRounds = [CandidateTally]
type FinalElectionResult = (Winner, ElectionRounds)

tally :: Election -> FinalElectionResult
tally [] = (Tie, [])
tally e = tallyRound e []

tallyRound :: Election -> ElectionRounds -> FinalElectionResult
tallyRound e priorRounds =
  case sortedResults of
    results | leaderVotes >= requiredForMajorityCount -> (Winner {candidate=leader}, results)
    results -> tallyRound redistributedBallots results
    -- results | length losers == 0 -> --  If there are two continuing candidates, the candidate with the most votes shall be declared the nominee of his or her party for a primary election, or elected winner for an election for which nominations were made by independent nominating petitions.
    -- results 
    -- results -> trace ("IRV condition detected"++ show results) results
  where
    talliedResults = countBallots e []
    sortedResults = sortOn (Data.Ord.Down . latestRoundVoteCount) talliedResults
    totalVotes = sum (map latestRoundVoteCount sortedResults)
    requiredForMajorityCount = ceiling (fromIntegral totalVotes * 0.5)
    (leader,_) = head sortedResults
    leaderVotes = latestRoundVoteCount (head sortedResults)
    ((worstLoser,_):losers) = reverse (tail sortedResults)
    worstLoserBallots = filter (isVoteForCandidate worstLoser) e
    worstLoserRemovedBallots = filter (not . isVoteForCandidate worstLoser) e
    loserEliminatedBallots = eliminateLoserVotes worstLoser worstLoserBallots
    redistributedBallots = loserEliminatedBallots ++ worstLoserRemovedBallots



countBallots :: Election -> ElectionRounds -> ElectionRounds
countBallots [] runningTally = runningTally
countBallots (Ballot{votes=(firstCandidateVote:_)}:ballots) runningTally =
  case findCandidateResult firstCandidateVote runningTally of
    Nothing -> countBallots ballots ((firstCandidateVote, [1]):filteredRunningTally)
    Just (firstCandidateVote, latestRoundVotes:priorRounds) -> countBallots ballots ((firstCandidateVote, latestRoundVotes+1:priorRounds):filteredRunningTally)
    where
      filteredRunningTally = filter (not . isCandidateTally firstCandidateVote) runningTally


findCandidateResult :: Candidate -> ElectionRounds -> Maybe CandidateTally
findCandidateResult candidate = find (isCandidateTally candidate)

isCandidateTally :: Candidate -> CandidateTally -> Bool
isCandidateTally soughtCandidate (candidate, _) = soughtCandidate == candidate

latestRoundVoteCount :: CandidateTally -> Int
latestRoundVoteCount (_, count:_) = count

isVoteForCandidate :: Candidate -> Ballot -> Bool
isVoteForCandidate candidate ballot = head (votes ballot) == candidate

eliminateLoserVotes :: Candidate -> [Ballot] -> [Ballot]
eliminateLoserVotes loser loserBallots =
  nullBallotsRemoved
  where
    loserRemoved = map eliminateHighestVote loserBallots
    nullBallotsRemoved = filter (not . null . votes) loserRemoved

eliminateHighestVote :: Ballot -> Ballot
eliminateHighestVote Ballot{votes=(_:remaining)} = Ballot{votes=remaining}
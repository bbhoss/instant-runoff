module Lib
  (
    Ballot(..),
    Candidate(..),
    Winner(..),
    tally,
  ) where

import Data.List
import Data.Ord

newtype Candidate = Candidate {name :: String} deriving (Show, Eq)
newtype Ballot = Ballot { votes :: [Candidate] }
data Winner = Winner { candidate :: Candidate } | Tie deriving (Show, Eq) -- TODO: implement n-way Tie
type Election = [Ballot]
type CandidateTally = (Candidate, Int)
type ElectionRound = [CandidateTally]
type FinalElectionResult = (Winner, [ElectionRound])

tally :: Election -> FinalElectionResult
tally [] = (Tie, [])
tally e =
  case (leader, second) of
    ((leaderCd, leaderVotes), (sndCd, sndVotes)) | leaderVotes == sndVotes -> (Tie, completedTabulation)
    ((leaderCd, _), _) -> (Winner{candidate=leaderCd}, completedTabulation)
  where
    completedTabulation = tallyRound e []
    finalRoundResults = head completedTabulation
    (leader:(second:_)) = finalRoundResults

-- (Winner {candidate=leader}, 
tallyRound :: [Ballot] -> [ElectionRound] -> [ElectionRound]
tallyRound e priorRounds =
  case sortedResults of
    results | leaderVotes >= requiredForMajorityCount -> results:priorRounds
    results -> tallyRound redistributedBallots (results:priorRounds)
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



countBallots :: Election -> ElectionRound -> ElectionRound
countBallots [] runningTally = runningTally
countBallots (Ballot{votes=(firstCandidateVote:_)}:ballots) runningTally =
  case findCandidateResult firstCandidateVote runningTally of
    Nothing -> countBallots ballots ((firstCandidateVote, 1):filteredRunningTally)
    Just (firstCandidateVote, votes) -> countBallots ballots ((firstCandidateVote, votes+1):filteredRunningTally)
    where
      filteredRunningTally = filter (not . isCandidateTally firstCandidateVote) runningTally


findCandidateResult :: Candidate -> ElectionRound -> Maybe CandidateTally
findCandidateResult candidate = find (isCandidateTally candidate)

isCandidateTally :: Candidate -> CandidateTally -> Bool
isCandidateTally soughtCandidate (candidate, _) = soughtCandidate == candidate

latestRoundVoteCount :: CandidateTally -> Int
latestRoundVoteCount (_, count) = count

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
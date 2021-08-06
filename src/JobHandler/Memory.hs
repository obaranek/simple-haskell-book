-- |

module JobHandler.Memory where

import RIO
import Core

import qualified JobHandler
import qualified RIO.Map as Map
import qualified Control.Concurrent.STM as STM
import qualified RIO.List as List
import qualified Agent

createService :: IO JobHandler.Service
createService = do
  state <- STM.newTVarIO State
    { jobs = mempty
    , logs = mempty
    , nextBuild = 1
    }

  pure JobHandler.Service
    { queueJob = \pipeline ->  STM.atomically do
        STM.stateTVar state $ queueJob_ pipeline
      , findJob = \number -> STM.atomically do
          s <- STM.readTVar state
          pure $ findJob_ number s
      , dispatchCmd = STM.atomically do
          STM.stateTVar state dispatchCmd_

      , processMsg = \msg -> STM.atomically do
          STM.modifyTVar' state $ processMsg_ msg
    }

findJob_ :: BuildNumber -> State -> Maybe JobHandler.Job
findJob_ number state =
  Map.lookup number state.jobs

data State
  = State
        { jobs :: Map BuildNumber JobHandler.Job
          ,logs :: Map (BuildNumber, StepName) ByteString
          ,nextBuild :: Int
        }
  deriving (Eq, Show)

queueJob_ :: Pipeline -> State -> (BuildNumber, State)
queueJob_ pipeline state =
  (number, updatedState)
  where
    number = BuildNumber state.nextBuild
    job = JobHandler.Job
        { pipeline = pipeline
        , state = JobHandler.JobQueued
        }
    updatedState =
      state
      { jobs = Map.insert number job state.jobs
      , nextBuild = state.nextBuild + 1
      }

dispatchCmd_ :: State -> (Maybe Agent.Cmd, State)
dispatchCmd_ state =
  case List.find queued $ Map.toList state.jobs of
    Just (number, job) ->
      let updatedJob = job{state = JobHandler.JobAssigned}
          updatedState = Map.insert number updatedJob state.jobs
          cmd = Just $ Agent.StartBuild number job.pipeline
      in (cmd, state{jobs = updatedState})

    _ -> (Nothing, state)
  where
    queued (_, job) = job.state == JobHandler.JobQueued

processMsg_ :: Agent.Msg -> State -> State
processMsg_ msg state = case msg of
  Agent.BuildUpdated number build ->
    let f job = job{state = JobHandler.JobScheduled build}
    in state{jobs = Map.adjust f number state.jobs}

  Agent.LogCollected number log ->
    let updatedLogs
          = Map.insertWith (flip mappend) (number, log.step) log.output state.logs
    in state{logs = updatedLogs}
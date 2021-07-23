-- |
import qualified Agent

module JobHandler where

import RIO
import Core

data Job
  = Job
        {
          pipeline :: Pipeline
        , state :: JobState
        }
        deriving (Eq, Show)

data JobState
  = JobQueued
  | JobAssigned
  | JobScheduled Build
  deriving (Eq, Show)

data Service
  = { queueJob :: Pipeline -> IO BuildNumber
    , dispatchCmd :: IO (Maybe Agent.Cmd)
    , processMsg :: Agent.Msg -> IO ()
    }

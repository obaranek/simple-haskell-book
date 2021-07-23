-- |

module Agent where

import RIO
import Core

data Cmd
  = StartBuild BuildNumber Pipeline
  deriving (Eq, Show)

data Msg = LogCollected BuildNumber Log
         | BuildUpdated BuildNumber Build
         deriving (Eq, Show)

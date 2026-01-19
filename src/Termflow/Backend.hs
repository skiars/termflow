module Termflow.Backend
  ( Backend,
  )
where

import Control.Concurrent.STM
import Termflow.Types

-- | A backend consumes events from the queue and renders them.
type Backend = TQueue FlowEvent -> IO ()

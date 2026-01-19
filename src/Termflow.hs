module Termflow
  ( module Termflow.Class,
    module Termflow.Core,
    module Termflow.Format,
    Backend,
    ansiBackend,
    plainBackend,
    detectBackend,
    runFlowWith,
    runFlow,
  )
where

import Control.Concurrent (forkIO, killThread)
import Control.Concurrent.STM
import Control.Exception (finally)
import Control.Monad (unless)
import Control.Monad.IO.Class (MonadIO (..))
import System.IO (hIsTerminalDevice, stdout)
import Termflow.Backend (Backend)
import Termflow.Backend.ANSI (ansiBackend)
import Termflow.Backend.Plain (plainBackend)
import Termflow.Class
import Termflow.Core
import Termflow.Format

-- | Detect the appropriate backend for the current environment.
detectBackend :: IO Backend
detectBackend = do
  isTerm <- hIsTerminalDevice stdout
  return $ if isTerm then ansiBackend else plainBackend

-- | Execute the Termflow monad with a specific backend.
runFlowWith :: Backend -> FlowT IO a -> IO a
runFlowWith backend flow = do
  q <- liftIO newTQueueIO

  -- Fork the backend renderer
  tid <- liftIO $ forkIO $ backend q

  -- Run the flow logic
  runFlowT q flow `finally` do
    -- Wait for the queue to drain before killing the backend
    atomically $ do
      empty <- isEmptyTQueue q
      unless empty retry
    -- A small grace period or just kill
    liftIO $ killThread tid

-- | Execute the Termflow monad.
-- Automatically detects the best backend for the current environment.
runFlow :: FlowT IO a -> IO a
runFlow flow = do
  backend <- detectBackend
  runFlowWith backend flow

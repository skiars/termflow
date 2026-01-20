module Termflow
  ( MonadFlow, -- dont expose methods directly
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
runFlowWith :: MonadIO m => Backend -> FlowT m a -> m a
runFlowWith backend flow = do
  q <- liftIO newTQueueIO

  -- Fork the backend renderer
  tid <- liftIO $ forkIO $ backend q
  -- Run the flow logic
  result <- runFlowT q flow
  -- Wait for the queue to drain before killing the backend
  liftIO $ atomically $ do
    empty <- isEmptyTQueue q
    unless empty retry
  -- A small grace period or just kill
  liftIO $ killThread tid

  return result

-- | Execute the Termflow monad.
-- Automatically detects the best backend for the current environment.
runFlow :: MonadIO m => FlowT m a -> m a
runFlow flow = do
  backend <- liftIO detectBackend
  runFlowWith backend flow

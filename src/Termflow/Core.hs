{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE ImpredicativeTypes #-}
{-# OPTIONS_GHC -Wno-redundant-constraints #-}

module Termflow.Core
  ( FlowT (..),
    RunFlowIO,
    MonadUnliftIO,
    runFlowT,
    withRunFlow,
    askRunFlow,
    info,
    warn,
    stream,
    progress,
    group,
    group',
    step,
    step',
    setMessage,
  )
where

import Control.Applicative (Alternative)
import Control.Concurrent.STM
import Control.Monad (MonadPlus)
import Control.Monad.Error.Class (MonadError)
import Control.Monad.Fail ()
import Control.Monad.Fix (MonadFix)
import Control.Monad.IO.Class
import Control.Monad.IO.Unlift (MonadUnliftIO, withRunInIO, askRunInIO)
import Control.Monad.Reader
import Control.Monad.State.Class (MonadState)
import Control.Monad.Writer.Class (MonadWriter)
import Termflow.Class
import Termflow.Types
import Termflow.Format (RichText)

-- | The Flow Transformer
newtype FlowT m a = FlowT {unFlowT :: ReaderT (TQueue FlowEvent) m a}
  deriving
    ( Functor,
      Applicative,
      Alternative,
      Monad,
      MonadFail,
      MonadFix,
      MonadIO,
      MonadPlus,
      MonadTrans,
      MonadState s,
      MonadWriter w,
      MonadError e,
      MonadUnliftIO
    )

instance (MonadReader r m) => MonadReader r (FlowT m) where
  ask = lift ask
  local f (FlowT m) = FlowT (mapReaderT (local f) m)

instance (MonadIO m) => MonadFlow (FlowT m) where
  emit ev = do
    q <- FlowT ask
    liftIO $ atomically $ writeTQueue q ev

-- | A type alias for a function that can run FlowT actions in IO.
type RunFlowIO m = forall a. m a -> IO a

-- | Run the FlowT monad
runFlowT :: TQueue FlowEvent -> FlowT m a -> m a
runFlowT q (FlowT m) = runReaderT m q

-- | Capture the current flow runner to execute flow actions within the underlying monad.
withRunFlow ::
  (MonadUnliftIO m, MonadFlow m) => (RunFlowIO m -> IO b) -> m b
withRunFlow = withRunInIO

-- | Capture the current flow runner to execute flow actions within the underlying monad.
askRunFlow ::
  (MonadUnliftIO m, MonadFlow m) => m (m a -> IO a)
askRunFlow = askRunInIO

-- | Emit an informational log message.
info :: MonadFlow m => RichText -> m ()
info t = emit (EvLog t)

-- | Emit a warning message.
warn :: MonadFlow m => RichText -> m ()
warn t = emit (EvWarn t)

-- | Output a log line from a stream (e.g. process output).
-- This is usually rendered without prefix and may be scrolled aggressively.
stream :: MonadFlow m => RichText -> m ()
stream t = emit (EvStream t)

-- | Output a transient progress line.
-- If the last output was also a progress line, it will be overwritten.
progress :: MonadFlow m => RichText -> m ()
progress t = emit (EvProgress t)

-- | Update the current message (e.g. in a step or group).
setMessage :: MonadFlow m => RichText -> m ()
setMessage t = emit (EvUpdateMessage t)

-- | Start a new group of log messages.
group :: MonadFlow m => RichText -> m a -> m a
group title action = do
  emit (EvGroupStart GtGroup title Nothing)
  res <- action
  emit EvGroupEnd
  return res

-- | Start a new group of log messages with a custom tag.
group' :: MonadFlow m => RichText -> RichText -> m a -> m a
group' tag title action = do
  emit (EvGroupStart GtGroup title (Just tag))
  res <- action
  emit EvGroupEnd
  return res

-- | Start a new step in the flow, just an alias for 'group'.
step :: MonadFlow m => RichText -> m a -> m a
step title action = do
  emit (EvGroupStart GtStep title Nothing)
  res <- action
  emit EvGroupEnd
  return res

-- | Start a new step in the flow with a custom tag.
step' :: MonadFlow m => RichText -> RichText -> m a -> m a
step' tag title action = do
  emit (EvGroupStart GtStep title (Just tag))
  res <- action
  emit EvGroupEnd
  return res

{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE UndecidableInstances #-}
{-# OPTIONS_GHC -Wno-redundant-constraints #-}

module Termflow.Core
  ( FlowT (..),
    RunFlowIO,
    MonadUnliftIO,
    runFlowT,
    withRunFlow,
  )
where

import Control.Applicative (Alternative)
import Control.Concurrent.STM
import Control.Monad (MonadPlus)
import Control.Monad.Error.Class (MonadError)
import Control.Monad.Fail ()
import Control.Monad.Fix (MonadFix)
import Control.Monad.IO.Class
import Control.Monad.IO.Unlift (MonadUnliftIO, withRunInIO)
import Control.Monad.Reader
import Control.Monad.State.Class (MonadState)
import Control.Monad.Writer.Class (MonadWriter)
import Termflow.Class
import Termflow.Types

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

-- | A type alias for a function that can run FlowT actions in IO.
type RunFlowIO m = forall a. m a -> IO a

-- | Run the FlowT monad
runFlowT :: TQueue FlowEvent -> FlowT m a -> m a
runFlowT q (FlowT m) = runReaderT m q

-- | Capture the current flow runner to execute flow actions within the underlying monad.
withRunFlow ::
  (MonadUnliftIO m, MonadFlow m) => (RunFlowIO m -> IO b) -> m b
withRunFlow = withRunInIO

-- | Helper to emit an event
emit :: (MonadIO m) => FlowEvent -> FlowT m ()
emit ev = do
  q <- FlowT ask
  liftIO $ atomically $ writeTQueue q ev

instance (MonadIO m) => MonadFlow (FlowT m) where
  info t = emit (EvLog t)

  warn t = emit (EvWarn t)

  stream t = emit (EvStream t)

  progress t = emit (EvProgress t)

  group title action = do
    emit (EvGroupStart title)
    res <- action
    emit EvGroupEnd
    return res

  step = group -- Steps are just groups aliased

  setMessage t = emit (EvUpdateMessage t)

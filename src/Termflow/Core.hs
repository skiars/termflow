{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE UndecidableInstances #-}

module Termflow.Core
  ( FlowT (..),
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
      MonadError e
    )

instance (MonadReader r m) => MonadReader r (FlowT m) where
  ask = lift ask
  local f (FlowT m) = FlowT (mapReaderT (local f) m)

-- | Run the FlowT monad
runFlowT :: TQueue FlowEvent -> FlowT m a -> m a
runFlowT q (FlowT m) = runReaderT m q

-- | Capture the current flow runner to execute flow actions within the underlying monad.
withRunFlow :: (Monad m) => ((forall a. FlowT m a -> m a) -> m b) -> FlowT m b
withRunFlow inner = do
  q <- FlowT ask
  lift $ inner (runFlowT q)

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

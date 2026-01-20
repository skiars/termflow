{-# LANGUAGE RankNTypes #-}
module Test where

import Control.Monad.Reader
import Control.Monad.IO.Class
import Control.Concurrent.STM

newtype FlowT m a = FlowT {unFlowT :: ReaderT (TQueue ()) m a}
  deriving (Functor, Applicative, Monad, MonadIO)

instance MonadTrans FlowT where
    lift = FlowT . lift

runFlowT :: TQueue () -> FlowT m a -> m a
runFlowT q (FlowT m) = runReaderT m q

-- Specialized for m ~ IO, only using MonadIO (implied by usage of IO)
withRunFlowIO :: ((forall a. FlowT IO a -> IO a) -> IO b) -> FlowT IO b
withRunFlowIO inner = FlowT $ ReaderT $ \q -> inner (\action -> runReaderT (unFlowT action) q)

-- Generic m, trying to use only MonadIO?
-- withRunFlowGeneric :: MonadIO m => ((forall a. FlowT m a -> IO a) -> IO b) -> FlowT m b
-- withRunFlowGeneric inner = FlowT $ ReaderT $ \q -> do
--    -- We are in m. We need to run 'inner'. 'inner' returns 'IO b'.
--    -- inner needs a run function: FlowT m a -> IO a.
--    -- We have access to q. So we can convert FlowT m a -> m a.
--    -- But we CANNOT convert m a -> IO a with just MonadIO.
--    undefined

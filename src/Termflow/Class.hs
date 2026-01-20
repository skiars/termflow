module Termflow.Class
  ( MonadFlow (..),
  )
where

import Termflow.Format (RichText)

import Control.Monad.Except as E
import Control.Monad.Reader (ReaderT, mapReaderT)
import Control.Monad.Writer.Strict (WriterT (..), runWriterT)
import Control.Monad.State.Strict (StateT (..), runStateT)
import Control.Monad.Trans.Class (lift)
import Control.Monad.Trans.Identity (IdentityT (..), mapIdentityT)
import Control.Monad.Trans.Maybe (MaybeT (..), mapMaybeT)
import qualified Control.Monad.State.Lazy as SL
import qualified Control.Monad.Writer.Lazy as WL
import qualified Control.Monad.RWS.Strict as RWSS
import qualified Control.Monad.RWS.Lazy as RWSL

class (Monad m) => MonadFlow m where
  info :: RichText -> m ()
  warn :: RichText -> m ()

  -- | Output a log line from a stream (e.g. process output).
  -- This is usually rendered without prefix and may be scrolled aggressively.
  stream :: RichText -> m ()

  -- | Output a transient progress line.
  -- If the last output was also a progress line, it will be overwritten.
  progress :: RichText -> m ()

  group :: RichText -> m a -> m a
  step :: RichText -> m a -> m a

  setMessage :: RichText -> m ()

instance MonadFlow m => MonadFlow (StateT s m) where
  info = lift . info
  warn = lift . warn
  stream = lift . stream
  progress = lift . progress
  setMessage = lift . setMessage
  group t m = StateT $ \s -> group t (runStateT m s)
  step t m = StateT $ \s -> step t (runStateT m s)

instance MonadFlow m => MonadFlow (ReaderT r m) where
  info = lift . info
  warn = lift . warn
  stream = lift . stream
  progress = lift . progress
  setMessage = lift . setMessage
  group t m = mapReaderT (group t) m
  step t m = mapReaderT (step t) m

instance (MonadFlow m, MonadError e m) => MonadFlow (E.ExceptT e m) where
  info = lift . info
  warn = lift . warn
  stream = lift . stream
  progress = lift . progress
  setMessage = lift . setMessage
  group t m = E.ExceptT $ do
    r <- group t (E.runExceptT m)
    return r
  step t m = E.ExceptT $ do
    r <- step t (E.runExceptT m)
    return r

instance (Monoid w, MonadFlow m) => MonadFlow (WriterT w m) where
  info = lift . info
  warn = lift . warn
  stream = lift . stream
  progress = lift . progress
  setMessage = lift . setMessage
  group t m = WriterT $ do
    (a, w) <- group t (runWriterT m)
    return (a, w)
  step t m = WriterT $ do
    (a, w) <- step t (runWriterT m)
    return (a, w)

instance MonadFlow m => MonadFlow (IdentityT m) where
  info = lift . info
  warn = lift . warn
  stream = lift . stream
  progress = lift . progress
  setMessage = lift . setMessage
  group t m = mapIdentityT (group t) m
  step t m = mapIdentityT (step t) m

instance MonadFlow m => MonadFlow (MaybeT m) where
  info = lift . info
  warn = lift . warn
  stream = lift . stream
  progress = lift . progress
  setMessage = lift . setMessage
  group t m = mapMaybeT (group t) m
  step t m = mapMaybeT (step t) m

instance MonadFlow m => MonadFlow (SL.StateT s m) where
  info = lift . info
  warn = lift . warn
  stream = lift . stream
  progress = lift . progress
  setMessage = lift . setMessage
  group t m = SL.StateT $ \s -> group t (SL.runStateT m s)
  step t m = SL.StateT $ \s -> step t (SL.runStateT m s)

instance (Monoid w, MonadFlow m) => MonadFlow (WL.WriterT w m) where
  info = lift . info
  warn = lift . warn
  stream = lift . stream
  progress = lift . progress
  setMessage = lift . setMessage
  group t m = WL.WriterT $ do
    (a, w) <- group t (WL.runWriterT m)
    return (a, w)
  step t m = WL.WriterT $ do
    (a, w) <- step t (WL.runWriterT m)
    return (a, w)

instance (Monoid w, MonadFlow m) => MonadFlow (RWSS.RWST r w s m) where
  info = lift . info
  warn = lift . warn
  stream = lift . stream
  progress = lift . progress
  setMessage = lift . setMessage
  group t m = RWSS.RWST $ \r s -> group t (RWSS.runRWST m r s)
  step t m = RWSS.RWST $ \r s -> step t (RWSS.runRWST m r s)

instance (Monoid w, MonadFlow m) => MonadFlow (RWSL.RWST r w s m) where
  info = lift . info
  warn = lift . warn
  stream = lift . stream
  progress = lift . progress
  setMessage = lift . setMessage
  group t m = RWSL.RWST $ \r s -> group t (RWSL.runRWST m r s)
  step t m = RWSL.RWST $ \r s -> step t (RWSL.runRWST m r s)

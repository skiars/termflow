module Termflow.Class
  ( MonadFlow (..) )
where

import Termflow.Types (FlowEvent)

import Control.Monad.Except as E
import Control.Monad.Reader (ReaderT)
import Control.Monad.Writer.Strict (WriterT)
import Control.Monad.State.Strict (StateT)
import Control.Monad.Trans.Class (lift)
import Control.Monad.Trans.Identity (IdentityT)
import Control.Monad.Trans.Maybe (MaybeT)
import qualified Control.Monad.State.Lazy as SL
import qualified Control.Monad.Writer.Lazy as WL
import qualified Control.Monad.RWS.Strict as RWSS
import qualified Control.Monad.RWS.Lazy as RWSL

class (Monad m) => MonadFlow m where
  emit :: FlowEvent -> m ()

instance MonadFlow m => MonadFlow (StateT s m) where
  emit = lift . emit

instance MonadFlow m => MonadFlow (ReaderT r m) where
  emit = lift . emit

instance (MonadFlow m, MonadError e m) => MonadFlow (E.ExceptT e m) where
  emit = lift . emit

instance (Monoid w, MonadFlow m) => MonadFlow (WriterT w m) where
  emit = lift . emit

instance MonadFlow m => MonadFlow (IdentityT m) where
  emit = lift . emit

instance MonadFlow m => MonadFlow (MaybeT m) where
  emit = lift . emit

instance MonadFlow m => MonadFlow (SL.StateT s m) where
  emit = lift . emit

instance (Monoid w, MonadFlow m) => MonadFlow (WL.WriterT w m) where
  emit = lift . emit

instance (Monoid w, MonadFlow m) => MonadFlow (RWSS.RWST r w s m) where
  emit = lift . emit

instance (Monoid w, MonadFlow m) => MonadFlow (RWSL.RWST r w s m) where
  emit = lift . emit

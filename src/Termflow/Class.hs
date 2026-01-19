module Termflow.Class
  ( MonadFlow (..),
  )
where

import Termflow.Format (RichText)

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

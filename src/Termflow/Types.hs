{-# LANGUAGE OverloadedStrings #-}

module Termflow.Types
  ( FlowEvent (..),
  )
where

import Termflow.Format (RichText)

-- | The core events that the backend needs to handle.
data FlowEvent
  = EvLog RichText
  | EvStream RichText
  | EvProgress RichText
  | EvWarn RichText
  | EvGroupStart RichText
  | EvGroupEnd
  | EvUpdateMessage RichText
  deriving (Show, Eq)

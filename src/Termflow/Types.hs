{-# LANGUAGE OverloadedStrings #-}

module Termflow.Types
  ( FlowEvent (..),
    GroupType (..),
  )
where

import Termflow.Format (RichText)

-- | The type of the group
data GroupType
  = GtGroup
  | GtStep
  deriving (Show, Eq)

-- | The core events that the backend needs to handle.
data FlowEvent
  = EvLog RichText
  | EvStream RichText
  | EvProgress RichText
  | EvWarn RichText
  | EvGroupStart GroupType RichText (Maybe RichText)
  | EvGroupEnd
  | EvUpdateMessage RichText
  deriving (Show, Eq)

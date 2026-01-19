{-# LANGUAGE OverloadedStrings #-}

module Termflow.Backend.Plain
  ( plainBackend,
  )
where

import Control.Concurrent.STM
import qualified Data.Text as T
import qualified Data.Text.IO as TIO
import Termflow.Backend
import Termflow.Format (RichText (..), Segment (..))
import Termflow.Types

plainBackend :: Backend
plainBackend q = loop
  where
    loop = do
      ev <- atomically $ readTQueue q
      case ev of
        EvLog t -> do
          TIO.putStrLn (stripStyles t)
          loop
        EvStream t -> do
          TIO.putStrLn (stripStyles t)
          loop
        EvProgress _ -> do
          -- Skip progress in plain backend to reduce noise
          loop
        EvWarn t -> do
          TIO.putStrLn $ "[WARN] " <> stripStyles t
          loop
        EvGroupStart t -> do
          TIO.putStrLn $ ">> " <> stripStyles t
          loop
        EvGroupEnd -> do
          loop
        EvUpdateMessage t -> do
          TIO.putStrLn $ "   ... " <> stripStyles t
          loop

stripStyles :: RichText -> T.Text
stripStyles (RichText segs) = T.concat $ map segText segs

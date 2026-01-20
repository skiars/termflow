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

      let process rt action = case rt of
            Placeholder -> loop
            _           -> action >> loop

      case ev of
        EvLog t           -> process t $ TIO.putStrLn (stripStyles t)
        EvStream t        -> process t $ TIO.putStrLn (stripStyles t)
        EvProgress _      -> loop
        EvWarn t          -> process t $ TIO.putStrLn $ "[WARN] " <> stripStyles t
        EvGroupStart _ t mTag ->
          let prefix = case mTag of
                Nothing -> ">> "
                Just tag -> stripStyles tag <> " "
           in process t $ TIO.putStrLn $ prefix <> stripStyles t
        EvGroupEnd        -> loop
        EvUpdateMessage t -> process t $ TIO.putStrLn $ "   ... " <> stripStyles t

stripStyles :: RichText -> T.Text
stripStyles (RichText segs) = T.concat $ map segText segs
stripStyles Placeholder = ""

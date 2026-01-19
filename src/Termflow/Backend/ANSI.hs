{-# LANGUAGE OverloadedStrings #-}

module Termflow.Backend.ANSI
  ( ansiBackend,
  )
where

import Control.Concurrent.STM
import Control.Monad (forM_, forever, when)
import Data.IORef
import qualified Data.Text.IO as TIO
import System.Console.ANSI
import System.IO (hFlush, stdout)
import Termflow.Backend
import Termflow.Format
import Termflow.Types

-- | Maximum number of lines a group can occupy (including header)
maxGroupHeight :: Int
maxGroupHeight = 10

data GroupState = GroupState
  { -- | Number of lines occupied by this group (including header)
    gsLines :: Int,
    -- | The current title of the group
    gsTitle :: RichText,
    -- | Buffer of currently visible log lines
    gsBuffer :: [RichText],
    -- | Whether the last line added was a transient progress line
    gsLastWasProgress :: Bool,
    -- | Accumulated warnings that should be flushed to history upon group exit
    gsWarnings :: [RichText]
  }

newtype RenderState = RenderState
  { -- | Stack of active groups
    rsStack :: [GroupState]
  }

ansiBackend :: Backend
ansiBackend q = do
  stateRef <- newIORef (RenderState [])
  forever $ do
    ev <- atomically $ readTQueue q
    processEvent stateRef ev
    hFlush stdout

processEvent :: IORef RenderState -> FlowEvent -> IO ()
processEvent ref ev = do
  st <- readIORef ref
  let stack = rsStack st

  case ev of
    EvLog msg -> addToGroup ref msg False
    EvStream msg -> addToGroup ref msg False
    EvProgress msg -> addToGroup ref msg True
    EvWarn msg -> do
      case stack of
        (_ : _) -> do
          addToGroup ref msg False
          modifyIORef' ref $ \state ->
            case rsStack state of
              (newG : newRest) -> state {rsStack = newG {gsWarnings = gsWarnings newG ++ [msg]} : newRest}
              [] -> state
        [] -> putRichLn msg
    EvGroupStart title -> do
      putRichLn $ "[-] " <> title
      modifyIORef' ref $ \state -> state {rsStack = GroupState 1 title [] False [] : rsStack state}
    EvUpdateMessage msg -> do
      case stack of
        (g@(GroupState count _ _ _ _) : rest) -> do
          cursorUpLine count
          clearLine
          putRichLn $ "[-] " <> msg
          when (count > 1) $ cursorDownLine (count - 1)
          modifyIORef' ref $ \state -> state {rsStack = g {gsTitle = msg} : rest}
        [] -> putRichLn $ "[WARN] setMessage called without active group: " <> msg
    EvGroupEnd -> do
      case stack of
        (GroupState count title _ _ warnings : rest) -> do
          cursorUpLine count
          clearFromCursorToScreenEnd

          if null warnings
            then do
              putRichLn $ "[+] " <> title
              modifyIORef' ref $ \state -> state {rsStack = incrementHead rest}
            else do
              let headerLine = "[-] " <> title
              let blockLines = headerLine : warnings
              mapM_ putRichLn blockLines

              case rest of
                (parent : grandparents) -> do
                  let newBuffer = gsBuffer parent ++ blockLines
                  let newWarnings = gsWarnings parent ++ blockLines
                  let newParent =
                        parent
                          { gsLines = gsLines parent + length blockLines,
                            gsBuffer = newBuffer,
                            gsWarnings = newWarnings
                          }

                  modifyIORef' ref $ \state -> state {rsStack = newParent : grandparents}
                [] -> do
                  modifyIORef' ref $ \state -> state {rsStack = []}
        [] -> return ()

addToGroup :: IORef RenderState -> RichText -> Bool -> IO ()
addToGroup ref msg isProgress = do
  st <- readIORef ref
  case rsStack st of
    (g@(GroupState count _ buffer lastWasProgress _) : rest) -> do
      if isProgress && lastWasProgress && not (null buffer)
        then do
          cursorUpLine 1
          clearLine
          putRichLn msg
          let newBuffer = init buffer ++ [msg]
          let newG = g {gsBuffer = newBuffer, gsLastWasProgress = True}
          modifyIORef' ref $ \state -> state {rsStack = newG : rest}
        else
          if count < maxGroupHeight
            then do
              putRichLn msg
              let newG = g {gsLines = count + 1, gsBuffer = buffer ++ [msg], gsLastWasProgress = isProgress}
              modifyIORef' ref $ \state -> state {rsStack = newG : rest}
            else do
              cursorUpLine (count - 1)
              clearFromCursorToScreenEnd
              let logCapacity = maxGroupHeight - 1
              let newBuffer = takeEnd logCapacity (buffer ++ [msg])
              mapM_ putRichLn newBuffer
              let newG = g {gsBuffer = newBuffer, gsLastWasProgress = isProgress}
              modifyIORef' ref $ \state -> state {rsStack = newG : rest}
    [] -> putRichLn msg

incrementHead :: [GroupState] -> [GroupState]
incrementHead [] = []
incrementHead (g : gs) = g {gsLines = gsLines g + 1} : gs

takeEnd :: Int -> [a] -> [a]
takeEnd n xs
  | length xs <= n = xs
  | otherwise = drop (length xs - n) xs

putRich :: RichText -> IO ()
putRich (RichText segs) = do
  forM_ segs $ \(Segment sty t) -> do
    setSGR (styleToSGR sty)
    TIO.putStr t
  setSGR [Reset]

putRichLn :: RichText -> IO ()
putRichLn rt = putRich rt >> TIO.putStrLn ""

styleToSGR :: Style -> [SGR]
styleToSGR sty =
  [ Reset,
    SetConsoleIntensity (if sBold sty then BoldIntensity else NormalIntensity),
    SetItalicized (sItalic sty),
    SetUnderlining (if sUnderline sty then SingleUnderline else NoUnderline)
  ]
    ++ colorSGR (sColor sty)

colorSGR :: Maybe (ColorIntensity, Color) -> [SGR]
colorSGR Nothing = []
colorSGR (Just (i, c)) = [SetColor Foreground i c]

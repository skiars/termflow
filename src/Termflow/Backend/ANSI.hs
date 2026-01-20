{-# LANGUAGE OverloadedStrings #-}
{-# OPTIONS_GHC -Wno-unused-top-binds #-}

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
    gsWarnings :: [RichText],
    -- | Optional custom tag for the group header
    gsTag :: Maybe RichText,
    -- | The type of the group
    gsType :: GroupType
  }

data RenderState = RenderState
  { -- | Stack of active groups
    rsStack :: [GroupState],
    -- | Whether the last line output at the top level was a transient progress line
    rsLastWasProgress :: Bool
  }

ansiBackend :: Backend
ansiBackend q = do
  stateRef <- newIORef (RenderState [] False)
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

    EvGroupStart gType title mTag -> do
      clearLastLineIfProgress ref

      let header = if title == Placeholder then Placeholder else formatHeader gType "[-] " mTag title
      putRichLn header
      let initialLines = if title == Placeholder then 0 else 1
      modifyIORef' ref $ \state -> state {rsStack = GroupState initialLines title [] False [] mTag gType : rsStack state, rsLastWasProgress = False}

    EvUpdateMessage msg -> do
      case stack of
        (g@(GroupState count title _ _ _ mTag gType) : rest) -> do
          let oldVisible = title /= Placeholder
              newVisible = msg /= Placeholder

          if oldVisible == newVisible
            then do
              when oldVisible $ do
                cursorUpLine count
                clearLine
                putRichLn $ formatHeader gType "[-] " mTag msg
                when (count > 1) $ cursorDownLine (count - 1)
              modifyIORef' ref $ \state -> state {rsStack = g {gsTitle = msg} : rest}
            else if oldVisible
              then do
                -- Visible -> Hidden
                cursorUpLine count
                -- TIO.putStr "\ESC[1M"
                when (count > 1) $ cursorDownLine (count - 1)
                modifyIORef' ref $ \state -> state {rsStack = g {gsTitle = msg, gsLines = count - 1} : rest}
              else do
                -- Hidden -> Visible
                cursorUpLine count
                -- TIO.putStr "\ESC[1L"
                putRichLn $ formatHeader gType "[-] " mTag msg
                when (count > 0) $ cursorDownLine count
                modifyIORef' ref $ \state -> state {rsStack = g {gsTitle = msg, gsLines = count + 1} : rest}

        [] -> putRichLn $ "[WARN] setMessage called without active group: " <> msg

    EvGroupEnd -> do
      case stack of
        (GroupState count title _ _ warnings mTag gType : rest) -> do
          cursorUpLine count
          clearFromCursorToScreenEnd

          if null warnings
            then do
              let isProgress = case gType of
                    GtGroup -> False
                    GtStep -> True
                  lineDiff = if title == Placeholder then 0 else 1
              putRichLn $ formatHeader gType "[+] " mTag title
              case rest of
                [] -> modifyIORef' ref $ \state -> state {rsStack = [], rsLastWasProgress = isProgress}
                _ -> modifyIORef' ref $ \state -> state {rsStack = updateParent rest isProgress lineDiff}
            else do
              let headerLine = formatHeader gType "[+] " mTag title
              let blockLines = headerLine : warnings
              mapM_ putRichLn blockLines

              case rest of
                (parent : grandparents) -> do
                  let newBuffer = gsBuffer parent ++ blockLines
                  let newWarnings = gsWarnings parent ++ blockLines
                  let newParent =
                        parent
                          { gsLines = gsLines parent + blockLineCount blockLines,
                            gsBuffer = newBuffer,
                            gsWarnings = newWarnings
                          }

                  modifyIORef' ref $ \state -> state {rsStack = newParent : grandparents}
                [] -> do
                  modifyIORef' ref $ \state -> state {rsStack = [], rsLastWasProgress = False}
        [] -> return ()

addToGroup :: IORef RenderState -> RichText -> Bool -> IO ()
addToGroup ref msg isProgress = do
  st <- readIORef ref
  case rsStack st of
    (GroupState _ _ _ lastWasProgress _ _ _ : _) -> do
      if isProgress && lastWasProgress
        then clearLastLineIfProgress ref
        else return ()
      appendLineToGroup ref msg isProgress
    [] -> do
      when (rsLastWasProgress st) $ clearLastLineIfProgress ref
      putRichLn msg
      modifyIORef' ref $ \state -> state {rsLastWasProgress = isProgress}

clearLastLineIfProgress :: IORef RenderState -> IO ()
clearLastLineIfProgress ref = do
  st <- readIORef ref
  case rsStack st of
    (g : rest) -> when (gsLastWasProgress g) $ do
      cursorUpLine 1
      clearLine
      let newBuffer = if null (gsBuffer g) then [] else init (gsBuffer g)
      let newG = g {gsBuffer = newBuffer, gsLines = gsLines g - 1, gsLastWasProgress = False}
      modifyIORef' ref $ \state -> state {rsStack = newG : rest}
    [] -> when (rsLastWasProgress st) $ do
      cursorUpLine 1
      clearLine
      modifyIORef' ref $ \state -> state {rsLastWasProgress = False}

appendLineToGroup :: IORef RenderState -> RichText -> Bool -> IO ()
appendLineToGroup ref msg isProgress = do
  st <- readIORef ref
  case rsStack st of
    (g@(GroupState count _ buffer _ _ _ _) : rest) -> do
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
    _ -> return ()

updateParent :: [GroupState] -> Bool -> Int -> [GroupState]
updateParent [] _ _ = []
updateParent (g : gs) isProgress lineDiff = g {
    gsLines = gsLines g + lineDiff, gsLastWasProgress = isProgress
  } : gs

blockLineCount :: [RichText] -> Int
blockLineCount = length . filter (/= Placeholder)

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
putRich Placeholder = return ()

putRichLn :: RichText -> IO ()
putRichLn Placeholder = return ()
putRichLn rt = putRich rt >> TIO.putStrLn ""

styleToSGR :: Style -> [SGR]
styleToSGR sty =
  [ Reset,
    SetConsoleIntensity
      ( if sBold sty
          then BoldIntensity
          else
            if sFaint sty
              then FaintIntensity
              else NormalIntensity
      ),
    SetItalicized (sItalic sty),
    SetUnderlining (if sUnderline sty then SingleUnderline else NoUnderline)
  ] <> colorSGR (sColor sty)

colorSGR :: Maybe TermColor -> [SGR]
colorSGR Nothing = []
colorSGR (Just (TermColorBasic i c)) = [SetColor Foreground i c]
colorSGR (Just (TermColor256 idx)) = [SetPaletteColor Foreground idx]

formatHeader :: GroupType -> RichText -> Maybe RichText -> RichText -> RichText
formatHeader gType defTag mTag title =
  let tagged = case gType of
        GtGroup -> faint defTag <> title
        GtStep  -> title
  in  case mTag of
        Nothing  -> tagged
        Just tag -> tag <> " " <> title

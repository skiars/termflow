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
      case stack of
        [] -> do
          RenderState _ lastWasProgress <- readIORef ref
          when lastWasProgress $ do
            cursorUpLine 1
            clearLine
        _ -> return ()

      putRichLn $ formatHeader gType "[-] " mTag title
      modifyIORef' ref $ \state -> state {rsStack = GroupState 1 title [] False [] mTag gType : rsStack state, rsLastWasProgress = False}
    EvUpdateMessage msg -> do
      case stack of
        (g@(GroupState count _ _ _ _ mTag gType) : rest) -> do
          cursorUpLine count
          clearLine
          putRichLn $ formatHeader gType "[-] " mTag msg
          when (count > 1) $ cursorDownLine (count - 1)
          modifyIORef' ref $ \state -> state {rsStack = g {gsTitle = msg} : rest}
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
              putRichLn $ formatHeader gType "[+] " mTag title
              case rest of
                [] -> modifyIORef' ref $ \state -> state {rsStack = [], rsLastWasProgress = isProgress}
                _ -> modifyIORef' ref $ \state -> state {rsStack = updateParent rest isProgress}
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
                          { gsLines = gsLines parent + length blockLines,
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
    (g@(GroupState count _ buffer lastWasProgress _ _ _) : rest) -> do
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
    [] -> do
      RenderState _ lastWasProgress <- readIORef ref
      when lastWasProgress $ do
        cursorUpLine 1
        clearLine
      putRichLn msg
      modifyIORef' ref $ \state -> state {rsLastWasProgress = isProgress}

updateParent :: [GroupState] -> Bool -> [GroupState]
updateParent [] _ = []
updateParent (g : gs) isProgress = g {gsLines = gsLines g + 1, gsLastWasProgress = isProgress} : gs

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
  let tagged = case mTag of
        Nothing  -> faint defTag <> title
        Just tag -> tag <> " " <> title
  in  case gType of
        GtGroup -> tagged
        GtStep  -> title

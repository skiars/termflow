{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RankNTypes #-}

module Main (main) where

import Control.Concurrent (forkIO, newEmptyMVar, putMVar, takeMVar, threadDelay)
import Control.Monad (forM_, replicateM, void, when)
import Control.Monad.IO.Class (liftIO)
import Data.String (IsString (..))
import System.Exit (ExitCode (..))
import System.IO (BufferMode (NoBuffering), hGetChar, hIsEOF, hSetBuffering)
import System.Process (StdStream (CreatePipe), createProcess, proc, std_err, std_out, waitForProcess)
import System.Environment (getArgs)
import Termflow

main :: IO ()
main = do
  args <- getArgs
  backend <-
    case args of
      ["--plain"] -> pure plainBackend
      ["--ansi"]  -> pure ansiBackend
      _           -> detectBackend
  runFlowWith backend app

app :: FlowT IO ()
app = do
  info $ green "Initializing Termflow..."
  compileProject

setupEnvironment :: FlowT IO ()
setupEnvironment = do
  info $ "Creating " <> blue "directories" <> "..."
  liftIO $ threadDelay 500000
  group (cyan "Downloading dependencies") downloadDependencies

downloadDependencies :: FlowT IO ()
downloadDependencies = do
  liftIO $ threadDelay 500000
  info $ yellow "Dependencies downloading..."
  downloadCoreLib
  downloadExtraLib
  compileProject
  gitClone
  realGitClone
  info $ green "Download complete"
  liftIO $ threadDelay 500000

downloadCoreLib :: FlowT IO ()
downloadCoreLib = step "Downloading core-lib..." $ repeatN 10 $ \p -> do
  setMessage $ "Progress: " <> disp ((p + 1) * 10) <> "%"
  void $ liftIO $ threadDelay 100000

downloadExtraLib :: FlowT IO ()
downloadExtraLib = step "Downloading extra-lib..." $ do
  liftIO $ threadDelay 500000
  -- Demonstrate persistent warning (should bubble up)
  warn $ red "extra-lib is deprecated!"
  liftIO $ threadDelay 500000
  info $ cyan "Download continuing..."
  liftIO $ threadDelay 500000

compileProject :: FlowT IO ()
compileProject = step "Compiling massive project (Parallel Build)" $ do
  withRunFlow $ \run -> liftIO $ do
    mvars <- replicateM 4 newEmptyMVar
    forM_ (zip [1 :: Int .. 8] mvars) $ \(tid, mvar) -> forkIO $ do
      -- Simulate work for this thread
      forM_ [1 .. 250 :: Int] $ \i -> do
        run $ step ("Thread " <> disp tid <> ": Compiling Module " <> bold (disp i)) $ do
          liftIO $ 
            threadDelay $ 10000 + (tid * 1000) -- stagger timing slightly
        when (tid == 2 && i == 10) $
          run $ warn $ yellow $ "Thread " <> disp tid <> ": Warning: Module " <> disp i <> " has unused imports."
      run $ info $ green $ "Thread " <> disp tid <> " finished."
      putMVar mvar ()

    -- Wait for all threads
    mapM_ takeMVar mvars
  -- forM_ [1 .. 100 :: Int] $ \i -> do
  --   liftIO $ threadDelay 50000
  --   step ("Compiling Module " <> disp i) $ pure ()

gitClone :: FlowT IO ()
gitClone = step "Git Clone (Stream)" $ do
  stream $ "Cloning into " <> cyan "'termflow'" <> "..."
  liftIO $ threadDelay 500000
  stream "remote: Enumerating objects: 120, done."
  liftIO $ threadDelay 200000
  stream "remote: Counting objects: 100% (120/120), done."
  liftIO $ threadDelay 200000
  stream "remote: Compressing objects: 100% (80/80), done."
  liftIO $ threadDelay 200000

  repeatN 50 $ \i -> do
    let pct = (i + 1) * 2
        kb = (i + 1) * 64
        speed = if i < 25 then "1.20" else "2.40"
    progress $
      fromString $
        "Receiving objects: "
          <> align 3 (show pct)
          <> "%"
          <> " ("
          <> show (i + 1)
          <> "/50), "
          <> align 4 (show kb)
          <> " KiB"
          <> " | "
          <> speed
          <> " MiB/s"
    liftIO $ threadDelay 50000

  stream "Receiving objects: 100% (50/50), 3.20 MiB | 2.40 MiB/s, done."
  liftIO $ threadDelay 200000
  stream "Resolving deltas: 100% (45/45), done."

realGitClone :: FlowT IO ()
realGitClone = step "Real Git Clone (commercialhaskell/stack)" $ do
  let repo = "https://github.com/commercialhaskell/stack.git"
      dir = ".temp"

  -- Clean up previous run & start git clone
  (_, _, Just herr, ph) <- liftIO $ do
    (_, _, _, phRm) <- createProcess (proc "rm" ["-rf", dir])
    _ <- waitForProcess phRm
    createProcess
      (proc "git" ["clone", "--progress", repo, dir])
        { std_out = CreatePipe,
          std_err = CreatePipe
        }

  liftIO $ hSetBuffering herr NoBuffering

  -- Simple loop to read characters and buffer them until newline or carriage return
  let loop buffer = do
        isEof <- liftIO $ hIsEOF herr
        if isEof
          then return ()
          else do
            c <- liftIO $ hGetChar herr
            if c == '\n'
              then do
                stream (fromString (reverse buffer))
                loop ""
              else
                if c == '\r'
                  then do
                    progress (fromString (reverse buffer))
                    loop ""
                  else loop (c : buffer)

  loop ""

  exitCode <- liftIO $ waitForProcess ph
  case exitCode of
    ExitSuccess -> info $ green "Clone successful!"
    ExitFailure c -> warn $ red $ "Clone failed with code: " <> disp c

-- | Demonstrate calling back into Flow from IO
demonstrateCallbacks :: FlowT IO ()
demonstrateCallbacks = step "Demonstrating IO Callbacks" $ do
  info "Calling an external IO function that calls back into Flow..."
  withRunFlow $ \run ->
    liftIO $ simulatedExternalLibrary run
  info "Callback demonstration complete."

simulatedExternalLibrary :: MonadFlow m => RunFlowIO m -> IO ()
simulatedExternalLibrary runner = do
  threadDelay 500000
  runner $ info $ blue "  [Callback] " <> "Inside IO, talking to Termflow!"
  threadDelay 500000
  runner $ warn $ yellow "  [Callback] " <> "Even warnings work from here."
  threadDelay 500000

repeatN :: (Monad m) => Int -> (Int -> m ()) -> m ()
repeatN n = forM_ [0 .. n - 1]

align :: Int -> String -> String
align n str = replicate (n - length str) ' ' ++ str

disp :: Int -> RichText
disp = fromString . show

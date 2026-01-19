# Termflow

**A Haskell library for structural, interactive, and collapsible CLI applications.**

Termflow transforms your command-line output from a linear stream of text into a structured, real-time interface. Inspired by the logging experience of modern frontend tools, it treats logs as an "event stream" that can be grouped, collapsed, and updated in place.

![asciinema demo](img/demo.gif)

## Features

- **📂 Collapsible Log Streams**: Automatically collapses completed task groups while keeping the current activity in focus.
- **🌲 Structured Grouping**: Organize logs into hierarchical groups (e.g., "Setup Environment", "Downloading").
- **✨ Rich Styling**: Built-in support for colors, bold text, and string composition.
- **🔄 Dynamic Progress**: Support for transient progress messages that don't clutter history.
- **🔌 Backend Agnostic**:
  - **Terminal**: Uses ANSI escape codes for folding and in-place updates on TTYs.
  - **Plain Text**: Automatically degrades to linear output for pipes and CI environments.

## Installation

Add `termflow` to your `package.yaml` or `.cabal` file dependencies:

```yaml
dependencies:
  - termflow
```

## Usage

Here represents a simple example showing how to create a hierarchical flow with Termflow:

```haskell
{-# LANGUAGE OverloadedStrings #-}

module Main where

import Control.Concurrent (threadDelay)
import Control.Monad.IO.Class (liftIO)
import Termflow

main :: IO ()
main = runFlow $ do
  info $ green "Initializing Termflow..."

  group (bold "Setup Environment") $ do
    info "Creating directories..."
    liftIO $ threadDelay 500000 -- Simulate work

    group (cyan "Downloading dependencies") $ do
      info "Fetching core-lib..."
      liftIO $ threadDelay 500000
      info "Fetching extra-lib..."

    info "Setup complete."

  info $ bold $ green "Done!"
```

### Running the demo

To see Termflow in action in this repository:

```bash
stack build
stack exec termflow-exe
```

## Roadmap

- [ ] **Interactive TUI Components**: Support for user input is currently in the design phase.
  - [ ] Select (Single item selection)
  - [ ] MultiSelect (Multiple item selection)
  - [ ] Text Input (String input with validation)
  - [ ] Confirm (Yes/No prompts)
- [ ] **Progress Bars**: Support for determinate progress bars (e.g., download percentage).

## License

This project is licensed under the BSD-3-Clause License.

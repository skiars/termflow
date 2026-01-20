{-# LANGUAGE FlexibleInstances #-}

module Termflow.Format
  ( RichText (..),
    Segment (..),
    Style (..),
    TermColor (..),
    Renderable (..),
    defaultStyle,
    plain,
    placeholder,

    -- * Style Helpers
    bold,
    faint,
    italic,
    underline,
    red,
    green,
    yellow,
    blue,
    magenta,
    cyan,
    white,
    black,
    gray,

    -- * Generic Style Applier
    style,
    color,
    palette,
  )
where

import Data.String (IsString (..))
import Data.Text (Text)
import qualified Data.Text as T
import Data.Word (Word8)
import System.Console.ANSI

-- | A piece of text with style.
data Segment = Segment
  { segStyle :: Style,
    segText :: Text
  }
  deriving (Show, Eq)

-- | 'RichText' is a lightweight, structured representation of text intended for
-- rendering with styling. Conceptually it is either:
--
--   * Concrete rich text, represented as a list of 'Segment's ('RichText [Segment]'), or
--   * A 'Placeholder' value that denotes “not yet available / unresolved” content.
--
-- The 'Placeholder' constructor is deliberately *absorbing* under concatenation:
-- once a 'Placeholder' is present, the combined result is 'Placeholder'. This
-- models the idea that unresolved content taints the whole rich-text value and
-- cannot be “fixed” by appending more segments.
--
-- Instances:
--
--   * 'Semigroup': concatenation appends segment lists. 'RichText []' behaves as
--     the identity element for concatenation. 'Placeholder' is an absorbing
--     element: @Placeholder <> x == Placeholder@ and @x <> Placeholder == Placeholder@.
--
-- Typical usage:
--
--   * Use 'raw' (or 'plain') to construct concrete text with the default style.
--   * Use 'placeholder' to create a marker that downstream consumers may treat
--     as “to be filled in later”. Note that on the Haskell side, concatenation
--     does not eliminate placeholders; they must be replaced explicitly.
data RichText
  = RichText [Segment]
  | Placeholder
  deriving (Show, Eq)

instance Semigroup RichText where
  Placeholder <> _           = Placeholder
  _           <> Placeholder = Placeholder
  RichText [] <> b = b
  a <> RichText [] = a
  RichText as <> RichText bs = RichText (as <> bs)

instance Monoid RichText where
  -- | The identity element for 'RichText' concatenation is the empty text.
  mempty = RichText []

instance IsString RichText where
  fromString s = raw (T.pack s)

-- | Terminal Color definition
data TermColor
  = TermColor256 Word8
  | TermColorBasic ColorIntensity Color
  deriving (Show, Eq)

-- | Style definition
data Style = Style
  { sBold :: Bool,
    sFaint :: Bool,
    sItalic :: Bool,
    sUnderline :: Bool,
    sColor :: Maybe TermColor
  }
  deriving (Show, Eq)

defaultStyle :: Style
defaultStyle =
  Style
    { sBold = False,
      sFaint = False,
      sItalic = False,
      sUnderline = False,
      sColor = Nothing
    }

-- | Construct plain (un-styled) rich text from a 'Text' value.
--
-- This is currently an alias of 'raw' and uses 'defaultStyle' for the single
-- produced segment.
plain :: Text -> RichText
plain = raw

-- | Construct a 'Placeholder' rich-text value.
--
-- Intended meaning: the backend may ignore placeholders until they are replaced
-- by concrete rich text. On the Haskell side, placeholders are not removable via
-- '(<>)' composition; they must be replaced explicitly by the caller.
placeholder :: RichText
placeholder = Placeholder

-- | Construct a concrete 'RichText' value consisting of a single 'Segment'
-- containing the provided 'Text' and the 'defaultStyle'.
raw :: Text -> RichText
raw t = RichText [Segment defaultStyle t]

-- | Update all segments in the RichText with a function applied to their style.
applyStyle :: (Style -> Style) -> RichText -> RichText
applyStyle f (RichText segs) = RichText $ map modify segs where
  modify (Segment st t) = Segment (f st) t
applyStyle _ Placeholder = Placeholder

-- | Set bold
bold :: RichText -> RichText
bold = applyStyle (\st -> st {sBold = True})

italic :: RichText -> RichText
italic = applyStyle (\st -> st {sItalic = True})

underline :: RichText -> RichText
underline = applyStyle (\st -> st {sUnderline = True})

faint :: RichText -> RichText
faint = applyStyle (\st -> st {sFaint = True})

-- | Helper to set color
color :: ColorIntensity -> Color -> RichText -> RichText
color i c = applyStyle (\st -> st {sColor = Just (TermColorBasic i c)})

-- | Helper to set palette color
palette :: Word8 -> RichText -> RichText
palette i = applyStyle (\st -> st {sColor = Just (TermColor256 i)})

-- | Standard colors (Vivid by default usually looks better on dark terminals)
red :: RichText -> RichText
red = color Vivid Red

green :: RichText -> RichText
green = color Vivid Green

yellow :: RichText -> RichText
yellow = color Vivid Yellow

blue :: RichText -> RichText
blue = color Vivid Blue

magenta :: RichText -> RichText
magenta = color Vivid Magenta

cyan :: RichText -> RichText
cyan = color Vivid Cyan

white :: RichText -> RichText
white = color Vivid White

black :: RichText -> RichText
black = color Vivid Black

gray :: RichText -> RichText
gray = color Dull White

style :: Style -> RichText -> RichText
style st = applyStyle (const st)

-- Helper class to convert things to RichText
class Renderable a where
  toRichText :: a -> RichText

instance Renderable RichText where
  toRichText = id

{-# LANGUAGE FlexibleInstances #-}

module Termflow.Format
  ( RichText (..),
    Segment (..),
    Style (..),
    Renderable (..),
    defaultStyle,
    plain,

    -- * Style Helpers
    bold,
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

    -- * Generic Style Applier
    style,
    color,
  )
where

import Data.String (IsString (..))
import Data.Text (Text)
import qualified Data.Text as T
import System.Console.ANSI

-- | A piece of text with style.
data Segment = Segment
  { segStyle :: Style,
    segText :: Text
  }
  deriving (Show, Eq)

-- | Rich text is a list of segments.
newtype RichText = RichText {unRichText :: [Segment]}
  deriving (Show, Eq)

instance Semigroup RichText where
  RichText xs <> RichText ys = RichText (merge xs ys)
    where
      -- Optional: Merge adjacent segments with same style
      merge [] b = b
      merge a [] = a
      merge a b = a ++ b -- For simplicity, don't merge/optimize yet

instance Monoid RichText where
  mempty = RichText []

instance IsString RichText where
  fromString s = raw (T.pack s)

-- | Style definition
data Style = Style
  { sBold :: Bool,
    sItalic :: Bool,
    sUnderline :: Bool,
    sColor :: Maybe (ColorIntensity, Color)
  }
  deriving (Show, Eq)

defaultStyle :: Style
defaultStyle =
  Style
    { sBold = False,
      sItalic = False,
      sUnderline = False,
      sColor = Nothing
    }

plain :: Text -> RichText
plain = raw

raw :: Text -> RichText
raw t = RichText [Segment defaultStyle t]

-- | Update all segments in the RichText with a function applied to their style.
applyStyle :: (Style -> Style) -> RichText -> RichText
applyStyle f (RichText segs) = RichText $ map modify segs
  where
    modify (Segment st t) = Segment (f st) t

-- | Set bold
bold :: RichText -> RichText
bold = applyStyle (\st -> st {sBold = True})

italic :: RichText -> RichText
italic = applyStyle (\st -> st {sItalic = True})

underline :: RichText -> RichText
underline = applyStyle (\st -> st {sUnderline = True})

-- | Helper to set color
color :: ColorIntensity -> Color -> RichText -> RichText
color i c = applyStyle (\st -> st {sColor = Just (i, c)})

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

style :: Style -> RichText -> RichText
style st = applyStyle (const st)

-- Helper class to convert things to RichText
class Renderable a where
  toRichText :: a -> RichText

instance Renderable RichText where
  toRichText = id

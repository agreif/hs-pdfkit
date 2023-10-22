{-# LANGUAGE OverloadedStrings #-}

module PdfKit.AfmFont where

import qualified Data.List as L
import Data.Text

data AfmFont = AfmFont
  { afmFontFontName :: Text,
    afmFontFullName :: Text,
    afmFontFamilyName :: Text,
    afmFontWeight :: Text,
    afmFontItalicAngle :: Double,
    afmFontIsFixedPitch :: Bool,
    afmFontCharacterSet :: Text,
    afmFontFontBBox :: (Double, Double, Double, Double),
    afmFontUnderlinePosition :: Double,
    afmFontUnderlineThickness :: Double,
    afmFontVersion :: Text,
    afmFontEncodingScheme :: Text,
    afmFontCapHeight :: Maybe Double,
    afmFontXHeight :: Maybe Double,
    afmFontAscender :: Maybe Double,
    afmFontDescender :: Maybe Double,
    afmFontStdHW :: Double,
    afmFontStdVW :: Double,
    afmFontCharMetrics :: [AfmCharMetric],
    afmFontKernPairXs :: [AfmKernPairX]
  }
  deriving (Show, Eq)

data AfmCharMetric = AfmCharMetric
  { afmCharMetricCharCode :: Double,
    afmCharMetricWidthX :: Double,
    afmCharMetricCharName :: Text,
    afmCharMetricBBox :: (Double, Double, Double, Double),
    afmCharMetricLigatures :: [(Text, Text)]
  }
  deriving (Show, Eq)

data AfmKernPairX = AfmKernPairX
  { afmKernPairXChar1Name :: Text,
    afmKernPairXChar2Name :: Text,
    afmKernPairXAmount :: Double
  }
  deriving (Show, Eq)

win2Ansi :: Int -> Int
win2Ansi 402 = 131
win2Ansi 8211 = 150
win2Ansi 8212 = 151
win2Ansi 8216 = 145
win2Ansi 8217 = 146
win2Ansi 8218 = 130
win2Ansi 8220 = 147
win2Ansi 8221 = 148
win2Ansi 8222 = 132
win2Ansi 8224 = 134
win2Ansi 8225 = 135
win2Ansi 8226 = 149
win2Ansi 8230 = 133
win2Ansi 8364 = 128
win2Ansi 8240 = 137
win2Ansi 8249 = 139
win2Ansi 8250 = 155
win2Ansi 710 = 136
win2Ansi 8482 = 153
win2Ansi 338 = 140
win2Ansi 339 = 156
win2Ansi 732 = 152
win2Ansi 352 = 138
win2Ansi 353 = 154
win2Ansi 376 = 159
win2Ansi 381 = 142
win2Ansi 382 = 15
win2Ansi c = c

glyphs :: [Text]
glyphs =
  [".notdef", ".notdef", ".notdef", ".notdef"]
    ++ [".notdef", ".notdef", ".notdef", ".notdef"]
    ++ [".notdef", ".notdef", ".notdef", ".notdef"]
    ++ [".notdef", ".notdef", ".notdef", ".notdef"]
    ++ [".notdef", ".notdef", ".notdef", ".notdef"]
    ++ [".notdef", ".notdef", ".notdef", ".notdef"]
    ++ [".notdef", ".notdef", ".notdef", ".notdef"]
    ++ [".notdef", ".notdef", ".notdef", ".notdef"]
    ++ ["space", "exclam", "quotedbl", "numbersign"]
    ++ ["dollar", "percent", "ampersand", "quotesingle"]
    ++ ["parenleft", "parenright", "asterisk", "plus"]
    ++ ["comma", "hyphen", "period", "slash"]
    ++ ["zero", "one", "two", "three"]
    ++ ["four", "five", "six", "seven"]
    ++ ["eight", "nine", "colon", "semicolon"]
    ++ ["less", "equal", "greater", "question"]
    ++ ["at", "A", "B", "C"]
    ++ ["D", "E", "F", "G"]
    ++ ["H", "I", "J", "K"]
    ++ ["L", "M", "N", "O"]
    ++ ["P", "Q", "R", "S"]
    ++ ["T", "U", "V", "W"]
    ++ ["X", "Y", "Z", "bracketleft"]
    ++ ["backslash", "bracketright", "asciicircum", "underscore"]
    ++ ["grave", "a", "b", "c"]
    ++ ["d", "e", "f", "g"]
    ++ ["h", "i", "j", "k"]
    ++ ["l", "m", "n", "o"]
    ++ ["p", "q", "r", "s"]
    ++ ["t", "u", "v", "w"]
    ++ ["x", "y", "z", "braceleft"]
    ++ ["bar", "braceright", "asciitilde", ".notdef"]
    ++ ["Euro", ".notdef", "quotesinglbase", "florin"]
    ++ ["quotedblbase", "ellipsis", "dagger", "daggerdbl"]
    ++ ["circumflex", "perthousand", "Scaron", "guilsinglleft"]
    ++ ["OE", ".notdef", "Zcaron", ".notdef"]
    ++ [".notdef", "quoteleft", "quoteright", "quotedblleft"]
    ++ ["quotedblright", "bullet", "endash", "emdash"]
    ++ ["tilde", "trademark", "scaron", "guilsinglright"]
    ++ ["oe", ".notdef", "zcaron", "ydieresis"]
    ++ ["space", "exclamdown", "cent", "sterling"]
    ++ ["currency", "yen", "brokenbar", "section"]
    ++ ["dieresis", "copyright", "ordfeminine", "guillemotleft"]
    ++ ["logicalnot", "hyphen", "registered", "macron"]
    ++ ["degree", "plusminus", "twosuperior", "threesuperior"]
    ++ ["acute", "mu", "paragraph", "periodcentered"]
    ++ ["cedilla", "onesuperior", "ordmasculine", "guillemotright"]
    ++ ["onequarter", "onehalf", "threequarters", "questiondown"]
    ++ ["Agrave", "Aacute", "Acircumflex", "Atilde"]
    ++ ["Adieresis", "Aring", "AE", "Ccedilla"]
    ++ ["Egrave", "Eacute", "Ecircumflex", "Edieresis"]
    ++ ["Igrave", "Iacute", "Icircumflex", "Idieresis"]
    ++ ["Eth", "Ntilde", "Ograve", "Oacute"]
    ++ ["Ocircumflex", "Otilde", "Odieresis", "multiply"]
    ++ ["Oslash", "Ugrave", "Uacute", "Ucircumflex"]
    ++ ["Udieresis", "Yacute", "Thorn", "germandbls"]
    ++ ["agrave", "aacute", "acircumflex", "atilde"]
    ++ ["adieresis", "aring", "ae", "ccedilla"]
    ++ ["egrave", "eacute", "ecircumflex", "edieresis"]
    ++ ["igrave", "iacute", "icircumflex", "idieresis"]
    ++ ["eth", "ntilde", "ograve", "oacute"]
    ++ ["ocircumflex", "otilde", "odieresis", "divide"]
    ++ ["oslash", "ugrave", "uacute", "ucircumflex"]
    ++ ["udieresis", "yacute", "thorn", "ydieresis"]

charCode2Glyph :: Int -> Text
charCode2Glyph charCode =
  if charPos < L.length glyphs then glyphs !! charPos else ".notdef"
  where
    charPos = win2Ansi charCode

text2Glyphs :: Text -> [Text]
text2Glyphs text = L.map (charCode2Glyph . fromEnum) $ unpack text

glyphWidth :: Text -> AfmFont -> Double
glyphWidth glyph font =
  case L.find (\cm -> afmCharMetricCharName cm == glyph) (afmFontCharMetrics font) of
    Just cm -> afmCharMetricWidthX cm
    _ -> 0

kernPairWidth :: (Text, Text) -> AfmFont -> Double
kernPairWidth (glyphLeft, glyphRight) font =
  case L.find
    ( \kp ->
        afmKernPairXChar1Name kp == glyphLeft
          && afmKernPairXChar2Name kp == glyphRight
    )
    (afmFontKernPairXs font) of
    Just kp -> afmKernPairXAmount kp
    _ -> 0

glyphsAdvances :: [Text] -> AfmFont -> Double
glyphsAdvances glyphList font =
  L.foldl
    ( \acc pair -> acc + glyphWidth (fst pair) font + kernPairWidth pair font
    )
    0
    pairs
  where
    pairs :: [(Text, Text)]
    pairs = L.tail $ L.zip ("" : glyphList) (glyphList ++ [""])

textWidth :: Text -> AfmFont -> Double -> Double
textWidth text font pointSize =
  glyphsAdvances (text2Glyphs text) font * pointSize / 1000

glyphsAdvances' :: [Text] -> AfmFont -> Double -> [(Double, Double, (Text, Text))]
glyphsAdvances' glyphList font pointSize =
  L.foldl
    ( \acc pair ->
        acc
          ++ [ ( case fst pair of
                   "" -> 0
                   glyph -> glyphWidth glyph font
                   * pointSize
                   / 1000,
                 kernPairWidth pair font,
                 pair
               )
             ]
    )
    []
    pairs
  where
    pairs :: [(Text, Text)]
    pairs = L.tail $ L.zip ("" : glyphList) (glyphList ++ [""])

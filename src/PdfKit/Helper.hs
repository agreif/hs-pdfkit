{-# LANGUAGE OverloadedStrings #-}

module PdfKit.Helper where

import Data.Aeson ((.=), ToJSON, object, toJSON)
import qualified Data.List as L
import Data.Text (Text)
import qualified Data.Text as T
import Data.Time
import PdfKit.AfmFont
import PdfKit.AfmParser
import Text.Printf (printf)

formatLocalTime :: TimeZone -> UTCTime -> String
formatLocalTime timeZone utcTime =
  formatTime defaultTimeLocale "%Y%m%d%H%M%S" $ utcToLocalTime timeZone utcTime

maybeTextToText :: Maybe Text -> Text
maybeTextToText Nothing = ""
maybeTextToText (Just t) = t

maybeIntToText :: Maybe Int -> Text
maybeIntToText Nothing = ""
maybeIntToText (Just i) = T.pack . show $ i

intToText :: Int -> Text
intToText = T.pack . show

doubleToText :: Double -> Text
doubleToText d = T.pack $ printf "%f" d

ref :: Int -> Text
ref objId = T.concat [intToText objId, " 0 R"]

formatXrefPos :: Int -> Text
formatXrefPos i = T.pack $ printf "%010d" i

-----------------------------------------------
data PdfStreamContent
  = PdfPath
      { pdfPathPoints :: [PdfPos],
        pdfPathWidth :: Maybe Double,
        pdfPathDoStroke :: Maybe Bool
      }
  | PdfText
      { pdfTextText :: Maybe Text,
        pdfTextX :: Double,
        pdfTextY :: Double,
        pdfTextStandardFont :: Maybe PdfStandardFont,
        pdfTextFontSize :: Maybe Double,
        pdfTextColor :: Maybe PdfColor,
        pdfTextFillOpacity :: Maybe Double
      }
  deriving (Eq)

instance ToJSON PdfStreamContent where
  toJSON o =
    case o of
      t@PdfText {} ->
        object
          [ "text" .= pdfTextText t,
            "x" .= pdfTextX t,
            "y" .= pdfTextY t,
            "standardFont" .= pdfTextStandardFont t,
            "fontSize" .= pdfTextFontSize t,
            "color" .= pdfTextColor t,
            "opacity" .= pdfTextFillOpacity t
          ]
      p@PdfPath {} ->
        object ["points" .= pdfPathPoints p, "doStroke" .= pdfPathDoStroke p]

isPdfText :: PdfStreamContent -> Bool
isPdfText pdfStreamContent =
  case pdfStreamContent of
    PdfText {} -> True
    _ -> False

isPdfPath :: PdfStreamContent -> Bool
isPdfPath pdfStreamContent =
  case pdfStreamContent of
    PdfPath {} -> True
    _ -> False

-----------------------------------------------
data PdfPageMargins = PdfPageMargins
  { pdfPageMarginTop :: Double,
    pdfPageMarginLeft :: Double,
    pdfPageMarginBottom :: Double,
    pdfPageMarginRight :: Double
  }

instance ToJSON PdfPageMargins where
  toJSON o =
    object
      [ "top" .= pdfPageMarginTop o,
        "left" .= pdfPageMarginLeft o,
        "bottom" .= pdfPageMarginBottom o,
        "right" .= pdfPageMarginRight o
      ]

-----------------------------------------------
data PdfColor
  = PdfColorRgb
      { pdfColorRgbR :: Double,
        pdfColorRgbG :: Double,
        pdfColorRgbB :: Double
      }
  | PdfColorCmyk
      { pdfColorCmykC :: Double,
        pdfColorCmykM :: Double,
        pdfColorCmykY :: Double,
        pdfColorCmykK :: Double
      }
  deriving (Eq)

instance ToJSON PdfColor where
  toJSON o =
    case o of
      PdfColorRgb r g b -> object ["r" .= r, "g" .= g, "b" .= b]
      PdfColorCmyk c m y k -> object ["c" .= c, "m" .= m, "y" .= y, "k" .= k]

-----------------------------------------------
data PdfPageLayout
  = Portrait
  | Landscape
  deriving (Show)

portrait :: PdfPageLayout
portrait = Portrait

landscape :: PdfPageLayout
landscape = Landscape

-----------------------------------------------
data PdfPos = PdfPos
  { pdfPosX :: Double,
    pdfPosY :: Double
  }
  deriving (Eq)

instance ToJSON PdfPos where
  toJSON o = object ["x" .= pdfPosX o, "y" .= pdfPosY o]

-----------------------------------------------
data PdfPageSize = PdfPageSize
  { pdfPageSizeWidth :: Double,
    pdfPageSizeHeight :: Double
  }

instance ToJSON PdfPageSize where
  toJSON o =
    object ["width" .= pdfPageSizeWidth o, "height" .= pdfPageSizeHeight o]

defaultPageSize :: PdfPageSize
defaultPageSize = sA4

s4A0 :: PdfPageSize
s4A0 = PdfPageSize 4767.87 6740.79

s2A0 :: PdfPageSize
s2A0 = PdfPageSize 3370.39 4767.87

sA0 :: PdfPageSize
sA0 = PdfPageSize 2383.94 3370.39

sA1 :: PdfPageSize
sA1 = PdfPageSize 1683.78 2383.94

sA2 :: PdfPageSize
sA2 = PdfPageSize 1190.55 1683.78

sA3 :: PdfPageSize
sA3 = PdfPageSize 841.89 1190.55

sA4 :: PdfPageSize
sA4 = PdfPageSize 595.28 841.89

sA5 :: PdfPageSize
sA5 = PdfPageSize 419.53 595.28

sA6 :: PdfPageSize
sA6 = PdfPageSize 297.64 419.53

sA7 :: PdfPageSize
sA7 = PdfPageSize 209.76 297.64

sA8 :: PdfPageSize
sA8 = PdfPageSize 147.40 209.76

sA9 :: PdfPageSize
sA9 = PdfPageSize 104.88 147.40

sA10 :: PdfPageSize
sA10 = PdfPageSize 73.70 104.88

sB0 :: PdfPageSize
sB0 = PdfPageSize 2834.65 4008.19

sB1 :: PdfPageSize
sB1 = PdfPageSize 2004.09 2834.65

sB2 :: PdfPageSize
sB2 = PdfPageSize 1417.32 2004.09

sB3 :: PdfPageSize
sB3 = PdfPageSize 1000.63 1417.32

sB4 :: PdfPageSize
sB4 = PdfPageSize 708.66 1000.63

sB5 :: PdfPageSize
sB5 = PdfPageSize 498.90 708.66

sB6 :: PdfPageSize
sB6 = PdfPageSize 354.33 498.90

sB7 :: PdfPageSize
sB7 = PdfPageSize 249.45 354.33

sB8 :: PdfPageSize
sB8 = PdfPageSize 175.75 249.45

sB9 :: PdfPageSize
sB9 = PdfPageSize 124.72 175.75

sB10 :: PdfPageSize
sB10 = PdfPageSize 87.87 124.72

sC0 :: PdfPageSize
sC0 = PdfPageSize 2599.37 3676.54

sC1 :: PdfPageSize
sC1 = PdfPageSize 1836.85 2599.37

sC2 :: PdfPageSize
sC2 = PdfPageSize 1298.27 1836.85

sC3 :: PdfPageSize
sC3 = PdfPageSize 918.43 1298.27

sC4 :: PdfPageSize
sC4 = PdfPageSize 649.13 918.43

sC5 :: PdfPageSize
sC5 = PdfPageSize 459.21 649.13

sC6 :: PdfPageSize
sC6 = PdfPageSize 323.15 459.21

sC7 :: PdfPageSize
sC7 = PdfPageSize 229.61 323.15

sC8 :: PdfPageSize
sC8 = PdfPageSize 161.57 229.61

sC9 :: PdfPageSize
sC9 = PdfPageSize 113.39 161.57

sC10 :: PdfPageSize
sC10 = PdfPageSize 79.37 113.39

sRA0 :: PdfPageSize
sRA0 = PdfPageSize 2437.80 3458.27

sRA1 :: PdfPageSize
sRA1 = PdfPageSize 1729.13 2437.80

sRA2 :: PdfPageSize
sRA2 = PdfPageSize 1218.90 1729.13

sRA3 :: PdfPageSize
sRA3 = PdfPageSize 864.57 1218.90

sRA4 :: PdfPageSize
sRA4 = PdfPageSize 609.45 864.57

sSRA0 :: PdfPageSize
sSRA0 = PdfPageSize 2551.18 3628.35

sSRA1 :: PdfPageSize
sSRA1 = PdfPageSize 1814.17 2551.18

sSRA2 :: PdfPageSize
sSRA2 = PdfPageSize 1275.59 1814.17

sSRA3 :: PdfPageSize
sSRA3 = PdfPageSize 907.09 1275.59

sSRA4 :: PdfPageSize
sSRA4 = PdfPageSize 637.80 907.09

sExecutive :: PdfPageSize
sExecutive = PdfPageSize 521.86 756.00

sFolio :: PdfPageSize
sFolio = PdfPageSize 612.00 936.00

sLegal :: PdfPageSize
sLegal = PdfPageSize 612.00 1008.00

sLetter :: PdfPageSize
sLetter = PdfPageSize 612.00 792.00

sTabloid :: PdfPageSize
sTabloid = PdfPageSize 792.00 1224.00

-----------------------------------------------
data PdfStandardFont = PdfStandardFont
  { pdfStandardFontBaseFont :: Text,
    pdfStandardFontSubtype :: Text,
    pdfStandardFontEncoding :: Text,
    pdfStandardFontAfmFont :: AfmFont
  }
  deriving (Show, Eq)

instance ToJSON PdfStandardFont where
  toJSON o =
    object
      [ "baseFont" .= pdfStandardFontBaseFont o,
        "subtype" .= pdfStandardFontSubtype o,
        "encoding" .= pdfStandardFontEncoding o
      ]

mkAfmStdFontCourier :: IO (Maybe PdfStandardFont)
mkAfmStdFontCourier = do
  maybeAfmFont <- mkAfmFont "Courier"
  return $
    case maybeAfmFont of
      Just afmFont ->
        Just $
          PdfStandardFont
            "Courier"
            "Type1"
            "WinAnsiEncoding"
            afmFont
      _ -> Nothing

mkAfmStdFontCourierBold :: IO (Maybe PdfStandardFont)
mkAfmStdFontCourierBold = do
  maybeAfmFont <- mkAfmFont "Courier-Bold"
  return $
    case maybeAfmFont of
      Just afmFont ->
        Just $
          PdfStandardFont
            "Courier-Bold"
            "Type1"
            "WinAnsiEncoding"
            afmFont
      _ -> Nothing

mkAfmStdFontCourierOblique :: IO (Maybe PdfStandardFont)
mkAfmStdFontCourierOblique = do
  maybeAfmFont <- mkAfmFont "Courier-Oblique"
  return $
    case maybeAfmFont of
      Just afmFont ->
        Just $
          PdfStandardFont
            "Courier-Oblique"
            "Type1"
            "WinAnsiEncoding"
            afmFont
      _ -> Nothing

mkAfmStdFontCourierBoldOblique :: IO (Maybe PdfStandardFont)
mkAfmStdFontCourierBoldOblique = do
  maybeAfmFont <- mkAfmFont "Courier-BoldOblique"
  return $
    case maybeAfmFont of
      Just afmFont ->
        Just $
          PdfStandardFont
            "Courier-BoldOblique"
            "Type1"
            "WinAnsiEncoding"
            afmFont
      _ -> Nothing

mkAfmStdFontHelvetica :: IO (Maybe PdfStandardFont)
mkAfmStdFontHelvetica = do
  maybeAfmFont <- mkAfmFont "Helvetica"
  return $
    case maybeAfmFont of
      Just afmFont ->
        Just $
          PdfStandardFont
            "Helvetica"
            "Type1"
            "WinAnsiEncoding"
            afmFont
      _ -> Nothing

mkAfmStdFontHelveticaBold :: IO (Maybe PdfStandardFont)
mkAfmStdFontHelveticaBold = do
  maybeAfmFont <- mkAfmFont "Helvetica-Bold"
  return $
    case maybeAfmFont of
      Just afmFont ->
        Just $
          PdfStandardFont
            "Helvetica-Bold"
            "Type1"
            "WinAnsiEncoding"
            afmFont
      _ -> Nothing

mkAfmStdFontHelveticaOblique :: IO (Maybe PdfStandardFont)
mkAfmStdFontHelveticaOblique = do
  maybeAfmFont <- mkAfmFont "Helvetica-Oblique"
  return $
    case maybeAfmFont of
      Just afmFont ->
        Just $
          PdfStandardFont
            "Helvetica-Oblique"
            "Type1"
            "WinAnsiEncoding"
            afmFont
      _ -> Nothing

mkAfmStdFontHelveticaBoldOblique :: IO (Maybe PdfStandardFont)
mkAfmStdFontHelveticaBoldOblique = do
  maybeAfmFont <- mkAfmFont "Helvetica-BoldOblique"
  return $
    case maybeAfmFont of
      Just afmFont ->
        Just $
          PdfStandardFont
            "Helvetica-BoldOblique"
            "Type1"
            "WinAnsiEncoding"
            afmFont
      _ -> Nothing

mkAfmStdFontTimesRoman :: IO (Maybe PdfStandardFont)
mkAfmStdFontTimesRoman = do
  maybeAfmFont <- mkAfmFont "Times-Roman"
  return $
    case maybeAfmFont of
      Just afmFont ->
        Just $
          PdfStandardFont
            "Times-Roman"
            "Type1"
            "WinAnsiEncoding"
            afmFont
      _ -> Nothing

mkAfmStdFontTimesBold :: IO (Maybe PdfStandardFont)
mkAfmStdFontTimesBold = do
  maybeAfmFont <- mkAfmFont "Times-Bold"
  return $
    case maybeAfmFont of
      Just afmFont ->
        Just $
          PdfStandardFont
            "Times-Bold"
            "Type1"
            "WinAnsiEncoding"
            afmFont
      _ -> Nothing

mkAfmStdFontTimesItalic :: IO (Maybe PdfStandardFont)
mkAfmStdFontTimesItalic = do
  maybeAfmFont <- mkAfmFont "Times-Italic"
  return $
    case maybeAfmFont of
      Just afmFont ->
        Just $
          PdfStandardFont
            "Times-Italic"
            "Type1"
            "WinAnsiEncoding"
            afmFont
      _ -> Nothing

mkAfmStdFontTimesBoldItalic :: IO (Maybe PdfStandardFont)
mkAfmStdFontTimesBoldItalic = do
  maybeAfmFont <- mkAfmFont "Times-BoldItalic"
  return $
    case maybeAfmFont of
      Just afmFont ->
        Just $
          PdfStandardFont
            "Times-BoldItalic"
            "Type1"
            "WinAnsiEncoding"
            afmFont
      _ -> Nothing

mkAfmStdFontSymbol :: IO (Maybe PdfStandardFont)
mkAfmStdFontSymbol = do
  maybeAfmFont <- mkAfmFont "Symbol"
  return $
    case maybeAfmFont of
      Just afmFont ->
        Just $
          PdfStandardFont
            "Symbol"
            "Type1"
            "WinAnsiEncoding"
            afmFont
      _ -> Nothing

mkAfmStdFontZapfDingbats :: IO (Maybe PdfStandardFont)
mkAfmStdFontZapfDingbats = do
  maybeAfmFont <- mkAfmFont "ZapfDingbats"
  return $
    case maybeAfmFont of
      Just afmFont ->
        Just $
          PdfStandardFont
            "ZapfDingbats"
            "Type1"
            "WinAnsiEncoding"
            afmFont
      _ -> Nothing

-----------------------------------------------
translateOrigin :: Double -> Text
translateOrigin pageHeight =
  T.concat ["1 0 0 -1 0 ", doubleToText pageHeight, " cm"]

-----------------------------------------------
fontAscent :: PdfStandardFont -> Double -> Double
fontAscent stdFont pointSize =
  case maybeAscender of
    Just ascender -> ascender / 1000 * pointSize
    _ -> 0
  where
    afmFont = pdfStandardFontAfmFont stdFont
    maybeAscender = afmFontAscender afmFont

fontLineHeight :: PdfStandardFont -> Double -> Double
fontLineHeight stdFont pointSize =
  case (maybeAscender, maybeDescender) of
    (Just ascender, Just descender) ->
      (ascender + lineGap ascender descender - descender) / 1000 * pointSize
    _ -> 0
  where
    afmFont = pdfStandardFontAfmFont stdFont
    maybeAscender = afmFontAscender afmFont
    maybeDescender = afmFontDescender afmFont
    lineGap ascender descender = top - bottom - ascender + descender
    (top, bottom) =
      let (_, b, _, t) = afmFontFontBBox afmFont
       in (t, b)

splitLines :: Text -> Double -> PdfStandardFont -> Double -> [Text]
splitLines text maxWidth stdFont pointSize = L.foldl (\acc paragraph -> acc ++ lines' paragraph) [] paragraphs
  where
    spaceWidth = textWidth " " font pointSize
    font = pdfStandardFontAfmFont stdFont
    paragraphs :: [Text]
    paragraphs = T.lines text
    wordWidthTuples :: Text -> [(Text, Double)]
    wordWidthTuples text' = L.map (\t -> (t, textWidth t font pointSize)) $ T.words text'
    lines' :: Text -> [Text]
    lines' text' = lines'' 0 [] [] (wordWidthTuples text')
    --
    lines'' :: Double -> [Text] -> [Text] -> [(Text, Double)] -> [Text]
    lines'' _ collectedWords collectedLines [] = collectedLines ++ [T.intercalate " " collectedWords]
    lines'' width collectedWords collectedLines widthTuples =
      if width + spaceWidth + wi > maxWidth
        then lines'' 0 [] (collectedLines ++ [T.intercalate " " collectedWords]) widthTuples
        else lines'' (width + spaceWidth + wi) (collectedWords ++ [wo]) collectedLines (L.tail widthTuples)
      where
        (wo, wi) = L.head widthTuples

defaultPageMargins :: PdfPageMargins
defaultPageMargins = PdfPageMargins 72 72 72 72

defaultFont :: IO (Maybe PdfStandardFont)
defaultFont = mkAfmStdFontHelvetica

defaultFontSize :: Double
defaultFontSize = 24

applyLayout :: PdfPageSize -> PdfPageLayout -> PdfPageSize
applyLayout size Portrait = size
applyLayout (PdfPageSize w h) Landscape =
  PdfPageSize {pdfPageSizeWidth = h, pdfPageSizeHeight = w}

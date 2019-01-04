{-# LANGUAGE OverloadedStrings #-}

module PdfKit.StandardFont where

import Data.Aeson
import Data.Text
import PdfKit.AfmFont.AfmFont
import PdfKit.AfmFont.Courier
import PdfKit.AfmFont.CourierBold
import PdfKit.AfmFont.CourierBoldOblique
import PdfKit.AfmFont.CourierOblique
import PdfKit.AfmFont.Helvetica
import PdfKit.AfmFont.HelveticaBold
import PdfKit.AfmFont.HelveticaBoldOblique
import PdfKit.AfmFont.HelveticaOblique
import PdfKit.AfmFont.Symbol
import PdfKit.AfmFont.TimesBold
import PdfKit.AfmFont.TimesBoldItalic
import PdfKit.AfmFont.TimesItalic
import PdfKit.AfmFont.TimesRoman
import PdfKit.AfmFont.ZapfDingbats

data PdfStandardFont = PdfStandardFont
  { pdfStandardFontBaseFont :: Text
  , pdfStandardFontSubtype :: Text
  , pdfStandardFontEncoding :: Text
  , pdfStandardFontAfmFont :: AfmFont
  } deriving (Eq)

instance ToJSON PdfStandardFont where
  toJSON o =
    object
      [ "baseFont" .= pdfStandardFontBaseFont o
      , "subtype" .= pdfStandardFontSubtype o
      , "encoding" .= pdfStandardFontEncoding o
      ]

courier :: PdfStandardFont
courier = PdfStandardFont "Courier" "Type1" "WinAnsiEncoding" afmFontCourier

courierBold :: PdfStandardFont
courierBold =
  PdfStandardFont "Courier-Bold" "Type1" "WinAnsiEncoding" afmFontCourierBold

courierOblique :: PdfStandardFont
courierOblique =
  PdfStandardFont
    "Courier-Oblique"
    "Type1"
    "WinAnsiEncoding"
    afmFontCourierOblique

courierBoldOblique :: PdfStandardFont
courierBoldOblique =
  PdfStandardFont
    "Courier-BoldOblique"
    "Type1"
    "WinAnsiEncoding"
    afmFontCourierBoldOblique

helvetica :: PdfStandardFont
helvetica =
  PdfStandardFont "Helvetica" "Type1" "WinAnsiEncoding" afmFontHelvetica

helveticaBold :: PdfStandardFont
helveticaBold =
  PdfStandardFont
    "Helvetica-Bold"
    "Type1"
    "WinAnsiEncoding"
    afmFontHelveticaBold

helveticaOblique :: PdfStandardFont
helveticaOblique =
  PdfStandardFont
    "Helvetica-Oblique"
    "Type1"
    "WinAnsiEncoding"
    afmFontHelveticaOblique

helveticaBoldOblique :: PdfStandardFont
helveticaBoldOblique =
  PdfStandardFont
    "Helvetica-BoldOblique"
    "Type1"
    "WinAnsiEncoding"
    afmFontHelveticaBoldOblique

timesRoman :: PdfStandardFont
timesRoman =
  PdfStandardFont "Times-Roman" "Type1" "WinAnsiEncoding" afmFontTimesRoman

timesBold :: PdfStandardFont
timesBold =
  PdfStandardFont "Times-Bold" "Type1" "WinAnsiEncoding" afmFontTimesBold

timesItalic :: PdfStandardFont
timesItalic =
  PdfStandardFont "Times-Italic" "Type1" "WinAnsiEncoding" afmFontTimesItalic

timesBoldItalic :: PdfStandardFont
timesBoldItalic =
  PdfStandardFont
    "Times-BoldItalic"
    "Type1"
    "WinAnsiEncoding"
    afmFontTimesBoldItalic

symbol :: PdfStandardFont
symbol = PdfStandardFont "Symbol" "Type1" "WinAnsiEncoding" afmFontSymbol

zapfDingbats :: PdfStandardFont
zapfDingbats =
  PdfStandardFont "ZapfDingbats" "Type1" "WinAnsiEncoding" afmFontZapfDingbats

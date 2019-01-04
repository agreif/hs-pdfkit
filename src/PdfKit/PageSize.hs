{-# LANGUAGE OverloadedStrings #-}

module PdfKit.PageSize where

import Data.Aeson

data PdfPageSize = PdfPageSize
  { pdfPageSizeWidth :: Double
  , pdfPageSizeHeight :: Double
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

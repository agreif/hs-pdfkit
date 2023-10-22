{-# LANGUAGE OverloadedStrings #-}

module PdfKit.AfmParser where

import qualified Data.ByteString as BS
import qualified Data.List as L
import qualified Data.Text as T
import Data.Text.Encoding as TE
import Paths_hs_pdfkit
import PdfKit.AfmFont
import System.Directory
import System.FilePath

mkAfmFont :: T.Text -> IO (Maybe AfmFont)
mkAfmFont name = do
  path <- getDataFileName $ "Core14_AFMs" </> T.unpack name <.> "afm"
  bytes <- BS.readFile $ path
  let lines' = L.map (T.splitOn " ") $ T.lines $ TE.decodeUtf8 bytes
  return $ buildAfmFont lines'

-- return $ buildAfmFont lines'

buildAfmFont :: Lines -> Maybe AfmFont
buildAfmFont lines' = do
  fontName <- findTextValue "FontName" lines'
  fullName <- findTextValue "FullName" lines'
  familyName <- findTextValue "FamilyName" lines'
  weight <- findTextValue "Weight" lines'
  italicAngle <- findDoubleValue "ItalicAngle" lines'
  isFixedPitch <- findBoolValue "IsFixedPitch" lines'
  characterSet <- findTextValue "CharacterSet" lines'
  fontBBox <- findDoubleValue'''' "FontBBox" lines'
  underlinePosition <- findDoubleValue "UnderlinePosition" lines'
  underlineThickness <- findDoubleValue "UnderlineThickness" lines'
  version <- findTextValue "Version" lines'
  encodingScheme <- findTextValue "EncodingScheme" lines'
  capHeight <- findMaybeDoubleValue "CapHeight" lines'
  xHeight <- findMaybeDoubleValue "XHeight" lines'
  ascender <- findMaybeDoubleValue "Ascender" lines'
  descender <- findMaybeDoubleValue "Descender" lines'
  stdHW <- findDoubleValue "StdHW" lines'
  stdVW <- findDoubleValue "StdVW" lines'
  charMetrics <- buildAfmCharMetrics lines'
  kernPairXs <- buildAfmKernPairXs lines'
  return $
    AfmFont
      { afmFontFontName = fontName,
        afmFontFullName = fullName,
        afmFontFamilyName = familyName,
        afmFontWeight = weight,
        afmFontItalicAngle = italicAngle,
        afmFontIsFixedPitch = isFixedPitch,
        afmFontCharacterSet = characterSet,
        afmFontFontBBox = fontBBox,
        afmFontUnderlinePosition = underlinePosition,
        afmFontUnderlineThickness = underlineThickness,
        afmFontVersion = version,
        afmFontEncodingScheme = encodingScheme,
        afmFontCapHeight = capHeight,
        afmFontXHeight = xHeight,
        afmFontAscender = ascender,
        afmFontDescender = descender,
        afmFontStdHW = stdHW,
        afmFontStdVW = stdVW,
        afmFontCharMetrics = charMetrics,
        afmFontKernPairXs = kernPairXs
      }

buildAfmKernPairXs :: Lines -> Maybe [AfmKernPairX]
buildAfmKernPairXs lines' = do
  let lines'' = L.dropWhile (\line -> L.head line /= "StartKernPairs") lines'
  case lines'' of
    [] -> return []
    _ -> do
      let lines''' = L.takeWhile (\line -> L.head line /= "EndKernPairs") $ L.tail lines''
      let pairs =
            L.map
              ( \line ->
                  AfmKernPairX
                    { afmKernPairXChar1Name = line !! 1,
                      afmKernPairXChar2Name = line !! 2,
                      afmKernPairXAmount = read $ T.unpack $ line !! 3
                    }
              )
              lines'''
      return pairs

buildAfmCharMetrics :: Lines -> Maybe [AfmCharMetric]
buildAfmCharMetrics lines' = do
  let lines'' = L.dropWhile (\line -> L.head line /= "StartCharMetrics") lines'
  let lines''' = L.takeWhile (\line -> L.head line /= "EndCharMetrics") $ L.tail lines''
  let metrics =
        L.map
          ( \line ->
              AfmCharMetric
                { afmCharMetricCharCode = read $ T.unpack $ line !! 1,
                  afmCharMetricWidthX = read $ T.unpack $ line !! 4,
                  afmCharMetricCharName = line !! 7,
                  afmCharMetricBBox =
                    ( read $ T.unpack $ line !! 10,
                      read $ T.unpack $ line !! 11,
                      read $ T.unpack $ line !! 12,
                      read $ T.unpack $ line !! 13
                    ),
                  afmCharMetricLigatures = pairs $ L.filter (/= ";") $ L.drop 15 line
                }
          )
          lines'''
  return metrics

pairs :: [a] -> [(a, a)]
pairs (x0 : x1 : xs) = (x0, x1) : pairs xs
pairs [] = []
pairs _ = error "odd number of elements"

type Line = [T.Text]

type Lines = [Line]

type Key = T.Text

findLine :: T.Text -> Lines -> Maybe Line
findLine key = L.find (\line -> L.head line == key)

findTextValue :: Key -> Lines -> Maybe T.Text
findTextValue key lines' = case findLine key lines' of
  Just line -> Just $ L.last line
  _ -> Nothing

findDoubleValue :: Key -> Lines -> Maybe Double
findDoubleValue key lines' = case findLine key lines' of
  Just line -> Just $ read $ T.unpack $ line !! 1
  _ -> Nothing

findMaybeDoubleValue :: Key -> Lines -> Maybe (Maybe Double)
findMaybeDoubleValue key lines' = case findLine key lines' of
  Just line -> Just $ Just $ read $ T.unpack $ line !! 1
  _ -> Just Nothing

findBoolValue :: Key -> Lines -> Maybe Bool
findBoolValue key lines' = case findLine key lines' of
  Just line -> case line !! 1 of
    "true" -> Just True
    "false" -> Just False
    _ -> Nothing
  _ -> Nothing

findDoubleValue'''' :: Key -> Lines -> Maybe (Double, Double, Double, Double)
findDoubleValue'''' key lines' = case findLine key lines' of
  Just line ->
    Just
      ( read $ T.unpack $ line !! 1,
        read $ T.unpack $ line !! 2,
        read $ T.unpack $ line !! 2,
        read $ T.unpack $ line !! 3
      )
  _ -> Nothing

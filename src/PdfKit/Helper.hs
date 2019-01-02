{-# LANGUAGE OverloadedStrings #-}
module PdfKit.Helper where

import Data.Text (Text)
import qualified Data.Text as T
import Data.Time
import Text.Printf

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
intToText i = T.pack . show $ i

doubleToText :: Double -> Text
doubleToText x = T.pack $ show $ x

ref :: Int -> Text
ref objId = T.concat [intToText objId, " 0 R"]

formatXrefPos :: Int -> Text
formatXrefPos i = T.pack $ printf "%010d" i

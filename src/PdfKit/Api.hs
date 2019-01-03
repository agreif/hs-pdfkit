module PdfKit.Api
  ( PdfKit.Api.producer
  , PdfKit.Api.creator
  , PdfKit.Api.page
  , PdfKit.Api.pageTemplate
  , PdfKit.Api.font
  , PdfKit.Api.fontSize
  , PdfKit.Api.pageSize
  , PdfKit.Api.pageSizeCustom
  , PdfKit.Api.layout
  , PdfKit.Api.margin
  , PdfKit.Api.margins
  , PdfKit.Api.textAt
  , PdfKit.Api.text
  , PdfKit.Api.moveDown
  , PdfKit.Api.buildPdfDoc
  , PdfKit.Api.encodePdf
  , PdfKit.Api.encodePdf'
  ) where

import Data.ByteString (ByteString)
import qualified Data.ByteString.Char8 as B8
import qualified Data.List as L
import Data.Text (Text)
import qualified Data.Text.Encoding as T
import Data.Time
import PdfKit.Builder

producer :: Text -> DocumentBuilder
producer = documentAction . ActionInfoSetProducer

creator :: Text -> DocumentBuilder
creator = documentAction . ActionInfoSetCreator

page :: PageBuilderM a -> DocumentBuilder
page (PageBuilderM actions _) =
  documentAction $ ActionComposite $ ActionPage : actions

pageTemplate :: PageBuilder -> PageBuilder -> DocumentBuilder
pageTemplate (PageBuilderM actions1 _) (PageBuilderM actions2 _) =
  page $ do
    pageAction $ ActionComposite actions1
    pageAction $ ActionComposite actions2

font :: PdfStandardFont -> PageBuilder
font = pageAction . ActionFont

fontSize :: Double -> PageBuilder
fontSize = pageAction . ActionFontSetSize

pageSize :: PdfPageSize -> PageBuilder
pageSize = pageAction . ActionPageSetSize

pageSizeCustom :: Double -> Double -> PageBuilder
pageSizeCustom w h = pageAction $ ActionPageSetSizeCustom w h

layout :: PdfPageLayout -> PageBuilder
layout = pageAction . ActionPageSetLayout

margin :: Double -> PageBuilder
margin = pageAction . ActionPageSetMargin

margins :: Double -> Double -> Double -> Double -> PageBuilder
margins t l b r = pageAction $ ActionPageSetMargins t l b r

textAt :: Text -> Double -> Double -> PageBuilder
textAt t x y = do
  pageAction ActionFontAddIfMissing
  pageAction $ ActionTextAt t x y
  pageAction ActionMoveDown

text :: Text -> PageBuilder
text t = do
  pageAction ActionFontAddIfMissing
  pageAction $ ActionText t
  pageAction ActionMoveDown

moveDown :: PageBuilder
moveDown = pageAction ActionMoveDown

-----------------------------------------------

buildPdfDoc :: UTCTime -> TimeZone -> DocumentBuilderM a -> PdfDocument
buildPdfDoc now timeZone (DocumentBuilderM userActions _) =
  L.foldl
    (flip execute)
    (initialPdfDocument now timeZone)
    (userActions ++ [ActionFinalize])

-----------------------------------------------
encodePdf :: PdfDocument -> ByteString
encodePdf pdfDoc = B8.unlines $ toByteStringLines pdfDoc

encodePdf' :: PdfDocument -> [Text]
encodePdf' pdfDoc = map T.decodeUtf8 $ toByteStringLines pdfDoc

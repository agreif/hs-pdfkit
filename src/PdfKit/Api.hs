module PdfKit.Api
  ( PdfKit.Api.producer
  , PdfKit.Api.creator
  , PdfKit.Api.creationDate
  , PdfKit.Api.page
  , PdfKit.Api.pageTemplate
  , PdfKit.Api.font
  , PdfKit.Api.fontSize
  , PdfKit.Api.pageSize
  , PdfKit.Api.pageSizeCustom
  , PdfKit.Api.layout
  , PdfKit.Api.margin
  , PdfKit.Api.margins
  , PdfKit.Api.textPos
  , PdfKit.Api.textColorRgb
  , PdfKit.Api.textColorCmyk
  , PdfKit.Api.textFillOpacity
  , PdfKit.Api.text
  , PdfKit.Api.textTemplate
  , PdfKit.Api.content
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
import PdfKit.Helper

producer :: Text -> PdfDocumentBuilder
producer = documentAction . ActionInfoSetProducer

creator :: Text -> PdfDocumentBuilder
creator = documentAction . ActionInfoSetCreator

creationDate :: UTCTime -> TimeZone -> PdfDocumentBuilder
creationDate now timeZone =
  documentAction $ ActionInfoSetCreationDate now timeZone

page :: PdfPageBuilderM a -> PdfDocumentBuilder
page (PdfPageBuilderM actions _) =
  documentAction $ ActionComposite $ ActionPage : actions

pageTemplate :: PdfPageBuilder -> PdfPageBuilder -> PdfDocumentBuilder
pageTemplate (PdfPageBuilderM actions1 _) (PdfPageBuilderM actions2 _) =
  page $ do
    pageAction $ ActionComposite actions1
    pageAction $ ActionComposite actions2

pageSize :: PdfPageSize -> PdfPageBuilder
pageSize = pageAction . ActionPageSetSize

pageSizeCustom :: Double -> Double -> PdfPageBuilder
pageSizeCustom w h = pageAction $ ActionPageSetSizeCustom w h

layout :: PdfPageLayout -> PdfPageBuilder
layout = pageAction . ActionPageSetLayout

margin :: Double -> PdfPageBuilder
margin = pageAction . ActionPageSetMargin

margins :: Double -> Double -> Double -> Double -> PdfPageBuilder
margins t l b r = pageAction $ ActionPageSetMargins t l b r

text :: PdfTextBuilderM a -> PdfPageBuilder
text (PdfTextBuilderM actions _) =
  pageAction $ ActionComposite $ ActionText : actions ++ [ActionMoveDown]

textTemplate :: PdfTextBuilder -> PdfTextBuilder -> PdfPageBuilder
textTemplate (PdfTextBuilderM actions1 _) (PdfTextBuilderM actions2 _) =
  text $ do
    textAction $ ActionComposite actions1
    textAction $ ActionComposite actions2

textPos :: Double -> Double -> PdfTextBuilder
textPos x y = textAction $ ActionTextPos x y

textColorRgb :: Double -> Double -> Double -> PdfTextBuilder
textColorRgb r g b = textAction $ ActionTextColor $ PdfColorRgb r g b

textColorCmyk :: Double -> Double -> Double -> Double -> PdfTextBuilder
textColorCmyk c m y k = textAction $ ActionTextColor $ PdfColorCmyk c m y k

textFillOpacity :: Double -> PdfTextBuilder
textFillOpacity = textAction . ActionTextFillOpacity

content :: Text -> PdfTextBuilder
content = textAction . ActionTextContent

font :: PdfStandardFont -> PdfTextBuilder
font = textAction . ActionTextFont

fontSize :: Double -> PdfTextBuilder
fontSize = textAction . ActionTextFontSize

moveDown :: PdfPageBuilder
moveDown = pageAction ActionMoveDown

-----------------------------------------------
buildPdfDoc :: PdfDocumentBuilderM a -> PdfDocument
buildPdfDoc (PdfDocumentBuilderM userActions _) =
  L.foldl (flip execute) initialPdfDocument (userActions ++ [ActionFinalize])

-----------------------------------------------
encodePdf :: PdfDocument -> ByteString
encodePdf pdfDoc = B8.unlines $ toByteStringLines pdfDoc

encodePdf' :: PdfDocument -> [Text]
encodePdf' pdfDoc = map T.decodeUtf8 $ toByteStringLines pdfDoc

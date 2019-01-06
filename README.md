# hs-pdfkit

## Sample (rendering in a yesod handler)

```haskell
samplePdfDoc :: Handler PdfDocument
samplePdfDoc = do
  timeZone <- liftIO getCurrentTimeZone
  now <- liftIO getCurrentTime
  return $
    buildPdfDoc $ do
      producer "sample producer"
      creator "sample creator"
      creationDate now timeZone
      pageA4Landscape $ do
        line $ do
          linePoint 100 100
          linePoint 100 200
          linePoint 200 200
          lineStroke
        text $ do
          textPos 180 190
          textColorCmyk 10 60 60 80
          textFillOpacity 1
          font helvetica
          fontSize 24
          content "foo bar"
        textHelvetica24BlueOpaque $ do
          content "ü ä=ã, ö=õ, ü=ũ"
  where
    pageA4Landscape :: PdfPageBuilder -> PdfDocumentBuilder
    pageA4Landscape =
      pageTemplate $ do
        pageSize sLetter
        layout landscape
    textHelvetica24BlueOpaque :: PdfTextBuilder -> PdfPageBuilder
    textHelvetica24BlueOpaque =
      textTemplate $ do
        font helvetica
        fontSize 24
        textColorRgb 0 0 255
        textFillOpacity 0.5

-- Yesod Handler
getSamplePdfInlineR :: Handler TypedContent
getSamplePdfInlineR = do
  pdfDoc <- samplePdfDoc
  addHeader "Content-Disposition" $
    T.concat ["inline; filename=\"", "samplepdf.pdf", "\""]
  respond (encodeUtf8 "application/pdf") $ encodePdf pdfDoc

-- Yesod Handler
getSamplePdfDownloadR :: Handler TypedContent
getSamplePdfDownloadR = do
  pdfDoc <- samplePdfDoc
  addHeader "Content-Disposition" $
    T.concat ["attachment; filename=\"", "samplepdf.pdf", "\""]
  respond (encodeUtf8 "application/pdf") $ encodePdf pdfDoc

-- Yesod Handler
getSamplePdfJsonR :: Handler Value
getSamplePdfJsonR = do
  pdfDoc <- samplePdfDoc
  return $
    toJSON
    ( pdfDoc
    , encodePdf' pdfDoc
    )
```

{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE FlexibleInstances #-}
module PdfKit.Builder where

import Data.Time
import Data.Aeson
import qualified Data.List as L
import Data.Text (Text)
import qualified Data.Text as T
import qualified Data.Text.Encoding as T
import Control.Monad
import qualified Data.Maybe as M
import Data.ByteString (ByteString)
import qualified Data.ByteString as BS
import qualified Data.ByteString.Char8 as B8
import PdfKit.Helper
import PdfKit.AfmFont.AfmFont
import PdfKit.AfmFont.Courier
import PdfKit.AfmFont.CourierBold
import PdfKit.AfmFont.CourierOblique
import PdfKit.AfmFont.CourierBoldOblique
import PdfKit.AfmFont.Helvetica
import PdfKit.AfmFont.HelveticaBold
import PdfKit.AfmFont.HelveticaOblique
import PdfKit.AfmFont.HelveticaBoldOblique
import PdfKit.AfmFont.TimesRoman
import PdfKit.AfmFont.TimesBold
import PdfKit.AfmFont.TimesItalic
import PdfKit.AfmFont.TimesBoldItalic
import PdfKit.AfmFont.Symbol
import PdfKit.AfmFont.ZapfDingbats

-----------------------------------------------

data DocumentBuilderM a = DocumentBuilderM [Action] a

type DocumentBuilder = DocumentBuilderM ()

instance Functor DocumentBuilderM where
  fmap = liftM

instance Applicative DocumentBuilderM where
  pure  = return
  (<*>) = ap

instance Monad DocumentBuilderM where
  return a = DocumentBuilderM [] a
  DocumentBuilderM actions1 a >>= f =
    let DocumentBuilderM actions2 b = f a
    in  DocumentBuilderM (actions1 ++ actions2) b

documentAction :: Action -> DocumentBuilder
documentAction action = DocumentBuilderM [action] ()

-----------------------------------------------

data PageBuilderM a = PageBuilderM [Action] a

type PageBuilder = PageBuilderM ()

instance Functor PageBuilderM where
  fmap = liftM

instance Applicative PageBuilderM where
  pure  = return
  (<*>) = ap

instance Monad PageBuilderM where
  return a = PageBuilderM [] a
  PageBuilderM actions1 a >>= f =
    let PageBuilderM actions2 b = f a
    in  PageBuilderM (actions1 ++ actions2) b

pageAction :: Action -> PageBuilder
pageAction action = PageBuilderM [action] ()

-----------------------------------------------

class ToByteStringLines a where
  toByteStringLines :: a -> [ByteString]

class IsExecutableAction a where
  execute :: a -> PdfDocument -> PdfDocument

-----------------------------------------------

data PdfDocument = PdfDocument
  { pdfDocumentVersion :: Text
  , pdfDocumentHeaderLines :: [ByteString]
  , pdfDocumentCreationDate :: Text
  , pdfDocumentNextObjId :: Int
  , pdfDocumentInfo :: PdfInfo
  , pdfDocumentRoot :: PdfRoot
  , pdfDocumentPages :: PdfPages
  , pdfDocumentFonts :: [PdfFont]
  , pdfDocumentTrailer :: PdfTrailer
  , pdfDocumentXref :: PdfXref
  , pdfDocumentStartXref :: Maybe Int
  }

instance ToJSON PdfDocument where
  toJSON o = object
    [ "version" .= pdfDocumentVersion o
    , "nextObjId" .= pdfDocumentNextObjId o
    , "info" .= pdfDocumentInfo o
    , "root" .= pdfDocumentRoot o
    , "pages" .= pdfDocumentPages o
    , "fonts" .= pdfDocumentFonts o
    , "trailer" .= pdfDocumentTrailer o
    , "xref" .= pdfDocumentXref o
    , "startxref" .= pdfDocumentStartXref o
    ]

instance ToByteStringLines PdfDocument where
  toByteStringLines pdfDoc =
    headerLines
    ++ L.foldl (\acc x -> acc ++ x) [] objectBlocks
    ++ footerLines
    where
      (headerLines, objectBlocks, footerLines) =
        pdfDocumentByteStringLineBlocks pdfDoc

initialPdfDocument :: UTCTime -> TimeZone -> PdfDocument
initialPdfDocument now timeZone =
  PdfDocument
  { pdfDocumentVersion = version
  , pdfDocumentHeaderLines =
    [ T.encodeUtf8 $ T.concat ["%PDF-", version]
    , B8.pack ['%', '\xff', '\xff', '\xff', '\xff']
    ]
  , pdfDocumentCreationDate = creationDate
  , pdfDocumentNextObjId = nextObjId
  , pdfDocumentInfo =
      PdfInfo
      { pdfInfoObjId = infoObjId
      , pdfInfoProducer = "hs-pdfkit"
      , pdfInfoCreator = "hs-pdfkit"
      , pdfInfoCreationDate = T.concat [ "D:", creationDate, "Z"]
      }
  , pdfDocumentRoot =
      PdfRoot
      { pdfRootObjId = rootObjId
      , pdfRootPages = ref pagesObjId
      }
  , pdfDocumentPages =
      PdfPages
      { pdfPagesObjId = pagesObjId
      , pdfPagesKids = []
      }
  , pdfDocumentFonts = []
  , pdfDocumentXref = PdfXref { pdfXrefPositions = [] }
  , pdfDocumentTrailer = PdfTrailer { pdfTrailerSize = Nothing }
  , pdfDocumentStartXref = Nothing
  }
  where
    creationDate = T.pack $ formatLocalTime timeZone now
    version = "1.3"
    infoObjId = 1
    rootObjId = 2
    pagesObjId = 3
    nextObjId = 4

-----------------------------------------------

data PdfInfo = PdfInfo
  { pdfInfoObjId :: Int
  , pdfInfoProducer :: Text
  , pdfInfoCreator :: Text
  , pdfInfoCreationDate :: Text
  }

instance ToJSON PdfInfo where
  toJSON o = object
    [ "objId" .= pdfInfoObjId o
    , "producer" .= pdfInfoProducer o
    , "creator" .= pdfInfoCreator o
    , "creationDate" .= pdfInfoCreationDate o
    ]

instance ToByteStringLines PdfInfo where
  toByteStringLines pdfInfo =
    [ T.encodeUtf8 $ T.concat [(T.pack $ show $ pdfInfoObjId pdfInfo), " 0 obj"]
    , T.encodeUtf8 $ T.concat ["% ------------------------------------------------------ info ", (intToText $ pdfInfoObjId pdfInfo)]
    , T.encodeUtf8 "<<"
    , T.encodeUtf8 $ T.concat ["/Producer (", pdfInfoProducer pdfInfo, ")"]
    , T.encodeUtf8 $ T.concat ["/Creator (", pdfInfoCreator pdfInfo, ")"]
    , T.encodeUtf8 $ T.concat ["/CreationDate (", pdfInfoCreationDate pdfInfo, ")"]
    , T.encodeUtf8 ">>"
    , T.encodeUtf8 "endobj"
    ]

-----------------------------------------------

data PdfRoot = PdfRoot
  { pdfRootObjId :: Int
  , pdfRootPages :: Text
  }

instance ToJSON PdfRoot where
  toJSON o = object
    [ "objId" .= pdfRootObjId o
    , "pages" .= pdfRootPages o
    ]

instance ToByteStringLines (PdfRoot, PdfDocument) where
  toByteStringLines (pdfRoot, pdfDoc) =
    [ T.encodeUtf8 $ T.concat [T.pack $ show $ pdfRootObjId pdfRoot, " 0 obj"]
    , T.encodeUtf8 $ T.concat ["% ------------------------------------------------------ root ", (intToText $ pdfRootObjId pdfRoot)]
    , T.encodeUtf8 "<<"
    , T.encodeUtf8 $ "/Type /Catalog"
    , T.encodeUtf8 $ T.concat ["/Pages ", (ref $ pdfPagesObjId $ pdfDocumentPages pdfDoc)]
    , T.encodeUtf8 ">>"
    , T.encodeUtf8 "endobj"
    ]

-----------------------------------------------

data PdfPages = PdfPages
  { pdfPagesObjId :: Int
  , pdfPagesKids :: [PdfPage]
  }

instance ToJSON PdfPages where
  toJSON o = object
    [ "objId" .= pdfPagesObjId o
    , "kids" .= pdfPagesKids o
    ]

instance ToByteStringLines PdfPages where
  toByteStringLines pdfPages =
    [ T.encodeUtf8 $ T.concat [T.pack $ show $ pdfPagesObjId pdfPages, " 0 obj"]
    , T.encodeUtf8 $ T.concat ["% ------------------------------------------------------ pages ", (intToText $ pdfPagesObjId pdfPages)]
    , T.encodeUtf8 "<<"
    , T.encodeUtf8 $ "/Type /Pages"
    , T.encodeUtf8 $ T.concat ["/Count ", (intToText $ L.length $ pdfPagesKids pdfPages)]
    , T.encodeUtf8 $ T.concat [ "/Kids ["
                              , (T.intercalate "  " $
                                  L.map (\pdfPage -> ref $ pdfPageObjId pdfPage) (pdfPagesKids pdfPages)
                                )
                              , "]"
                              ]
    , T.encodeUtf8 ">>"
    , T.encodeUtf8 "endobj"
    ]

-----------------------------------------------

data PdfFont = PdfFont
  { pdfFontObjId :: Int
  , pdfFontName :: Text
  , pdfFontStandardFont ::PdfStandardFont
  }
  deriving Eq

instance ToJSON PdfFont where
  toJSON o = object
    [ "objId" .= pdfFontObjId o
    , "name" .= pdfFontName o
    , "standardFont" .= pdfFontStandardFont o
    ]

instance ToByteStringLines PdfFont where
  toByteStringLines pdfFont =
    [ T.encodeUtf8 $ T.concat [(T.pack $ show $ pdfFontObjId pdfFont), " 0 obj"]
    , T.encodeUtf8 $ T.concat ["% ------------------------------------------------------ Font ", (intToText $ pdfFontObjId pdfFont)]
    , T.encodeUtf8 "<<"
    , T.encodeUtf8 $ "/Type /Font"
    , T.encodeUtf8 $ T.concat ["/Name /", pdfFontName pdfFont]
    , T.encodeUtf8 $ T.concat ["/BaseFont /", pdfStandardFontBaseFont standardFont]
    , T.encodeUtf8 $ T.concat ["/Subtype /" , pdfStandardFontSubtype standardFont]
    , T.encodeUtf8 $ T.concat ["/Encoding /", pdfStandardFontEncoding standardFont]
    , T.encodeUtf8 ">>"
    , T.encodeUtf8 "endobj"
    ]
    where
      standardFont = pdfFontStandardFont pdfFont

-----------------------------------------------

data PdfResources = PdfResources
  { pdfResourcesObjId :: Int
  , pdfResourcesFonts :: [PdfFont]
  }

instance ToJSON PdfResources where
  toJSON o = object
    [ "objId" .= pdfResourcesObjId o
    , "fonts" .= pdfResourcesFonts o
    ]

instance ToByteStringLines PdfResources where
  toByteStringLines pdfResources =
    [ T.encodeUtf8 $ T.concat [T.pack $ show $ pdfResourcesObjId pdfResources, " 0 obj"]
    , T.encodeUtf8 $ T.concat ["% ------------------------------------------------------ Resources ", (intToText $ pdfResourcesObjId pdfResources)]
    , T.encodeUtf8 "<<"
    , T.encodeUtf8 $ "/ProcSet [/PDF /Text /ImageB /ImageC /ImageI]"
    ]
    ++
    ( case pdfResourcesFonts pdfResources of
        [] -> []
        pdfFonts ->
          [ T.encodeUtf8 $ "/Font <<" ]
          ++
          ( L.map (\pdfFont ->
                     T.encodeUtf8 $ T.concat ["/", pdfFontName pdfFont, " ", (ref $ pdfFontObjId pdfFont)]
                  ) pdfFonts
          )
          ++
          [ T.encodeUtf8 $ ">>" ]
    )
    ++
    [ T.encodeUtf8 ">>"
    , T.encodeUtf8 "endobj"
    ]

-----------------------------------------------

data PdfPage = PdfPage
  { pdfPageObjId :: Int
  , pdfPageSize :: PdfPageSize
  , pdfPageMargins :: PdfPageMargins
  , pdfPageLayout :: PdfPageLayout
  , pdfPageResources :: PdfResources
  , pdfPageContents :: PdfContents
  , pdfPageCurrentPos :: PdfPos
  , pdfPageCurrentFont :: PdfStandardFont
  , pdfPageCurrentFontSize :: Double
  }

instance ToJSON PdfPage where
  toJSON o = object
    [ "objId" .= pdfPageObjId o
    , "size" .= pdfPageSize o
    , "margins" .= pdfPageMargins o
    , "layout" .= ( T.pack $ case pdfPageLayout o of
                               Portrait -> "portrait"
                               Landscape -> "landscape" )
    , "resources" .= pdfPageResources o
    , "contents" .= pdfPageContents o
    , "currentPos" .= pdfPageCurrentPos o
    , "currentFont" .= pdfPageCurrentFont o
    , "currentFontSize" .= pdfPageCurrentFontSize o
    ]

instance ToByteStringLines (PdfPage, PdfDocument) where
  toByteStringLines (pdfPage, pdfDoc) =
    [ T.encodeUtf8 $ T.concat [T.pack $ show $ pdfPageObjId pdfPage, " 0 obj"]
    , T.encodeUtf8 $ T.concat ["% ------------------------------------------------------ Page ", (intToText $ pdfPageObjId pdfPage)]
    , T.encodeUtf8 "<<"
    , T.encodeUtf8 $ "/Type /Page"
    , T.encodeUtf8 $ T.concat ["/Parent ", (ref $ pdfPagesObjId $ pdfDocumentPages pdfDoc)]
    , T.encodeUtf8 $ T.concat ["% ", (T.pack $ show $ pdfPageLayout pdfPage)]
    , T.encodeUtf8 $ T.concat [ "/MediaBox [0 0 "
                              , (doubleToText $ pdfPageSizeWidth $ pdfPageSize pdfPage)
                              , " "
                              , (doubleToText $ pdfPageSizeHeight $ pdfPageSize pdfPage)
                              , "]"
                              ]
    , T.encodeUtf8 $ T.concat ["/Resources ", (ref $ pdfResourcesObjId $ pdfPageResources pdfPage)]
    , T.encodeUtf8 $ T.concat ["/Contents ", (ref $ pdfContentsObjId $ pdfPageContents pdfPage)]
    , T.encodeUtf8 ">>"
    , T.encodeUtf8 "endobj"
    ]

-----------------------------------------------

data PdfContents = PdfContents
  { pdfContentsObjId :: Int
  , pdfContentsTexts :: [PdfText]
  }

instance ToJSON PdfContents where
  toJSON o = object
    [ "objId" .= pdfContentsObjId o
    , "texts" .= pdfContentsTexts o
    ]

instance ToByteStringLines (PdfContents, PdfPage, PdfDocument) where
  toByteStringLines (pdfContents, pdfPage, pdfDoc) =
    [ T.encodeUtf8 $ T.concat [T.pack $ show $ pdfContentsObjId pdfContents, " 0 obj"]
    , T.encodeUtf8 $ T.concat ["% ------------------------------------------------------ Contents ", (intToText $ pdfContentsObjId pdfContents)]
    , T.encodeUtf8 "<<"
    , T.encodeUtf8 $ T.concat ["/Length ", intToText streamLength]
    , T.encodeUtf8 ">>"
    , T.encodeUtf8 "stream"
    , T.encodeUtf8 translateOrigin
    ]
    ++
    ( map T.encodeUtf8 streamTextLines )
    ++
    [ T.encodeUtf8 "endstream"
    ]
    where
      PdfPageSize {pdfPageSizeHeight = pageHeight} = pdfPageSize pdfPage
      streamTextLines =
        L.foldl
        (\acc pdfText ->
           acc
           ++
           [ "q"
           , translateOrigin
           , "BT"
           , translatePos pdfText
           , T.concat[ "/"
                     , ( case findPdfFont (pdfTextStandardFont pdfText) pdfDoc of
                           Just pdfFont -> pdfFontName pdfFont
                           _ -> ""
                       )
                     , " "
                     , (doubleToText $ pdfTextFontSize pdfText)
                     , " Tf"
                     ]
           , T.concat ["(", pdfTextText pdfText,  ") Tj"]
           , "ET"
           , "Q"
           ] :: [Text]
        ) [] $ pdfContentsTexts pdfContents
      translateOrigin = T.concat ["1 0 0 -1 0 ", (doubleToText pageHeight), " cm"]
      translatePos pdfText =
        T.concat [ "1 0 0 1 ", doubleToText $ pdfTextX pdfText
                 , " ", (doubleToText $ pageHeight-(pdfTextY pdfText)
                         -(dy (pdfTextStandardFont pdfText) (pdfTextFontSize pdfText))
                        )
                 , " Tm"
                 ]
      streamLength :: Int
      streamLength = BS.length $ T.encodeUtf8 $ T.unlines streamTextLines

-----------------------------------------------

data PdfText = PdfText
  { pdfTextText :: Text
  , pdfTextX :: Double
  , pdfTextY :: Double
  , pdfTextStandardFont :: PdfStandardFont
  , pdfTextFontSize :: Double
  }

instance ToJSON PdfText where
  toJSON o = object
    [ "text" .= pdfTextText o
    , "x" .= pdfTextX o
    , "y" .= pdfTextY o
    , "standardFont" .= pdfTextStandardFont o
    , "fontSize" .= pdfTextFontSize o
    ]

-----------------------------------------------

data PdfXref = PdfXref
  { pdfXrefPositions :: [Int]
  }

instance ToJSON PdfXref where
  toJSON o = object
    [ "positions" .= pdfXrefPositions o
    ]

instance ToByteStringLines (PdfXref, PdfDocument) where
  toByteStringLines (pdfXref, pdfDoc) =
    [ T.encodeUtf8 "xref"
    , T.encodeUtf8 "% ------------------------------------------------------ xref"
    , T.encodeUtf8 $ T.concat ["0 ", ( case pdfTrailerSize $ pdfDocumentTrailer pdfDoc of
                                         Just size -> intToText $ 1 + size
                                         _ -> ""
                                     )
                              ]
    , T.encodeUtf8 $ "0000000000 65535 f"
    ]
    ++
    (L.map (\pos -> T.encodeUtf8 $ T.concat [formatXrefPos pos, " 00000 n"]) $ pdfXrefPositions pdfXref)

-----------------------------------------------

data PdfTrailer = PdfTrailer
  { pdfTrailerSize :: Maybe Int
  }

instance ToJSON PdfTrailer where
  toJSON o = object
    [ "size" .= pdfTrailerSize o
    ]

instance ToByteStringLines (PdfTrailer, PdfDocument) where
  toByteStringLines (pdfTrailer, pdfDoc) =
    [ T.encodeUtf8 "trailer"
    , T.encodeUtf8 "% ------------------------------------------------------ trailer"
    , T.encodeUtf8 "<<"
    , T.encodeUtf8 $ T.concat ["/Size ", maybeIntToText $ pdfTrailerSize pdfTrailer]
    , T.encodeUtf8 $ T.concat ["/Root ", ref $ pdfRootObjId $ pdfDocumentRoot pdfDoc]
    , T.encodeUtf8 $ T.concat ["/Info ", ref $ pdfInfoObjId $ pdfDocumentInfo pdfDoc]
    , T.encodeUtf8 ">>"
    ]

-----------------------------------------------

data PdfPageMargins =
  PdfPageMargins
  { pdfPageMarginTop :: Double
  , pdfPageMarginLeft :: Double
  , pdfPageMarginBottom :: Double
  , pdfPageMarginRight :: Double
  }

instance ToJSON PdfPageMargins where
  toJSON o = object
    [ "top" .= pdfPageMarginTop o
    , "left" .= pdfPageMarginLeft o
    , "bottom" .= pdfPageMarginBottom o
    , "right" .= pdfPageMarginRight o
    ]

defaultPageMargins :: PdfPageMargins
defaultPageMargins = PdfPageMargins 72 72 72 72

-----------------------------------------------

data PdfPageLayout = Portrait | Landscape
  deriving Show

portrait :: PdfPageLayout
portrait = Portrait

landscape :: PdfPageLayout
landscape = Landscape

-----------------------------------------------

data PdfPos = PdfPos
  { pdfPosX :: Double
  , pdfPosY :: Double
  }

instance ToJSON PdfPos where
  toJSON o = object
    [ "x" .= pdfPosX o
    , "y" .= pdfPosY o
    ]

-----------------------------------------------

data PdfPageSize = PdfPageSize
  { pdfPageSizeWidth :: Double
  , pdfPageSizeHeight :: Double
  }

instance ToJSON PdfPageSize where
  toJSON o = object
    [ "width" .= pdfPageSizeWidth o
    , "height" .= pdfPageSizeHeight o
    ]

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
  { pdfStandardFontBaseFont :: Text
  , pdfStandardFontSubtype :: Text
  , pdfStandardFontEncoding :: Text
  , pdfStandardFontAfmFont :: AfmFont
  }
  deriving Eq

instance ToJSON PdfStandardFont where
  toJSON o = object
    [ "baseFont" .= pdfStandardFontBaseFont o
    , "subtype" .= pdfStandardFontSubtype o
    , "encoding" .= pdfStandardFontEncoding o
    ]

dy :: PdfStandardFont -> Double -> Double
dy stdFont size =
  case maybeAscender of
    Just ascender -> ascender / 1000 * size
    _ -> 0
  where
    afmFont = pdfStandardFontAfmFont stdFont
    maybeAscender = afmFontAscender afmFont

fontLineHeight :: PdfStandardFont -> Double -> Double
fontLineHeight stdFont size =
  case (maybeAscender, maybeDescender) of
    (Just ascender, Just descender) -> (ascender + lineGap - descender) / 1000 * size
    _ -> 0
  where
    afmFont = pdfStandardFontAfmFont stdFont
    maybeAscender = afmFontAscender afmFont
    maybeDescender = afmFontDescender afmFont
    lineGap = case (maybeAscender, maybeDescender) of
      (Just ascender, Just descender) -> top - bottom - ascender + descender;
      _ -> 0
    (top, bottom) = let (_, b, _, t) = afmFontFontBBox afmFont
                    in (t, b)

currentLineHeight :: PdfPage -> Double
currentLineHeight pdfPage =
  fontLineHeight (pdfPageCurrentFont pdfPage) (pdfPageCurrentFontSize pdfPage)

defaultFont :: PdfStandardFont
defaultFont = helvetica

defaultFontSize :: Double
defaultFontSize = 24

courier :: PdfStandardFont
courier = PdfStandardFont "Courier" "Type1" "WinAnsiEncoding" afmFontCourier
courierBold :: PdfStandardFont
courierBold = PdfStandardFont "Courier-Bold" "Type1" "WinAnsiEncoding" afmFontCourierBold
courierOblique :: PdfStandardFont
courierOblique = PdfStandardFont "Courier-Oblique" "Type1" "WinAnsiEncoding" afmFontCourierOblique
courierBoldOblique :: PdfStandardFont
courierBoldOblique = PdfStandardFont "Courier-BoldOblique" "Type1" "WinAnsiEncoding" afmFontCourierBoldOblique
helvetica :: PdfStandardFont
helvetica = PdfStandardFont "Helvetica" "Type1" "WinAnsiEncoding" afmFontHelvetica
helveticaBold :: PdfStandardFont
helveticaBold = PdfStandardFont "Helvetica-Bold" "Type1" "WinAnsiEncoding" afmFontHelveticaBold
helveticaOblique :: PdfStandardFont
helveticaOblique = PdfStandardFont "Helvetica-Oblique" "Type1" "WinAnsiEncoding" afmFontHelveticaOblique
helveticaBoldOblique :: PdfStandardFont
helveticaBoldOblique = PdfStandardFont "Helvetica-BoldOblique" "Type1" "WinAnsiEncoding" afmFontHelveticaBoldOblique
timesRoman :: PdfStandardFont
timesRoman = PdfStandardFont "Times-Roman" "Type1" "WinAnsiEncoding" afmFontTimesRoman
timesBold :: PdfStandardFont
timesBold = PdfStandardFont "Times-Bold" "Type1" "WinAnsiEncoding" afmFontTimesBold
timesItalic :: PdfStandardFont
timesItalic = PdfStandardFont "Times-Italic" "Type1" "WinAnsiEncoding" afmFontTimesItalic
timesBoldItalic :: PdfStandardFont
timesBoldItalic = PdfStandardFont "Times-BoldItalic" "Type1" "WinAnsiEncoding" afmFontTimesBoldItalic
symbol :: PdfStandardFont
symbol = PdfStandardFont "Symbol" "Type1" "WinAnsiEncoding" afmFontSymbol
zapfDingbats :: PdfStandardFont
zapfDingbats = PdfStandardFont "ZapfDingbats" "Type1" "WinAnsiEncoding" afmFontZapfDingbats

findPdfFont :: PdfStandardFont -> PdfDocument -> Maybe PdfFont
findPdfFont stdFont pdfDoc =
  L.find
  (\pdfFont -> pdfFontStandardFont pdfFont == stdFont)
  (pdfDocumentFonts pdfDoc)

currentPdfFont :: PdfDocument -> Maybe PdfFont
currentPdfFont pdfDoc =
  findPdfFont (pdfPageCurrentFont lastPage) pdfDoc
  where
    (_, _, lastPage) = pdfPagesTuple pdfDoc

currentPdfFontId :: PdfDocument -> Maybe Int
currentPdfFontId pdfDoc =
  case currentPdfFont pdfDoc of
    Just pdfFont -> Just $ pdfFontObjId pdfFont
    _ -> Nothing

-----------------------------------------------

pdfPagesTuple :: PdfDocument -> (PdfPages, [PdfPage], PdfPage)
pdfPagesTuple pdfDoc = (pdfPages, initPages, lastPage)
  where
    pdfPages = pdfDocumentPages pdfDoc
    initPages = L.init $ pdfPagesKids pdfPages
    lastPage = L.last $ pdfPagesKids pdfPages

applyLayout :: PdfPageSize -> PdfPageLayout -> PdfPageSize
applyLayout size Portrait = size
applyLayout (PdfPageSize w h ) Landscape =
  PdfPageSize
  { pdfPageSizeWidth = h
  , pdfPageSizeHeight = w
  }

pdfDocumentByteStringLineBlocks :: PdfDocument -> ([ByteString], [[ByteString]], [ByteString])
pdfDocumentByteStringLineBlocks pdfDoc =
  ( -- header lines
    pdfDocumentHeaderLines pdfDoc
  , -- referencable line-blocks
    [ toByteStringLines $ pdfDocumentInfo pdfDoc
    , toByteStringLines (pdfDocumentRoot pdfDoc, pdfDoc)
    , toByteStringLines $ pdfDocumentPages pdfDoc
    ]
    ++
    ( L.foldl
      (\acc pdfPage ->
          [ toByteStringLines (pdfPage, pdfDoc)
          , toByteStringLines $ pdfPageResources pdfPage
          , toByteStringLines (pdfPageContents pdfPage, pdfPage, pdfDoc)
          ]
          ++ acc
      )
      []
      (pdfPagesKids $ pdfDocumentPages pdfDoc)
    )
    ++
    ( L.map (\pdfFont -> toByteStringLines pdfFont) $ pdfDocumentFonts pdfDoc )
  , -- footer lines
    ( toByteStringLines (pdfDocumentXref pdfDoc, pdfDoc))
    ++ ( toByteStringLines (pdfDocumentTrailer pdfDoc, pdfDoc))
    ++ [ T.encodeUtf8 "startxref"
       , T.encodeUtf8 $ maybeIntToText $ pdfDocumentStartXref pdfDoc
       , T.encodeUtf8 "%%EOF"
       ]
  )

-----------------------------------------------

data Action =
  ActionInfoSetProducer Text
  | ActionInfoSetCreator Text
  | ActionFinalize
  | ActionFont PdfStandardFont
  | ActionFontAddIfMissing
  | ActionFontSetSize Double
  | ActionPage
  | ActionPageSetSize PdfPageSize
  | ActionPageSetLayout PdfPageLayout
  | ActionPageSetMargin Double
  | ActionPageSetMargins Double Double Double Double
  | ActionPageSetSizeCustom Double Double
  | ActionTextAt Text Double Double
  | ActionText Text
  | ActionMoveDown
  | ActionComposite [Action]

-----------------------------------------------

instance IsExecutableAction Action where

  execute (ActionComposite actions) pdfDoc =
    pdfDoc'
    where
      pdfDoc' =
        L.foldl
        (\acc action -> execute action acc)
        pdfDoc
        actions

  execute (ActionInfoSetProducer text) pdfDoc =
    pdfDoc
    { pdfDocumentInfo =
        (pdfDocumentInfo pdfDoc) { pdfInfoProducer = text}
    }

  execute (ActionInfoSetCreator text) pdfDoc =
    pdfDoc
    { pdfDocumentInfo =
        (pdfDocumentInfo pdfDoc) { pdfInfoCreator = text}
    }

  execute ActionFinalize pdfDoc =
    pdfDoc
    { pdfDocumentXref = PdfXref { pdfXrefPositions = L.init positions }
    , pdfDocumentStartXref = Just $ L.last positions
    , pdfDocumentTrailer =
        (pdfDocumentTrailer pdfDoc) { pdfTrailerSize = Just $ pdfDocumentNextObjId pdfDoc - 1 }
    }
    where
      (headerLines, objectBlocks, _) = pdfDocumentByteStringLineBlocks pdfDoc
      headerLength = BS.length $ B8.unlines headerLines
      objectLines = L.map B8.unlines objectBlocks
      lengths = L.map BS.length objectLines
      (positions, _) = L.foldl (\(ls,accl) len -> (ls ++ [accl+len], accl+len)) ([headerLength], headerLength) lengths

  execute (ActionFont standardFont) pdfDoc =
    pdfDoc
    { pdfDocumentPages =
        pdfPages
        { pdfPagesKids =
          initPages
          ++
          [ lastPage { pdfPageCurrentFont = standardFont } ]
        }
    }
    where
      (pdfPages, initPages, lastPage) = pdfPagesTuple pdfDoc

  execute (ActionFontSetSize fontSize) pdfDoc =
    pdfDoc
    { pdfDocumentPages =
        pdfPages
        { pdfPagesKids =
          initPages
          ++
          [ lastPage { pdfPageCurrentFontSize = fontSize } ]
        }
    }
    where
      (pdfPages, initPages, lastPage) = pdfPagesTuple pdfDoc

  execute (ActionFontAddIfMissing) pdfDoc =
    pdfDoc
    { pdfDocumentNextObjId = nextObjId
    , pdfDocumentFonts =
        (pdfDocumentFonts pdfDoc)
        ++
        if fontAlreadyAdded
        then []
        else [ PdfFont
               { pdfFontObjId = fontObjId
               , pdfFontName = T.concat ["F", intToText fontObjId]
               , pdfFontStandardFont = pdfPageCurrentFont lastPage
               }
             ]
    }
    where
      (_, _, lastPage) = pdfPagesTuple pdfDoc
      fontAlreadyAdded = M.isJust $ currentPdfFont pdfDoc
      fontObjId = pdfDocumentNextObjId pdfDoc
      nextObjId = if fontAlreadyAdded
                  then pdfDocumentNextObjId pdfDoc
                  else pdfDocumentNextObjId pdfDoc + 1+ 1

  execute ActionPage pdfDoc =
    pdfDoc
    { pdfDocumentNextObjId = nextObjId
    , pdfDocumentPages =
        (pdfDocumentPages pdfDoc)
        { pdfPagesKids =
            (pdfPagesKids $ pdfDocumentPages pdfDoc)
            ++
            [ PdfPage
              { pdfPageObjId = pageObjId
              , pdfPageSize = defaultPageSize
              , pdfPageMargins = defaultPageMargins
              , pdfPageLayout = Portrait
              , pdfPageResources =
                  PdfResources
                  { pdfResourcesObjId = resourcesObjId
                  , pdfResourcesFonts = []
                  }
              , pdfPageContents =
                  PdfContents
                  { pdfContentsObjId = contentsObjId
                  , pdfContentsTexts = []
                  }
              , pdfPageCurrentPos =
                PdfPos
                { pdfPosX = pdfPageMarginLeft defaultPageMargins
                , pdfPosY = pdfPageMarginTop defaultPageMargins
                }
              , pdfPageCurrentFont = defaultFont
              , pdfPageCurrentFontSize = defaultFontSize
              }
            ]
        }
    }
    where
      pageObjId = pdfDocumentNextObjId pdfDoc
      resourcesObjId = pdfDocumentNextObjId pdfDoc + 1
      contentsObjId = pdfDocumentNextObjId pdfDoc + 2
      nextObjId = pdfDocumentNextObjId pdfDoc + 3

  execute (ActionPageSetSize size) pdfDoc =
    pdfDoc
    { pdfDocumentPages =
        pdfPages
        { pdfPagesKids =
            initPages
            ++
            [ lastPage { pdfPageSize = size } ]
        }
    }
    where
      (pdfPages, initPages, lastPage) = pdfPagesTuple pdfDoc

  execute (ActionPageSetSizeCustom width height) pdfDoc =
    pdfDoc
    { pdfDocumentPages =
        pdfPages
        { pdfPagesKids =
          initPages
          ++
          [ lastPage
            { pdfPageSize =
                PdfPageSize
                { pdfPageSizeWidth = width
                , pdfPageSizeHeight = height
                }
            }
          ]
        }
    }
    where
      (pdfPages, initPages, lastPage) = pdfPagesTuple pdfDoc

  execute (ActionPageSetLayout lay) pdfDoc =
    pdfDoc
    { pdfDocumentPages =
        pdfPages
        { pdfPagesKids =
          initPages
          ++
          [ lastPage
            { pdfPageLayout = lay
            , pdfPageSize = pageSize
            }
          ]
        }
    }
    where
      (pdfPages, initPages, lastPage) = pdfPagesTuple pdfDoc
      pageSize = applyLayout (pdfPageSize lastPage) lay

  execute (ActionPageSetMargin x) pdfDoc =
    pdfDoc
    { pdfDocumentPages =
        pdfPages
        { pdfPagesKids =
          initPages
          ++
          [ lastPage { pdfPageMargins = PdfPageMargins x x x x } ]
        }
    }
    where
      (pdfPages, initPages, lastPage) = pdfPagesTuple pdfDoc

  execute (ActionPageSetMargins top left bottom right) pdfDoc =
    pdfDoc
    { pdfDocumentPages =
        pdfPages
        { pdfPagesKids =
          initPages
          ++
          [ lastPage { pdfPageMargins = PdfPageMargins top left bottom right } ]
        }
    }
    where
      (pdfPages, initPages, lastPage) = pdfPagesTuple pdfDoc

  execute (ActionTextAt t x y) pdfDoc =
    pdfDoc
    { pdfDocumentPages =
        pdfPages
        { pdfPagesKids =
          initPages
          ++
          [ lastPage
            { pdfPageResources =
                (pdfPageResources lastPage)
                { pdfResourcesFonts =
                    L.nub $ ( pageFonts lastPage
                              ++
                              case currentPdfFont pdfDoc of
                                Just pdfFont -> [pdfFont]
                                _ -> []
                            )
                }
            , pdfPageContents =
                (pdfPageContents lastPage)
                { pdfContentsTexts =
                    (pdfContentsTexts $ pdfPageContents lastPage)
                    ++
                    [ PdfText
                      { pdfTextText = t
                      , pdfTextX = x
                      , pdfTextY = y
                      , pdfTextStandardFont = pdfPageCurrentFont lastPage
                      , pdfTextFontSize = pdfPageCurrentFontSize lastPage
                      }
                  ]
                }
            , pdfPageCurrentPos = PdfPos x y
            }
          ]
        }
    }
    where
      (pdfPages, initPages, lastPage) = pdfPagesTuple pdfDoc
      pageFonts = pdfResourcesFonts . pdfPageResources

  execute (ActionText t) pdfDoc =
    execute (ActionTextAt t x y) pdfDoc
    where
      (_, _, lastPage) = pdfPagesTuple pdfDoc
      PdfPos x y = pdfPageCurrentPos lastPage

  execute ActionMoveDown pdfDoc =
    pdfDoc
    { pdfDocumentPages =
        pdfPages
        { pdfPagesKids =
          initPages
          ++
          [ lastPage { pdfPageCurrentPos = PdfPos x (y + currentLineHeight lastPage) } ]
        }
    }
    where
      (pdfPages, initPages, lastPage) = pdfPagesTuple pdfDoc
      PdfPos x y = pdfPageCurrentPos lastPage

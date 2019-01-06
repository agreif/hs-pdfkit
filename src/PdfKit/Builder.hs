{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE FlexibleInstances #-}

module PdfKit.Builder where

import Control.Monad
import Data.Aeson (ToJSON, (.=), object, toJSON)
import Data.ByteString (ByteString)
import qualified Data.ByteString as BS
import qualified Data.ByteString.Char8 as B8
import qualified Data.Hex as H
import qualified Data.List as L
import qualified Data.Maybe as M
import Data.Text (Text)
import qualified Data.Text as T
import qualified Data.Text.Encoding as T
import Data.Time
import PdfKit.Helper

-----------------------------------------------
data PdfDocumentBuilderM a =
  PdfDocumentBuilderM [Action]
                      a

type PdfDocumentBuilder = PdfDocumentBuilderM ()

instance Functor PdfDocumentBuilderM where
  fmap = liftM

instance Applicative PdfDocumentBuilderM where
  pure = return
  (<*>) = ap

instance Monad PdfDocumentBuilderM where
  return = PdfDocumentBuilderM []
  PdfDocumentBuilderM actions1 a >>= f =
    let PdfDocumentBuilderM actions2 b = f a
     in PdfDocumentBuilderM (actions1 ++ actions2) b

documentAction :: Action -> PdfDocumentBuilder
documentAction action = PdfDocumentBuilderM [action] ()

-----------------------------------------------
data PdfPageBuilderM a =
  PdfPageBuilderM [Action]
                  a

type PdfPageBuilder = PdfPageBuilderM ()

instance Functor PdfPageBuilderM where
  fmap = liftM

instance Applicative PdfPageBuilderM where
  pure = return
  (<*>) = ap

instance Monad PdfPageBuilderM where
  return = PdfPageBuilderM []
  PdfPageBuilderM actions1 a >>= f =
    let PdfPageBuilderM actions2 b = f a
     in PdfPageBuilderM (actions1 ++ actions2) b

pageAction :: Action -> PdfPageBuilder
pageAction action = PdfPageBuilderM [action] ()

-----------------------------------------------
data PdfTextBuilderM a =
  PdfTextBuilderM [Action]
                  a

type PdfTextBuilder = PdfTextBuilderM ()

instance Functor PdfTextBuilderM where
  fmap = liftM

instance Applicative PdfTextBuilderM where
  pure = return
  (<*>) = ap

instance Monad PdfTextBuilderM where
  return = PdfTextBuilderM []
  PdfTextBuilderM actions1 a >>= f =
    let PdfTextBuilderM actions2 b = f a
     in PdfTextBuilderM (actions1 ++ actions2) b

textAction :: Action -> PdfTextBuilder
textAction action = PdfTextBuilderM [action] ()

-----------------------------------------------
data PdfPathBuilderM a =
  PdfPathBuilderM [Action]
                  a

type PdfPathBuilder = PdfPathBuilderM ()

instance Functor PdfPathBuilderM where
  fmap = liftM

instance Applicative PdfPathBuilderM where
  pure = return
  (<*>) = ap

instance Monad PdfPathBuilderM where
  return = PdfPathBuilderM []
  PdfPathBuilderM actions1 a >>= f =
    let PdfPathBuilderM actions2 b = f a
     in PdfPathBuilderM (actions1 ++ actions2) b

pathAction :: Action -> PdfPathBuilder
pathAction action = PdfPathBuilderM [action] ()

-----------------------------------------------
class ToByteStringLines a where
  toByteStringLines :: a -> [ByteString]

class IsExecutableAction a where
  execute :: a -> PdfDocument -> PdfDocument

class IsStreamContent a where
  toStreamTextLines :: a -> PdfPage -> PdfDocument -> [Text]

-----------------------------------------------
data PdfDocument = PdfDocument
  { pdfDocumentVersion :: Text
  , pdfDocumentHeaderLines :: [ByteString]
  , pdfDocumentNextObjId :: Int
  , pdfDocumentInfo :: PdfInfo
  , pdfDocumentRoot :: PdfRoot
  , pdfDocumentPages :: PdfPages
  , pdfDocumentFonts :: [PdfFont]
  , pdfDocumentTrailer :: PdfTrailer
  , pdfDocumentXref :: PdfXref
  , pdfDocumentStartXref :: Maybe Int
  , pdfDocumentExtGStates :: [PdfExtGState]
  }

instance ToJSON PdfDocument where
  toJSON o =
    object
      [ "version" .= pdfDocumentVersion o
      , "nextObjId" .= pdfDocumentNextObjId o
      , "info" .= pdfDocumentInfo o
      , "root" .= pdfDocumentRoot o
      , "pages" .= pdfDocumentPages o
      , "fonts" .= pdfDocumentFonts o
      , "trailer" .= pdfDocumentTrailer o
      , "xref" .= pdfDocumentXref o
      , "startxref" .= pdfDocumentStartXref o
      , "extGStates" .= pdfDocumentExtGStates o
      ]

instance ToByteStringLines PdfDocument where
  toByteStringLines pdfDoc = headerLines ++ concat objectBlocks ++ footerLines
    where
      (headerLines, objectBlocks, footerLines) =
        pdfDocumentByteStringLineBlocks pdfDoc

initialPdfDocument :: PdfDocument
initialPdfDocument =
  PdfDocument
    { pdfDocumentVersion = version
    , pdfDocumentHeaderLines =
        [ T.encodeUtf8 $ T.concat ["%PDF-", version]
        , B8.pack ['%', '\xff', '\xff', '\xff', '\xff']
        ]
    , pdfDocumentNextObjId = nextObjId
    , pdfDocumentInfo =
        PdfInfo
          { pdfInfoObjId = infoObjId
          , pdfInfoProducer = "hs-pdfkit"
          , pdfInfoCreator = "hs-pdfkit"
          , pdfInfoCreationDate = Nothing
          }
    , pdfDocumentRoot =
        PdfRoot {pdfRootObjId = rootObjId, pdfRootPages = ref pagesObjId}
    , pdfDocumentPages =
        PdfPages {pdfPagesObjId = pagesObjId, pdfPagesKids = []}
    , pdfDocumentFonts = []
    , pdfDocumentXref = PdfXref {pdfXrefPositions = []}
    , pdfDocumentTrailer = PdfTrailer {pdfTrailerSize = Nothing}
    , pdfDocumentStartXref = Nothing
    , pdfDocumentExtGStates = []
    }
  where
    version = "1.4"
    infoObjId = 1
    rootObjId = 2
    pagesObjId = 3
    nextObjId = 4

-----------------------------------------------
data PdfInfo = PdfInfo
  { pdfInfoObjId :: Int
  , pdfInfoProducer :: Text
  , pdfInfoCreator :: Text
  , pdfInfoCreationDate :: Maybe Text
  }

instance ToJSON PdfInfo where
  toJSON o =
    object
      [ "objId" .= pdfInfoObjId o
      , "producer" .= pdfInfoProducer o
      , "creator" .= pdfInfoCreator o
      , "creationDate" .= pdfInfoCreationDate o
      ]

instance ToByteStringLines PdfInfo where
  toByteStringLines pdfInfo =
    [ T.encodeUtf8 $ T.concat [T.pack $ show $ pdfInfoObjId pdfInfo, " 0 obj"]
    , T.encodeUtf8 $
      T.concat
        [ "% ------------------------------------------------------ info "
        , intToText $ pdfInfoObjId pdfInfo
        ]
    , T.encodeUtf8 "<<"
    , T.encodeUtf8 $ T.concat ["/Producer (", pdfInfoProducer pdfInfo, ")"]
    , T.encodeUtf8 $ T.concat ["/Creator (", pdfInfoCreator pdfInfo, ")"]
    ] ++
    (case pdfInfoCreationDate pdfInfo of
       Just creationDate ->
         [T.encodeUtf8 $ T.concat ["/CreationDate (D:", creationDate, "Z)"]]
       _ -> []) ++
    [T.encodeUtf8 ">>", T.encodeUtf8 "endobj"]

-----------------------------------------------
data PdfRoot = PdfRoot
  { pdfRootObjId :: Int
  , pdfRootPages :: Text
  }

instance ToJSON PdfRoot where
  toJSON o = object ["objId" .= pdfRootObjId o, "pages" .= pdfRootPages o]

instance ToByteStringLines (PdfRoot, PdfDocument) where
  toByteStringLines (pdfRoot, pdfDoc) =
    [ T.encodeUtf8 $ T.concat [T.pack $ show $ pdfRootObjId pdfRoot, " 0 obj"]
    , T.encodeUtf8 $
      T.concat
        [ "% ------------------------------------------------------ root "
        , intToText $ pdfRootObjId pdfRoot
        ]
    , T.encodeUtf8 "<<"
    , T.encodeUtf8 "/Type /Catalog"
    , T.encodeUtf8 $
      T.concat ["/Pages ", ref $ pdfPagesObjId $ pdfDocumentPages pdfDoc]
    , T.encodeUtf8 ">>"
    , T.encodeUtf8 "endobj"
    ]

-----------------------------------------------
data PdfPages = PdfPages
  { pdfPagesObjId :: Int
  , pdfPagesKids :: [PdfPage]
  }

instance ToJSON PdfPages where
  toJSON o = object ["objId" .= pdfPagesObjId o, "kids" .= pdfPagesKids o]

instance ToByteStringLines PdfPages where
  toByteStringLines pdfPages =
    [ T.encodeUtf8 $ T.concat [T.pack $ show $ pdfPagesObjId pdfPages, " 0 obj"]
    , T.encodeUtf8 $
      T.concat
        [ "% ------------------------------------------------------ pages "
        , intToText $ pdfPagesObjId pdfPages
        ]
    , T.encodeUtf8 "<<"
    , T.encodeUtf8 "/Type /Pages"
    , T.encodeUtf8 $
      T.concat ["/Count ", intToText $ L.length $ pdfPagesKids pdfPages]
    , T.encodeUtf8 $
      T.concat
        [ "/Kids ["
        , T.intercalate "  " $
          L.map (ref . pdfPageObjId) (pdfPagesKids pdfPages)
        , "]"
        ]
    , T.encodeUtf8 ">>"
    , T.encodeUtf8 "endobj"
    ]

-----------------------------------------------
data PdfExtGState = PdfExtGState
  { pdfExtGStateObjId :: Int
  , pdfExtGStateName :: Text
  , pdfExtGStateFillOpacity :: Maybe Double
  } deriving (Eq)

instance ToJSON PdfExtGState where
  toJSON o =
    object
      [ "objId" .= pdfExtGStateObjId o
      , "name" .= pdfExtGStateName o
      , "fillOpacity" .= pdfExtGStateFillOpacity o
      ]

instance ToByteStringLines PdfExtGState where
  toByteStringLines pdfExtGState =
    [ T.encodeUtf8 $
      T.concat [T.pack $ show $ pdfExtGStateObjId pdfExtGState, " 0 obj"]
    , T.encodeUtf8 $
      T.concat
        [ "% ------------------------------------------------------ ExtGState "
        , intToText $ pdfExtGStateObjId pdfExtGState
        ]
    , T.encodeUtf8 "<<"
    , T.encodeUtf8 "/Type /ExtGState"
    ] ++
    (case pdfExtGStateFillOpacity pdfExtGState of
       Just x -> [T.encodeUtf8 $ T.concat ["/ca ", doubleToText x]]
       _ -> []) ++
    [T.encodeUtf8 ">>", T.encodeUtf8 "endobj"]

-----------------------------------------------
data PdfFont = PdfFont
  { pdfFontObjId :: Int
  , pdfFontName :: Text
  , pdfFontStandardFont :: PdfStandardFont
  } deriving (Eq)

instance ToJSON PdfFont where
  toJSON o =
    object
      [ "objId" .= pdfFontObjId o
      , "name" .= pdfFontName o
      , "standardFont" .= pdfFontStandardFont o
      ]

instance ToByteStringLines PdfFont where
  toByteStringLines pdfFont =
    [ T.encodeUtf8 $ T.concat [T.pack $ show $ pdfFontObjId pdfFont, " 0 obj"]
    , T.encodeUtf8 $
      T.concat
        [ "% ------------------------------------------------------ Font "
        , intToText $ pdfFontObjId pdfFont
        ]
    , T.encodeUtf8 "<<"
    , T.encodeUtf8 "/Type /Font"
    , T.encodeUtf8 $ T.concat ["/Name /", pdfFontName pdfFont]
    , T.encodeUtf8 $ T.concat ["/BaseFont /", pdfStandardFontBaseFont stdFont]
    , T.encodeUtf8 $ T.concat ["/Subtype /", pdfStandardFontSubtype stdFont]
    , T.encodeUtf8 $ T.concat ["/Encoding /", pdfStandardFontEncoding stdFont]
    , T.encodeUtf8 ">>"
    , T.encodeUtf8 "endobj"
    ]
    where
      stdFont = pdfFontStandardFont pdfFont

-----------------------------------------------
data PdfResources = PdfResources
  { pdfResourcesObjId :: Int
  , pdfResourcesFonts :: [PdfFont]
  , pdfResourcesExtGStates :: [PdfExtGState]
  }

instance ToJSON PdfResources where
  toJSON o =
    object
      [ "objId" .= pdfResourcesObjId o
      , "fonts" .= pdfResourcesFonts o
      , "extGStates" .= pdfResourcesExtGStates o
      ]

instance ToByteStringLines PdfResources where
  toByteStringLines pdfResources =
    [ T.encodeUtf8 $
      T.concat [T.pack $ show $ pdfResourcesObjId pdfResources, " 0 obj"]
    , T.encodeUtf8 $
      T.concat
        [ "% ------------------------------------------------------ Resources "
        , intToText $ pdfResourcesObjId pdfResources
        ]
    , T.encodeUtf8 "<<"
    , T.encodeUtf8 "/ProcSet [/PDF /Text /ImageB /ImageC /ImageI]"
    ] ++
    (case pdfResourcesFonts pdfResources of
       [] -> []
       pdfFonts ->
         [T.encodeUtf8 "/Font <<"] ++
         L.map
           (\pdfFont ->
              T.encodeUtf8 $
              T.concat
                ["/", pdfFontName pdfFont, " ", ref $ pdfFontObjId pdfFont])
           pdfFonts ++
         [T.encodeUtf8 ">>"]) ++
    (case pdfResourcesExtGStates pdfResources of
       [] -> []
       pdfExtGStates ->
         [T.encodeUtf8 "/ExtGState <<"] ++
         L.map
           (\pdfExtGState ->
              T.encodeUtf8 $
              T.concat
                [ "/"
                , pdfExtGStateName pdfExtGState
                , " "
                , ref $ pdfExtGStateObjId pdfExtGState
                ])
           pdfExtGStates ++
         [T.encodeUtf8 ">>"]) ++
    [T.encodeUtf8 ">>", T.encodeUtf8 "endobj"]

-----------------------------------------------
data PdfPage = PdfPage
  { pdfPageObjId :: Int
  , pdfPageSize :: PdfPageSize
  , pdfPageMargins :: PdfPageMargins
  , pdfPageLayout :: PdfPageLayout
  , pdfPageResources :: PdfResources
  , pdfPageContents :: PdfContents
  , pdfPageCurrentPos :: PdfPos
  }

instance ToJSON PdfPage where
  toJSON o =
    object
      [ "objId" .= pdfPageObjId o
      , "size" .= pdfPageSize o
      , "margins" .= pdfPageMargins o
      , "layout" .=
        T.pack
          (case pdfPageLayout o of
             Portrait -> "portrait"
             Landscape -> "landscape")
      , "resources" .= pdfPageResources o
      , "contents" .= pdfPageContents o
      , "currentPos" .= pdfPageCurrentPos o
      ]

instance ToByteStringLines (PdfPage, PdfDocument) where
  toByteStringLines (pdfPage, pdfDoc) =
    [ T.encodeUtf8 $ T.concat [T.pack $ show $ pdfPageObjId pdfPage, " 0 obj"]
    , T.encodeUtf8 $
      T.concat
        [ "% ------------------------------------------------------ Page "
        , intToText $ pdfPageObjId pdfPage
        ]
    , T.encodeUtf8 "<<"
    , T.encodeUtf8 "/Type /Page"
    , T.encodeUtf8 $
      T.concat ["/Parent ", ref $ pdfPagesObjId $ pdfDocumentPages pdfDoc]
    , T.encodeUtf8 $ T.concat ["% ", T.pack $ show $ pdfPageLayout pdfPage]
    , T.encodeUtf8 $
      T.concat
        [ "/MediaBox [0 0 "
        , doubleToText $ pdfPageSizeWidth $ pdfPageSize pdfPage
        , " "
        , doubleToText $ pdfPageSizeHeight $ pdfPageSize pdfPage
        , "]"
        ]
    , T.encodeUtf8 $
      T.concat
        ["/Resources ", ref $ pdfResourcesObjId $ pdfPageResources pdfPage]
    , T.encodeUtf8 $
      T.concat ["/Contents ", ref $ pdfContentsObjId $ pdfPageContents pdfPage]
    , T.encodeUtf8 ">>"
    , T.encodeUtf8 "endobj"
    ]

-----------------------------------------------
data PdfContents = PdfContents
  { pdfContentsObjId :: Int
  , pdfContentsStreamContents :: [PdfStreamContent]
  }

instance ToJSON PdfContents where
  toJSON o =
    object
      ["objId" .= pdfContentsObjId o, "texts" .= pdfContentsStreamContents o]

instance ToByteStringLines (PdfContents, PdfPage, PdfDocument) where
  toByteStringLines (pdfContents, pdfPage, pdfDoc) =
    [ T.encodeUtf8 $
      T.concat [T.pack $ show $ pdfContentsObjId pdfContents, " 0 obj"]
    , T.encodeUtf8 $
      T.concat
        [ "% ------------------------------------------------------ Contents "
        , intToText $ pdfContentsObjId pdfContents
        ]
    , T.encodeUtf8 "<<"
    , T.encodeUtf8 $ T.concat ["/Length ", intToText streamLength]
    , T.encodeUtf8 ">>"
    , T.encodeUtf8 "stream"
    , T.encodeUtf8 $ translateOrigin pageHeight
    ] ++
    map T.encodeUtf8 streamTextLines ++ [T.encodeUtf8 "endstream"]
    where
      PdfPageSize {pdfPageSizeHeight = pageHeight} = pdfPageSize pdfPage
      streamTextLines =
        L.foldl
          (\acc pdfStreamContent ->
             acc ++ toStreamTextLines pdfStreamContent pdfPage pdfDoc)
          []
          (pdfContentsStreamContents pdfContents)
      streamLength = BS.length $ T.encodeUtf8 $ T.unlines streamTextLines

-----------------------------------------------
instance IsStreamContent PdfStreamContent where
  toStreamTextLines pdfText@PdfText {} pdfPage pdfDoc =
    case pdfTextText pdfText of
      Just t ->
        ["q", translateOrigin pageHeight, "BT"] ++
        (case pdfTextColor pdfText of
           Just (PdfColorRgb r g b) ->
             [ "/DeviceRGB cs"
             , T.intercalate " " $ L.map doubleToText [r, g, b] ++ ["scn"]
             ]
           Just (PdfColorCmyk c m y k) ->
             [ "/DeviceCMYK cs"
             , T.intercalate " " $ L.map doubleToText [c, m, y, k] ++ ["scn"]
             ]
           _ -> []) ++
        (case pdfTextFillOpacity pdfText of
           Just fillOpacity ->
             case findPdfExtGState fillOpacity pdfDoc of
               Just pdfExtGState ->
                 [T.concat ["/", pdfExtGStateName pdfExtGState, " gs"]]
               _ -> []
           _ -> []) ++
        [ translatePos
        , T.concat
            [ "/"
            , case pdfTextStandardFont pdfText of
                Just stdFont ->
                  case findPdfFont stdFont pdfDoc of
                    Just pdfFont -> pdfFontName pdfFont
                    _ -> ""
                _ -> ""
            , " "
            , case pdfTextFontSize pdfText of
                Just fontSize -> doubleToText fontSize
                _ -> ""
            , " Tf"
            ]
        , T.concat
            ["[<", T.decodeUtf8 $ H.hex $ B8.pack $ T.unpack t, "> 0] TJ"]
        , "ET"
        , "Q"
        ] :: [Text]
      _ -> []
    where
      PdfPageSize {pdfPageSizeHeight = pageHeight} = pdfPageSize pdfPage
      translatePos =
        T.concat
          [ "1 0 0 1 "
          , doubleToText $ pdfTextX pdfText
          , " "
          , case (pdfTextStandardFont pdfText, pdfTextFontSize pdfText) of
              (Just stdFont, Just fontSize) ->
                doubleToText $
                pageHeight - pdfTextY pdfText - dy stdFont fontSize
              _ -> ""
          , " Tm"
          ]
  toStreamTextLines pdfPath@PdfPath {} _ _ =
    case pdfPathPoints pdfPath of
      [] -> []
      PdfPos x y:followingPoints ->
        [T.concat [doubleToText x, " ", doubleToText y, " m"]] ++
        L.map
          (\(PdfPos x' y') ->
             T.concat [doubleToText x', " ", doubleToText y', " l"])
          followingPoints ++
        case pdfPathDoStroke pdfPath of
          Just True -> ["S"]
          _ -> []

-----------------------------------------------
newtype PdfXref = PdfXref
  { pdfXrefPositions :: [Int]
  }

instance ToJSON PdfXref where
  toJSON o = object ["positions" .= pdfXrefPositions o]

instance ToByteStringLines (PdfXref, PdfDocument) where
  toByteStringLines (pdfXref, pdfDoc) =
    [ T.encodeUtf8 "xref"
    , T.encodeUtf8
        "% ------------------------------------------------------ xref"
    , T.encodeUtf8 $
      T.concat
        [ "0 "
        , case pdfTrailerSize $ pdfDocumentTrailer pdfDoc of
            Just size -> intToText size
            _ -> ""
        ]
    , T.encodeUtf8 "0000000000 65535 f"
    ] ++
    xrefLines
    where
      xrefLines =
        L.map (\pos -> T.encodeUtf8 $ T.concat [formatXrefPos pos, " 00000 n"]) $
        pdfXrefPositions pdfXref

-----------------------------------------------
newtype PdfTrailer = PdfTrailer
  { pdfTrailerSize :: Maybe Int
  }

instance ToJSON PdfTrailer where
  toJSON o = object ["size" .= pdfTrailerSize o]

instance ToByteStringLines (PdfTrailer, PdfDocument) where
  toByteStringLines (pdfTrailer, pdfDoc) =
    [ T.encodeUtf8 "trailer"
    , T.encodeUtf8
        "% ------------------------------------------------------ trailer"
    , T.encodeUtf8 "<<"
    , T.encodeUtf8 $
      T.concat ["/Size ", maybeIntToText $ pdfTrailerSize pdfTrailer]
    , T.encodeUtf8 $
      T.concat ["/Root ", ref $ pdfRootObjId $ pdfDocumentRoot pdfDoc]
    , T.encodeUtf8 $
      T.concat ["/Info ", ref $ pdfInfoObjId $ pdfDocumentInfo pdfDoc]
    , T.encodeUtf8 ">>"
    ]

-----------------------------------------------
findPdfFont :: PdfStandardFont -> PdfDocument -> Maybe PdfFont
findPdfFont stdFont pdfDoc =
  L.find
    (\pdfFont -> pdfFontStandardFont pdfFont == stdFont)
    (pdfDocumentFonts pdfDoc)

findPdfExtGState :: Double -> PdfDocument -> Maybe PdfExtGState
findPdfExtGState fillOpacity pdfDoc =
  L.find
    (\pdfExtGState -> pdfExtGStateFillOpacity pdfExtGState == Just fillOpacity)
    (pdfDocumentExtGStates pdfDoc)

-----------------------------------------------
pdfPagesTuple :: PdfDocument -> (PdfPages, [PdfPage], PdfPage)
pdfPagesTuple pdfDoc = (pdfPages, initPages, lastPage)
  where
    pdfPages = pdfDocumentPages pdfDoc
    initPages = L.init $ pdfPagesKids pdfPages
    lastPage = L.last $ pdfPagesKids pdfPages

pdfTextsTuple ::
     PdfPage -> ([PdfStreamContent], PdfStreamContent, [PdfStreamContent])
pdfTextsTuple = pdfStreamContentTuple isPdfText

pdfPathsTuple ::
     PdfPage -> ([PdfStreamContent], PdfStreamContent, [PdfStreamContent])
pdfPathsTuple = pdfStreamContentTuple isPdfPath

pdfStreamContentTuple ::
     (PdfStreamContent -> Bool)
  -> PdfPage
  -> ([PdfStreamContent], PdfStreamContent, [PdfStreamContent])
pdfStreamContentTuple predicate pdfPage =
  (prefixContents, lastContent, suffixContents)
  where
    streamContents = pdfContentsStreamContents $ pdfPageContents pdfPage
    revertedStreamContents = L.reverse streamContents
    Just lastContent = L.find predicate revertedStreamContents
    prefixContents = L.takeWhile (/= lastContent) streamContents
    suffixContents =
      L.reverse $ L.takeWhile (/= lastContent) revertedStreamContents

pdfDocumentByteStringLineBlocks ::
     PdfDocument -> ([ByteString], [[ByteString]], [ByteString])
pdfDocumentByteStringLineBlocks pdfDoc =
  (headerLines, refLineBlocks, footerLines)
  where
    headerLines = pdfDocumentHeaderLines pdfDoc
    refLineBlocks =
      [ toByteStringLines $ pdfDocumentInfo pdfDoc
      , toByteStringLines (pdfDocumentRoot pdfDoc, pdfDoc)
      , toByteStringLines $ pdfDocumentPages pdfDoc
      ] ++
      L.foldl
        (\acc pdfPage ->
           [ toByteStringLines (pdfPage, pdfDoc)
           , toByteStringLines $ pdfPageResources pdfPage
           , toByteStringLines (pdfPageContents pdfPage, pdfPage, pdfDoc)
           ] ++
           acc)
        []
        (pdfPagesKids $ pdfDocumentPages pdfDoc) ++
      L.map toByteStringLines (pdfDocumentFonts pdfDoc) ++
      L.map toByteStringLines (pdfDocumentExtGStates pdfDoc)
    footerLines =
      toByteStringLines (pdfDocumentXref pdfDoc, pdfDoc) ++
      toByteStringLines (pdfDocumentTrailer pdfDoc, pdfDoc) ++
      [ T.encodeUtf8 "startxref"
      , T.encodeUtf8 $ maybeIntToText $ pdfDocumentStartXref pdfDoc
      , T.encodeUtf8 "%%EOF"
      ]

-----------------------------------------------
data Action
  = ActionComposite [Action]
  | ActionInfoSetProducer Text
  | ActionInfoSetCreator Text
  | ActionInfoSetCreationDate UTCTime
                              TimeZone
  | ActionFinalize
  | ActionPage
  | ActionPageSetSize PdfPageSize
  | ActionPageSetLayout PdfPageLayout
  | ActionPageSetMargin Double
  | ActionPageSetMargins Double
                         Double
                         Double
                         Double
  | ActionPageSetSizeCustom Double
                            Double
  | ActionText
  | ActionTextContent Text
  | ActionTextFont PdfStandardFont
  | ActionTextFontSize Double
  | ActionTextPos Double
                  Double
  | ActionTextColor PdfColor
  | ActionTextFillOpacity Double
  | ActionMoveDown
  | ActionPath
  | ActionPathPoint Double
                    Double
  | ActionPathStroke

-----------------------------------------------
instance IsExecutableAction Action where
  execute (ActionComposite actions) pdfDoc = pdfDoc'
    where
      pdfDoc' = L.foldl (flip execute) pdfDoc actions
  execute (ActionInfoSetProducer text) pdfDoc =
    pdfDoc {pdfDocumentInfo = (pdfDocumentInfo pdfDoc) {pdfInfoProducer = text}}
  execute (ActionInfoSetCreator text) pdfDoc =
    pdfDoc {pdfDocumentInfo = (pdfDocumentInfo pdfDoc) {pdfInfoCreator = text}}
  execute (ActionInfoSetCreationDate now timeZone) pdfDoc =
    pdfDoc
      { pdfDocumentInfo =
          (pdfDocumentInfo pdfDoc) {pdfInfoCreationDate = Just creationDate}
      }
    where
      creationDate = T.pack $ formatLocalTime timeZone now
  execute ActionFinalize pdfDoc =
    pdfDoc
      { pdfDocumentXref = PdfXref {pdfXrefPositions = L.init positions}
      , pdfDocumentStartXref = Just $ L.last positions
      , pdfDocumentTrailer =
          (pdfDocumentTrailer pdfDoc)
            {pdfTrailerSize = Just $ pdfDocumentNextObjId pdfDoc}
      }
    where
      (headerLines, objectBlocks, _) = pdfDocumentByteStringLineBlocks pdfDoc
      headerLength = BS.length $ B8.unlines headerLines
      objectLines = L.map B8.unlines objectBlocks
      lengths = L.map BS.length objectLines
      (positions, _) =
        L.foldl
          (\(ls, accl) len -> (ls ++ [accl + len], accl + len))
          ([headerLength], headerLength)
          lengths
  execute ActionPage pdfDoc =
    pdfDoc
      { pdfDocumentNextObjId = nextObjId
      , pdfDocumentPages =
          (pdfDocumentPages pdfDoc)
            { pdfPagesKids =
                pdfPagesKids (pdfDocumentPages pdfDoc) ++
                [ PdfPage
                    { pdfPageObjId = pageObjId
                    , pdfPageSize = defaultPageSize
                    , pdfPageMargins = defaultPageMargins
                    , pdfPageLayout = Portrait
                    , pdfPageResources =
                        PdfResources
                          { pdfResourcesObjId = resourcesObjId
                          , pdfResourcesFonts = []
                          , pdfResourcesExtGStates = []
                          }
                    , pdfPageContents =
                        PdfContents
                          { pdfContentsObjId = contentsObjId
                          , pdfContentsStreamContents = []
                          }
                    , pdfPageCurrentPos =
                        PdfPos
                          { pdfPosX = pdfPageMarginLeft defaultPageMargins
                          , pdfPosY = pdfPageMarginTop defaultPageMargins
                          }
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
          pdfPages {pdfPagesKids = initPages ++ [lastPage {pdfPageSize = size}]}
      }
    where
      (pdfPages, initPages, lastPage) = pdfPagesTuple pdfDoc
  execute (ActionPageSetSizeCustom width height) pdfDoc =
    pdfDoc
      { pdfDocumentPages =
          pdfPages
            { pdfPagesKids =
                initPages ++
                [ lastPage
                    { pdfPageSize =
                        PdfPageSize
                          {pdfPageSizeWidth = width, pdfPageSizeHeight = height}
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
                initPages ++
                [lastPage {pdfPageLayout = lay, pdfPageSize = pageSize}]
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
                initPages ++
                [lastPage {pdfPageMargins = PdfPageMargins x x x x}]
            }
      }
    where
      (pdfPages, initPages, lastPage) = pdfPagesTuple pdfDoc
  execute (ActionPageSetMargins top left bottom right) pdfDoc =
    pdfDoc
      { pdfDocumentPages =
          pdfPages
            { pdfPagesKids =
                initPages ++
                [ lastPage
                    {pdfPageMargins = PdfPageMargins top left bottom right}
                ]
            }
      }
    where
      (pdfPages, initPages, lastPage) = pdfPagesTuple pdfDoc
  execute ActionText pdfDoc =
    pdfDoc
      { pdfDocumentPages =
          pdfPages
            { pdfPagesKids =
                initPages ++
                [ lastPage
                    { pdfPageContents =
                        (pdfPageContents lastPage)
                          { pdfContentsStreamContents =
                              pdfContentsStreamContents
                                (pdfPageContents lastPage) ++
                              [ PdfText
                                  { pdfTextText = Nothing
                                  , pdfTextX = x
                                  , pdfTextY = y
                                  , pdfTextStandardFont = Nothing
                                  , pdfTextFontSize = Nothing
                                  , pdfTextColor = Nothing
                                  , pdfTextFillOpacity = Nothing
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
      PdfPos x y = pdfPageCurrentPos lastPage
  execute (ActionTextContent t) pdfDoc =
    pdfDoc
      { pdfDocumentPages =
          pdfPages
            { pdfPagesKids =
                initPages ++
                [ lastPage
                    { pdfPageContents =
                        (pdfPageContents lastPage)
                          { pdfContentsStreamContents =
                              prefixContents ++
                              [lastText {pdfTextText = Just t}] ++
                              suffixContents
                          }
                    }
                ]
            }
      }
    where
      (pdfPages, initPages, lastPage) = pdfPagesTuple pdfDoc
      (prefixContents, lastText, suffixContents) = pdfTextsTuple lastPage
  execute (ActionTextFont stdFont) pdfDoc =
    pdfDoc
      { pdfDocumentNextObjId = nextObjId
      , pdfDocumentFonts = L.nub $ pdfDocumentFonts pdfDoc ++ [pdfFont]
      , pdfDocumentPages =
          pdfPages
            { pdfPagesKids =
                initPages ++
                [ lastPage
                    { pdfPageResources =
                        (pdfPageResources lastPage)
                          { pdfResourcesFonts =
                              L.nub $
                              (pdfResourcesFonts . pdfPageResources $ lastPage) ++
                              [pdfFont]
                          }
                    , pdfPageContents =
                        (pdfPageContents lastPage)
                          { pdfContentsStreamContents =
                              prefixContents ++
                              [lastText {pdfTextStandardFont = Just stdFont}] ++
                              suffixContents
                          }
                    }
                ]
            }
      }
    where
      fontObjId = pdfDocumentNextObjId pdfDoc
      maybePdfFont = findPdfFont stdFont pdfDoc
      fontAlreadyAdded = M.isJust maybePdfFont
      pdfFont =
        case maybePdfFont of
          Just pdfFont' -> pdfFont'
          _ ->
            PdfFont
              { pdfFontObjId = fontObjId
              , pdfFontName = T.concat ["F", intToText fontObjId]
              , pdfFontStandardFont = stdFont
              }
      nextObjId =
        if fontAlreadyAdded
          then pdfDocumentNextObjId pdfDoc
          else pdfDocumentNextObjId pdfDoc + 1
      (pdfPages, initPages, lastPage) = pdfPagesTuple pdfDoc
      (prefixContents, lastText, suffixContents) = pdfTextsTuple lastPage
  execute (ActionTextFontSize fontSize) pdfDoc =
    pdfDoc
      { pdfDocumentPages =
          pdfPages
            { pdfPagesKids =
                initPages ++
                [ lastPage
                    { pdfPageContents =
                        (pdfPageContents lastPage)
                          { pdfContentsStreamContents =
                              prefixContents ++
                              [lastText {pdfTextFontSize = Just fontSize}] ++
                              suffixContents
                          }
                    }
                ]
            }
      }
    where
      (pdfPages, initPages, lastPage) = pdfPagesTuple pdfDoc
      (prefixContents, lastText, suffixContents) = pdfTextsTuple lastPage
  execute (ActionTextPos x y) pdfDoc =
    pdfDoc
      { pdfDocumentPages =
          pdfPages
            { pdfPagesKids =
                initPages ++
                [ lastPage
                    { pdfPageContents =
                        (pdfPageContents lastPage)
                          { pdfContentsStreamContents =
                              prefixContents ++
                              [lastText {pdfTextX = x, pdfTextY = y}] ++
                              suffixContents
                          }
                    }
                ]
            }
      }
    where
      (pdfPages, initPages, lastPage) = pdfPagesTuple pdfDoc
      (prefixContents, lastText, suffixContents) = pdfTextsTuple lastPage
  execute (ActionTextColor color) pdfDoc =
    pdfDoc
      { pdfDocumentPages =
          pdfPages
            { pdfPagesKids =
                initPages ++
                [ lastPage
                    { pdfPageContents =
                        (pdfPageContents lastPage)
                          { pdfContentsStreamContents =
                              prefixContents ++
                              [lastText {pdfTextColor = Just color}] ++
                              suffixContents
                          }
                    }
                ]
            }
      }
    where
      (pdfPages, initPages, lastPage) = pdfPagesTuple pdfDoc
      (prefixContents, lastText, suffixContents) = pdfTextsTuple lastPage
  execute (ActionTextFillOpacity fillOpacity) pdfDoc =
    pdfDoc
      { pdfDocumentNextObjId = nextObjId
      , pdfDocumentExtGStates =
          L.nub $ pdfDocumentExtGStates pdfDoc ++ [pdfExtGState]
      , pdfDocumentPages =
          pdfPages
            { pdfPagesKids =
                initPages ++
                [ lastPage
                    { pdfPageResources =
                        (pdfPageResources lastPage)
                          { pdfResourcesExtGStates =
                              L.nub $
                              (pdfResourcesExtGStates . pdfPageResources $
                               lastPage) ++
                              [pdfExtGState]
                          }
                    , pdfPageContents =
                        (pdfPageContents lastPage)
                          { pdfContentsStreamContents =
                              prefixContents ++
                              [lastText {pdfTextFillOpacity = Just fillOpacity}] ++
                              suffixContents
                          }
                    }
                ]
            }
      }
    where
      extGStateObjId = pdfDocumentNextObjId pdfDoc
      maybePdfExtGState = findPdfExtGState fillOpacity pdfDoc
      extGStateAlreadyAdded = M.isJust maybePdfExtGState
      pdfExtGState =
        case maybePdfExtGState of
          Just pdfExtGState' -> pdfExtGState'
          _ ->
            PdfExtGState
              { pdfExtGStateObjId = extGStateObjId
              , pdfExtGStateName = T.concat ["GS", intToText extGStateObjId]
              , pdfExtGStateFillOpacity = Just fillOpacity
              }
      nextObjId =
        if extGStateAlreadyAdded
          then pdfDocumentNextObjId pdfDoc
          else pdfDocumentNextObjId pdfDoc + 1
      (pdfPages, initPages, lastPage) = pdfPagesTuple pdfDoc
      (prefixContents, lastText, suffixContents) = pdfTextsTuple lastPage
  execute ActionMoveDown pdfDoc =
    pdfDoc
      { pdfDocumentPages =
          pdfPages
            { pdfPagesKids =
                initPages ++
                [ lastPage
                    { pdfPageCurrentPos =
                        PdfPos
                          (pdfTextX lastText)
                          (pdfTextY lastText + currentLineHeight)
                    }
                ]
            }
      }
    where
      (pdfPages, initPages, lastPage) = pdfPagesTuple pdfDoc
      (_, lastText, _) = pdfTextsTuple lastPage
      currentLineHeight =
        case (pdfTextStandardFont lastText, pdfTextFontSize lastText) of
          (Just stdFont, Just fontSize) -> fontLineHeight stdFont fontSize
          _ -> 0
  execute ActionPath pdfDoc =
    pdfDoc
      { pdfDocumentPages =
          pdfPages
            { pdfPagesKids =
                initPages ++
                [ lastPage
                    { pdfPageContents =
                        (pdfPageContents lastPage)
                          { pdfContentsStreamContents =
                              pdfContentsStreamContents
                                (pdfPageContents lastPage) ++
                              [ PdfPath
                                  { pdfPathPoints = []
                                  , pdfPathDoStroke = Nothing
                                  }
                              ]
                          }
                    }
                ]
            }
      }
    where
      (pdfPages, initPages, lastPage) = pdfPagesTuple pdfDoc
  execute (ActionPathPoint x y) pdfDoc =
    pdfDoc
      { pdfDocumentPages =
          pdfPages
            { pdfPagesKids =
                initPages ++
                [ lastPage
                    { pdfPageContents =
                        (pdfPageContents lastPage)
                          { pdfContentsStreamContents =
                              prefixContents ++
                              [ lastPath
                                  { pdfPathPoints =
                                      pdfPathPoints lastPath ++ [PdfPos x y]
                                  }
                              ] ++
                              suffixContents
                          }
                    }
                ]
            }
      }
    where
      (pdfPages, initPages, lastPage) = pdfPagesTuple pdfDoc
      (prefixContents, lastPath, suffixContents) = pdfPathsTuple lastPage
  execute ActionPathStroke pdfDoc =
    pdfDoc
      { pdfDocumentPages =
          pdfPages
            { pdfPagesKids =
                initPages ++
                [ lastPage
                    { pdfPageContents =
                        (pdfPageContents lastPage)
                          { pdfContentsStreamContents =
                              prefixContents ++
                              [lastPath {pdfPathDoStroke = Just True}] ++
                              suffixContents
                          }
                    }
                ]
            }
      }
    where
      (pdfPages, initPages, lastPage) = pdfPagesTuple pdfDoc
      (prefixContents, lastPath, suffixContents) = pdfPathsTuple lastPage

{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}

module PdfKit.Builder where

import Control.Monad
import Data.Aeson ((.=), ToJSON, object, toJSON)
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
import Lens.Micro.Platform hiding ((.=))
import PdfKit.Helper

-----------------------------------------------
data PdfDocument = PdfDocument
  { _pdfDocumentVersion :: Text,
    _pdfDocumentHeaderLines :: [ByteString],
    _pdfDocumentNextObjId :: Int,
    _pdfDocumentInfo :: PdfInfo,
    _pdfDocumentRoot :: PdfRoot,
    _pdfDocumentPages :: PdfPages,
    _pdfDocumentFonts :: [PdfFont],
    _pdfDocumentTrailer :: PdfTrailer,
    _pdfDocumentXref :: PdfXref,
    _pdfDocumentStartXref :: Maybe Int,
    _pdfDocumentExtGStates :: [PdfExtGState]
  }

data PdfInfo = PdfInfo
  { _pdfInfoObjId :: Int,
    _pdfInfoProducer :: Text,
    _pdfInfoCreator :: Text,
    _pdfInfoCreationDate :: Maybe Text
  }

data PdfRoot = PdfRoot
  { pdfRootObjId :: Int,
    pdfRootPages :: Text
  }

data PdfPages = PdfPages
  { _pdfPagesObjId :: Int,
    _pdfPagesKids :: [PdfPage]
  }

data PdfExtGState = PdfExtGState
  { pdfExtGStateObjId :: Int,
    pdfExtGStateName :: Text,
    pdfExtGStateFillOpacity :: Maybe Double
  }
  deriving (Eq)

data PdfFont = PdfFont
  { pdfFontObjId :: Int,
    pdfFontName :: Text,
    pdfFontStandardFont :: PdfStandardFont
  }
  deriving (Eq)

data PdfResources = PdfResources
  { pdfResourcesObjId :: Int,
    pdfResourcesFonts :: [PdfFont],
    pdfResourcesExtGStates :: [PdfExtGState]
  }

data PdfPage = PdfPage
  { pdfPageObjId :: Int,
    pdfPageSize :: PdfPageSize,
    pdfPageMargins :: PdfPageMargins,
    pdfPageLayout :: PdfPageLayout,
    pdfPageResources :: PdfResources,
    pdfPageContents :: PdfContents,
    pdfPageCurrentPos :: PdfPos
  }

data PdfContents = PdfContents
  { pdfContentsObjId :: Int,
    pdfContentsStreamContents :: [PdfStreamContent]
  }

newtype PdfXref = PdfXref
  { _pdfXrefPositions :: [Int]
  }

newtype PdfTrailer = PdfTrailer
  { _pdfTrailerSize :: Maybe Int
  }

makeLenses ''PdfDocument

makeLenses ''PdfPages

makeLenses ''PdfInfo

makeLenses ''PdfXref

makeLenses ''PdfTrailer

-----------------------------------------------

data PdfDocumentBuilderM a
  = PdfDocumentBuilderM
      [Action]
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
data PdfPageBuilderM a
  = PdfPageBuilderM
      [Action]
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
data PdfTextBuilderM a
  = PdfTextBuilderM
      [Action]
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
data PdfPathBuilderM a
  = PdfPathBuilderM
      [Action]
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

instance ToJSON PdfDocument where
  toJSON o =
    object
      [ "version" .= _pdfDocumentVersion o,
        "nextObjId" .= _pdfDocumentNextObjId o,
        "info" .= _pdfDocumentInfo o,
        "root" .= _pdfDocumentRoot o,
        "pages" .= _pdfDocumentPages o,
        "fonts" .= _pdfDocumentFonts o,
        "trailer" .= _pdfDocumentTrailer o,
        "xref" .= _pdfDocumentXref o,
        "startxref" .= _pdfDocumentStartXref o,
        "extGStates" .= _pdfDocumentExtGStates o
      ]

instance ToByteStringLines PdfDocument where
  toByteStringLines pdfDoc = headerLines ++ concat objectBlocks ++ footerLines
    where
      (headerLines, objectBlocks, footerLines) =
        pdfDocumentByteStringLineBlocks pdfDoc

initialPdfDocument :: PdfDocument
initialPdfDocument =
  PdfDocument
    { _pdfDocumentVersion = version,
      _pdfDocumentHeaderLines =
        [ T.encodeUtf8 $ T.concat ["%PDF-", version],
          B8.pack ['%', '\xff', '\xff', '\xff', '\xff']
        ],
      _pdfDocumentNextObjId = nextObjId,
      _pdfDocumentInfo =
        PdfInfo
          { _pdfInfoObjId = infoObjId,
            _pdfInfoProducer = "hs-pdfkit",
            _pdfInfoCreator = "hs-pdfkit",
            _pdfInfoCreationDate = Nothing
          },
      _pdfDocumentRoot =
        PdfRoot {pdfRootObjId = rootObjId, pdfRootPages = ref pagesObjId},
      _pdfDocumentPages =
        PdfPages {_pdfPagesObjId = pagesObjId, _pdfPagesKids = []},
      _pdfDocumentFonts = [],
      _pdfDocumentXref = PdfXref {_pdfXrefPositions = []},
      _pdfDocumentTrailer = PdfTrailer {_pdfTrailerSize = Nothing},
      _pdfDocumentStartXref = Nothing,
      _pdfDocumentExtGStates = []
    }
  where
    version = "1.4"
    infoObjId = 1
    rootObjId = 2
    pagesObjId = 3
    nextObjId = 4

-----------------------------------------------

instance ToJSON PdfInfo where
  toJSON o =
    object
      [ "objId" .= _pdfInfoObjId o,
        "producer" .= _pdfInfoProducer o,
        "creator" .= _pdfInfoCreator o,
        "creationDate" .= _pdfInfoCreationDate o
      ]

instance ToByteStringLines PdfInfo where
  toByteStringLines pdfInfo =
    [ T.encodeUtf8 $ T.concat [T.pack $ show $ _pdfInfoObjId pdfInfo, " 0 obj"],
      T.encodeUtf8 $
        T.concat
          [ "% ------------------------------------------------------ info ",
            intToText $ _pdfInfoObjId pdfInfo
          ],
      T.encodeUtf8 "<<",
      T.encodeUtf8 $ T.concat ["/Producer (", _pdfInfoProducer pdfInfo, ")"],
      T.encodeUtf8 $ T.concat ["/Creator (", _pdfInfoCreator pdfInfo, ")"]
    ]
      ++ ( case _pdfInfoCreationDate pdfInfo of
             Just creationDate ->
               [T.encodeUtf8 $ T.concat ["/CreationDate (D:", creationDate, "Z)"]]
             _ -> []
         )
      ++ [T.encodeUtf8 ">>", T.encodeUtf8 "endobj"]

-----------------------------------------------

instance ToJSON PdfRoot where
  toJSON o = object ["objId" .= pdfRootObjId o, "pages" .= pdfRootPages o]

instance ToByteStringLines (PdfRoot, PdfDocument) where
  toByteStringLines (pdfRoot, pdfDoc) =
    [ T.encodeUtf8 $ T.concat [T.pack $ show $ pdfRootObjId pdfRoot, " 0 obj"],
      T.encodeUtf8 $
        T.concat
          [ "% ------------------------------------------------------ root ",
            intToText $ pdfRootObjId pdfRoot
          ],
      T.encodeUtf8 "<<",
      T.encodeUtf8 "/Type /Catalog",
      T.encodeUtf8 $
        T.concat ["/Pages ", ref $ _pdfPagesObjId $ _pdfDocumentPages pdfDoc],
      T.encodeUtf8 ">>",
      T.encodeUtf8 "endobj"
    ]

-----------------------------------------------
instance ToJSON PdfPages where
  toJSON o = object ["objId" .= _pdfPagesObjId o, "kids" .= _pdfPagesKids o]

instance ToByteStringLines PdfPages where
  toByteStringLines pdfPages =
    [ T.encodeUtf8 $ T.concat [T.pack $ show $ _pdfPagesObjId pdfPages, " 0 obj"],
      T.encodeUtf8 $
        T.concat
          [ "% ------------------------------------------------------ pages ",
            intToText $ _pdfPagesObjId pdfPages
          ],
      T.encodeUtf8 "<<",
      T.encodeUtf8 "/Type /Pages",
      T.encodeUtf8 $
        T.concat ["/Count ", intToText $ L.length $ _pdfPagesKids pdfPages],
      T.encodeUtf8 $
        T.concat
          [ "/Kids [",
            T.intercalate "  " $
              L.map (ref . pdfPageObjId) (_pdfPagesKids pdfPages),
            "]"
          ],
      T.encodeUtf8 ">>",
      T.encodeUtf8 "endobj"
    ]

-----------------------------------------------
instance ToJSON PdfExtGState where
  toJSON o =
    object
      [ "objId" .= pdfExtGStateObjId o,
        "name" .= pdfExtGStateName o,
        "fillOpacity" .= pdfExtGStateFillOpacity o
      ]

instance ToByteStringLines PdfExtGState where
  toByteStringLines pdfExtGState =
    [ T.encodeUtf8 $
        T.concat [T.pack $ show $ pdfExtGStateObjId pdfExtGState, " 0 obj"],
      T.encodeUtf8 $
        T.concat
          [ "% ------------------------------------------------------ ExtGState ",
            intToText $ pdfExtGStateObjId pdfExtGState
          ],
      T.encodeUtf8 "<<",
      T.encodeUtf8 "/Type /ExtGState"
    ]
      ++ ( case pdfExtGStateFillOpacity pdfExtGState of
             Just x -> [T.encodeUtf8 $ T.concat ["/ca ", doubleToText x]]
             _ -> []
         )
      ++ [T.encodeUtf8 ">>", T.encodeUtf8 "endobj"]

-----------------------------------------------
instance ToJSON PdfFont where
  toJSON o =
    object
      [ "objId" .= pdfFontObjId o,
        "name" .= pdfFontName o,
        "standardFont" .= pdfFontStandardFont o
      ]

instance ToByteStringLines PdfFont where
  toByteStringLines pdfFont =
    [ T.encodeUtf8 $ T.concat [T.pack $ show $ pdfFontObjId pdfFont, " 0 obj"],
      T.encodeUtf8 $
        T.concat
          [ "% ------------------------------------------------------ Font ",
            intToText $ pdfFontObjId pdfFont
          ],
      T.encodeUtf8 "<<",
      T.encodeUtf8 "/Type /Font",
      T.encodeUtf8 $ T.concat ["/Name /", pdfFontName pdfFont],
      T.encodeUtf8 $ T.concat ["/BaseFont /", pdfStandardFontBaseFont stdFont],
      T.encodeUtf8 $ T.concat ["/Subtype /", pdfStandardFontSubtype stdFont],
      T.encodeUtf8 $ T.concat ["/Encoding /", pdfStandardFontEncoding stdFont],
      T.encodeUtf8 ">>",
      T.encodeUtf8 "endobj"
    ]
    where
      stdFont = pdfFontStandardFont pdfFont

-----------------------------------------------
instance ToJSON PdfResources where
  toJSON o =
    object
      [ "objId" .= pdfResourcesObjId o,
        "fonts" .= pdfResourcesFonts o,
        "extGStates" .= pdfResourcesExtGStates o
      ]

instance ToByteStringLines PdfResources where
  toByteStringLines pdfResources =
    [ T.encodeUtf8 $
        T.concat [T.pack $ show $ pdfResourcesObjId pdfResources, " 0 obj"],
      T.encodeUtf8 $
        T.concat
          [ "% ------------------------------------------------------ Resources ",
            intToText $ pdfResourcesObjId pdfResources
          ],
      T.encodeUtf8 "<<",
      T.encodeUtf8 "/ProcSet [/PDF /Text /ImageB /ImageC /ImageI]"
    ]
      ++ ( case pdfResourcesFonts pdfResources of
             [] -> []
             pdfFonts ->
               [T.encodeUtf8 "/Font <<"]
                 ++ L.map
                   ( \pdfFont ->
                       T.encodeUtf8 $
                         T.concat
                           ["/", pdfFontName pdfFont, " ", ref $ pdfFontObjId pdfFont]
                   )
                   pdfFonts
                 ++ [T.encodeUtf8 ">>"]
         )
      ++ ( case pdfResourcesExtGStates pdfResources of
             [] -> []
             pdfExtGStates ->
               [T.encodeUtf8 "/ExtGState <<"]
                 ++ L.map
                   ( \pdfExtGState ->
                       T.encodeUtf8 $
                         T.concat
                           [ "/",
                             pdfExtGStateName pdfExtGState,
                             " ",
                             ref $ pdfExtGStateObjId pdfExtGState
                           ]
                   )
                   pdfExtGStates
                 ++ [T.encodeUtf8 ">>"]
         )
      ++ [T.encodeUtf8 ">>", T.encodeUtf8 "endobj"]

-----------------------------------------------
instance ToJSON PdfPage where
  toJSON o =
    object
      [ "objId" .= pdfPageObjId o,
        "size" .= pdfPageSize o,
        "margins" .= pdfPageMargins o,
        "layout"
          .= T.pack
            ( case pdfPageLayout o of
                Portrait -> "portrait"
                Landscape -> "landscape"
            ),
        "resources" .= pdfPageResources o,
        "contents" .= pdfPageContents o,
        "currentPos" .= pdfPageCurrentPos o
      ]

instance ToByteStringLines (PdfPage, PdfDocument) where
  toByteStringLines (pdfPage, pdfDoc) =
    [ T.encodeUtf8 $ T.concat [T.pack $ show $ pdfPageObjId pdfPage, " 0 obj"],
      T.encodeUtf8 $
        T.concat
          [ "% ------------------------------------------------------ Page ",
            intToText $ pdfPageObjId pdfPage
          ],
      T.encodeUtf8 "<<",
      T.encodeUtf8 "/Type /Page",
      T.encodeUtf8 $
        T.concat ["/Parent ", ref $ _pdfPagesObjId $ _pdfDocumentPages pdfDoc],
      T.encodeUtf8 $ T.concat ["% ", T.pack $ show $ pdfPageLayout pdfPage],
      T.encodeUtf8 $
        T.concat
          [ "/MediaBox [0 0 ",
            doubleToText $ pdfPageSizeWidth $ pdfPageSize pdfPage,
            " ",
            doubleToText $ pdfPageSizeHeight $ pdfPageSize pdfPage,
            "]"
          ],
      T.encodeUtf8 $
        T.concat
          ["/Resources ", ref $ pdfResourcesObjId $ pdfPageResources pdfPage],
      T.encodeUtf8 $
        T.concat ["/Contents ", ref $ pdfContentsObjId $ pdfPageContents pdfPage],
      T.encodeUtf8 ">>",
      T.encodeUtf8 "endobj"
    ]

-----------------------------------------------
instance ToJSON PdfContents where
  toJSON o =
    object
      ["objId" .= pdfContentsObjId o, "texts" .= pdfContentsStreamContents o]

instance ToByteStringLines (PdfContents, PdfPage, PdfDocument) where
  toByteStringLines (pdfContents, pdfPage, pdfDoc) =
    [ T.encodeUtf8 $
        T.concat [T.pack $ show $ pdfContentsObjId pdfContents, " 0 obj"],
      T.encodeUtf8 $
        T.concat
          [ "% ------------------------------------------------------ Contents ",
            intToText $ pdfContentsObjId pdfContents
          ],
      T.encodeUtf8 "<<",
      T.encodeUtf8 $ T.concat ["/Length ", intToText streamLength],
      T.encodeUtf8 ">>",
      T.encodeUtf8 "stream",
      T.encodeUtf8 $ translateOrigin pageHeight
    ]
      ++ map T.encodeUtf8 streamTextLines
      ++ [T.encodeUtf8 "endstream", T.encodeUtf8 "endobj"]
    where
      PdfPageSize {pdfPageSizeHeight = pageHeight} = pdfPageSize pdfPage
      streamTextLines =
        L.foldl
          ( \acc pdfStreamContent ->
              acc ++ toStreamTextLines pdfStreamContent pdfPage pdfDoc
          )
          []
          (pdfContentsStreamContents pdfContents)
      streamLength = BS.length $ T.encodeUtf8 $ T.unlines streamTextLines

-----------------------------------------------
instance IsStreamContent PdfStreamContent where
  toStreamTextLines pdfText@PdfText {} pdfPage pdfDoc =
    case pdfTextText pdfText of
      Just t ->
        ["q", translateOrigin pageHeight]
          ++ ( case pdfTextColor pdfText of
                 Just (PdfColorRgb r g b) ->
                   [ "/DeviceRGB cs",
                     T.intercalate " " $ L.map doubleToText [r, g, b] ++ ["scn"]
                   ]
                 Just (PdfColorCmyk c m y k) ->
                   [ "/DeviceCMYK cs",
                     T.intercalate " " $ L.map (doubleToText . (/ 100)) [c, m, y, k] ++ ["scn"]
                   ]
                 _ -> []
             )
          ++ ( case pdfTextFillOpacity pdfText of
                 Just fillOpacity ->
                   case findPdfExtGState fillOpacity pdfDoc of
                     Just pdfExtGState ->
                       [T.concat ["/", pdfExtGStateName pdfExtGState, " gs"]]
                     _ -> []
                 _ -> []
             )
          ++ [ "BT",
               translatePos,
               T.concat
                 [ "/",
                   case pdfTextStandardFont pdfText of
                     Just stdFont ->
                       case findPdfFont stdFont pdfDoc of
                         Just pdfFont -> pdfFontName pdfFont
                         _ -> ""
                     _ -> "",
                   " ",
                   case pdfTextFontSize pdfText of
                     Just fontSize -> doubleToText fontSize
                     _ -> "",
                   " Tf"
                 ],
               T.concat
                 ["[<", T.decodeUtf8 $ H.hex $ B8.pack $ T.unpack t, "> 0] TJ"],
               "ET",
               "Q"
             ]
      _ -> []
    where
      PdfPageSize {pdfPageSizeHeight = pageHeight} = pdfPageSize pdfPage
      translatePos =
        T.concat
          [ "1 0 0 1 ",
            doubleToText $ pdfTextX pdfText,
            " ",
            case (pdfTextStandardFont pdfText, pdfTextFontSize pdfText) of
              (Just stdFont, Just fontSize) ->
                doubleToText $ pageHeight - pdfTextY pdfText - fontAscent stdFont fontSize
              _ -> "",
            " Tm"
          ]
  toStreamTextLines pdfPath@PdfPath {} _ _ =
    case pdfPathPoints pdfPath of
      [] -> []
      PdfPos x y : followingPoints ->
        [T.concat [doubleToText x, " ", doubleToText y, " m"]]
          ++ L.map
            ( \(PdfPos x' y') ->
                T.concat [doubleToText x', " ", doubleToText y', " l"]
            )
            followingPoints
          ++ ( case pdfPathWidth pdfPath of
                 Just w -> [T.concat [doubleToText w, " w"]]
                 _ -> []
             )
          ++ case pdfPathDoStroke pdfPath of
            Just True -> ["S"]
            _ -> []

-----------------------------------------------
instance ToJSON PdfXref where
  toJSON o = object ["positions" .= _pdfXrefPositions o]

instance ToByteStringLines (PdfXref, PdfDocument) where
  toByteStringLines (pdfXref, pdfDoc) =
    [ T.encodeUtf8 "xref",
      T.encodeUtf8
        "% ------------------------------------------------------ xref",
      T.encodeUtf8 $
        T.concat
          [ "0 ",
            case _pdfTrailerSize $ _pdfDocumentTrailer pdfDoc of
              Just size -> intToText size
              _ -> ""
          ],
      T.encodeUtf8 "0000000000 65535 f"
    ]
      ++ L.map
        (\pos -> T.encodeUtf8 $ T.concat [formatXrefPos pos, " 00000 n"])
        (_pdfXrefPositions pdfXref)

-- -----------------------------------------------
instance ToJSON PdfTrailer where
  toJSON o = object ["size" .= _pdfTrailerSize o]

instance ToByteStringLines (PdfTrailer, PdfDocument) where
  toByteStringLines (pdfTrailer, pdfDoc) =
    [ T.encodeUtf8 "trailer",
      T.encodeUtf8
        "% ------------------------------------------------------ trailer",
      T.encodeUtf8 "<<",
      T.encodeUtf8 $
        T.concat ["/Size ", maybeIntToText $ _pdfTrailerSize pdfTrailer],
      T.encodeUtf8 $
        T.concat ["/Root ", ref $ pdfRootObjId $ _pdfDocumentRoot pdfDoc],
      T.encodeUtf8 $
        T.concat ["/Info ", ref $ _pdfInfoObjId $ _pdfDocumentInfo pdfDoc],
      T.encodeUtf8 ">>"
    ]

-----------------------------------------------
findPdfFont :: PdfStandardFont -> PdfDocument -> Maybe PdfFont
findPdfFont stdFont pdfDoc =
  L.find
    (\pdfFont -> pdfFontStandardFont pdfFont == stdFont)
    (_pdfDocumentFonts pdfDoc)

findPdfExtGState :: Double -> PdfDocument -> Maybe PdfExtGState
findPdfExtGState fillOpacity pdfDoc =
  L.find
    (\pdfExtGState -> pdfExtGStateFillOpacity pdfExtGState == Just fillOpacity)
    (_pdfDocumentExtGStates pdfDoc)

-----------------------------------------------
pdfPagesTuple :: PdfDocument -> ([PdfPage], PdfPage)
pdfPagesTuple pdfDoc = (initPages, lastPage)
  where
    pdfPages = _pdfDocumentPages pdfDoc
    initPages = L.init $ _pdfPagesKids pdfPages
    lastPage = L.last $ _pdfPagesKids pdfPages

pdfTextsTuple ::
  PdfPage -> ([PdfStreamContent], PdfStreamContent, [PdfStreamContent])
pdfTextsTuple = pdfStreamContentTuple isPdfText

pdfPathsTuple ::
  PdfPage -> ([PdfStreamContent], PdfStreamContent, [PdfStreamContent])
pdfPathsTuple = pdfStreamContentTuple isPdfPath

pdfStreamContentTuple ::
  (PdfStreamContent -> Bool) ->
  PdfPage ->
  ([PdfStreamContent], PdfStreamContent, [PdfStreamContent])
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
    headerLines = _pdfDocumentHeaderLines pdfDoc
    refLineBlocks =
      L.map snd
        $ L.sort
        $ [ ( _pdfInfoObjId $ _pdfDocumentInfo pdfDoc,
              toByteStringLines $ _pdfDocumentInfo pdfDoc
            ),
            ( pdfRootObjId $ _pdfDocumentRoot pdfDoc,
              toByteStringLines (_pdfDocumentRoot pdfDoc, pdfDoc)
            ),
            ( _pdfPagesObjId $ _pdfDocumentPages pdfDoc,
              toByteStringLines $ _pdfDocumentPages pdfDoc
            )
          ]
          ++ L.foldl
            ( \acc pdfPage ->
                [ (pdfPageObjId pdfPage, toByteStringLines (pdfPage, pdfDoc)),
                  ( pdfResourcesObjId $ pdfPageResources pdfPage,
                    toByteStringLines $ pdfPageResources pdfPage
                  ),
                  ( pdfContentsObjId $ pdfPageContents pdfPage,
                    toByteStringLines (pdfPageContents pdfPage, pdfPage, pdfDoc)
                  )
                ]
                  ++ acc
            )
            []
            (_pdfPagesKids $ _pdfDocumentPages pdfDoc)
          ++ L.map
            (\f -> (pdfFontObjId f, toByteStringLines f))
            (_pdfDocumentFonts pdfDoc)
          ++ L.map
            (\gs -> (pdfExtGStateObjId gs, toByteStringLines gs))
            (_pdfDocumentExtGStates pdfDoc)
    footerLines =
      toByteStringLines (_pdfDocumentXref pdfDoc, pdfDoc)
        ++ toByteStringLines (_pdfDocumentTrailer pdfDoc, pdfDoc)
        ++ [ T.encodeUtf8 "startxref",
             T.encodeUtf8 $ maybeIntToText $ _pdfDocumentStartXref pdfDoc,
             T.encodeUtf8 "%%EOF"
           ]

-----------------------------------------------
data Action
  = ActionComposite [Action]
  | ActionInfoSetProducer Text
  | ActionInfoSetCreator Text
  | ActionInfoSetCreationDate
      UTCTime
      TimeZone
  | ActionFinalize
  | ActionPage
  | ActionPageSetSize PdfPageSize
  | ActionPageSetLayout PdfPageLayout
  | ActionPageSetMargin Double
  | ActionPageSetMargins
      Double
      Double
      Double
      Double
  | ActionPageSetSizeCustom
      Double
      Double
  | ActionText
  | ActionTextContent Text
  | ActionTextFont PdfStandardFont
  | ActionTextFontSize Double
  | ActionTextPos
      Double
      Double
  | ActionTextColor PdfColor
  | ActionTextFillOpacity Double
  | ActionMoveDown
  | ActionPath
  | ActionPathPoint
      Double
      Double
  | ActionPathWidth Double
  | ActionPathStroke

-----------------------------------------------
instance IsExecutableAction Action where
  execute (ActionComposite actions) pdfDoc = pdfDoc'
    where
      pdfDoc' = L.foldl (flip execute) pdfDoc actions
  execute (ActionInfoSetProducer text) pdfDoc =
    pdfDoc & (pdfDocumentInfo . pdfInfoProducer) .~ text
  execute (ActionInfoSetCreator text) pdfDoc =
    pdfDoc & (pdfDocumentInfo . pdfInfoCreator) .~ text
  execute (ActionInfoSetCreationDate now timeZone) pdfDoc =
    pdfDoc & ((pdfDocumentInfo . pdfInfoCreationDate) ?~ creationDate)
    where
      creationDate = T.pack $ formatLocalTime timeZone now
  execute ActionFinalize pdfDoc =
    pdfDoc
      & pdfDocumentXref .~ PdfXref {_pdfXrefPositions = L.init positions}
      & pdfDocumentStartXref ?~ L.last positions
      & pdfDocumentTrailer . pdfTrailerSize ?~ _pdfDocumentNextObjId pdfDoc
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
      & pdfDocumentNextObjId .~ nextObjId
      & (pdfDocumentPages . pdfPagesKids)
        <>~ [ PdfPage
                { pdfPageObjId = pageObjId,
                  pdfPageSize = defaultPageSize,
                  pdfPageMargins = defaultPageMargins,
                  pdfPageLayout = Portrait,
                  pdfPageResources =
                    PdfResources
                      { pdfResourcesObjId = resourcesObjId,
                        pdfResourcesFonts = [],
                        pdfResourcesExtGStates = []
                      },
                  pdfPageContents =
                    PdfContents
                      { pdfContentsObjId = contentsObjId,
                        pdfContentsStreamContents = []
                      },
                  pdfPageCurrentPos =
                    PdfPos
                      { pdfPosX = pdfPageMarginLeft defaultPageMargins,
                        pdfPosY = pdfPageMarginTop defaultPageMargins
                      }
                }
            ]
    where
      pageObjId = _pdfDocumentNextObjId pdfDoc
      resourcesObjId = _pdfDocumentNextObjId pdfDoc + 1
      contentsObjId = _pdfDocumentNextObjId pdfDoc + 2
      nextObjId = _pdfDocumentNextObjId pdfDoc + 3
  execute (ActionPageSetSize size) pdfDoc =
    pdfDoc
      & (pdfDocumentPages . pdfPagesKids) .~ initPages ++ [lastPage {pdfPageSize = size}]
    where
      (initPages, lastPage) = pdfPagesTuple pdfDoc
  execute (ActionPageSetSizeCustom width height) pdfDoc =
    pdfDoc
      & (pdfDocumentPages . pdfPagesKids) .~ initPages
        ++ [ lastPage
               { pdfPageSize =
                   PdfPageSize
                     { pdfPageSizeWidth = width,
                       pdfPageSizeHeight = height
                     }
               }
           ]
    where
      (initPages, lastPage) = pdfPagesTuple pdfDoc
  execute (ActionPageSetLayout lay) pdfDoc =
    pdfDoc
      & (pdfDocumentPages . pdfPagesKids) .~ initPages ++ [lastPage {pdfPageLayout = lay, pdfPageSize = pageSize}]
    where
      (initPages, lastPage) = pdfPagesTuple pdfDoc
      pageSize = applyLayout (pdfPageSize lastPage) lay
  execute (ActionPageSetMargin x) pdfDoc =
    pdfDoc
      & (pdfDocumentPages . pdfPagesKids) .~ initPages ++ [lastPage {pdfPageMargins = PdfPageMargins x x x x}]
    where
      (initPages, lastPage) = pdfPagesTuple pdfDoc
  execute (ActionPageSetMargins top left bottom right) pdfDoc =
    pdfDoc
      & (pdfDocumentPages . pdfPagesKids) .~ initPages ++ [lastPage {pdfPageMargins = PdfPageMargins top left bottom right}]
    where
      (initPages, lastPage) = pdfPagesTuple pdfDoc
  execute ActionText pdfDoc =
    pdfDoc
      & (pdfDocumentPages . pdfPagesKids) .~ initPages
      ++ [ lastPage
             { pdfPageContents =
                 (pdfPageContents lastPage)
                   { pdfContentsStreamContents =
                       pdfContentsStreamContents
                         (pdfPageContents lastPage)
                         ++ [ PdfText
                                { pdfTextText = Nothing,
                                  pdfTextX = x,
                                  pdfTextY = y,
                                  pdfTextStandardFont = Nothing,
                                  pdfTextFontSize = Nothing,
                                  pdfTextColor = Nothing,
                                  pdfTextFillOpacity = Nothing
                                }
                            ]
                   },
               pdfPageCurrentPos = PdfPos x y
             }
         ]
    where
      (initPages, lastPage) = pdfPagesTuple pdfDoc
      PdfPos x y = pdfPageCurrentPos lastPage
  execute (ActionTextContent t) pdfDoc =
    pdfDoc
      & (pdfDocumentPages . pdfPagesKids) .~ initPages
        ++ [ lastPage
               { pdfPageContents =
                   (pdfPageContents lastPage)
                     { pdfContentsStreamContents =
                         prefixContents
                           ++ [lastText {pdfTextText = Just t}]
                           ++ suffixContents
                     }
               }
           ]
    where
      (initPages, lastPage) = pdfPagesTuple pdfDoc
      (prefixContents, lastText, suffixContents) = pdfTextsTuple lastPage
  execute (ActionTextFont stdFont) pdfDoc =
    pdfDoc
      & pdfDocumentNextObjId .~ nextObjId
      & pdfDocumentFonts .~ L.nub (_pdfDocumentFonts pdfDoc ++ [pdfFont])
      & (pdfDocumentPages . pdfPagesKids) .~ initPages
        ++ [ lastPage
               { pdfPageResources =
                   (pdfPageResources lastPage)
                     { pdfResourcesFonts =
                         L.nub $
                           (pdfResourcesFonts . pdfPageResources $ lastPage)
                             ++ [pdfFont]
                     },
                 pdfPageContents =
                   (pdfPageContents lastPage)
                     { pdfContentsStreamContents =
                         prefixContents
                           ++ [lastText {pdfTextStandardFont = Just stdFont}]
                           ++ suffixContents
                     }
               }
           ]
    where
      fontObjId = _pdfDocumentNextObjId pdfDoc
      maybePdfFont = findPdfFont stdFont pdfDoc
      fontAlreadyAdded = M.isJust maybePdfFont
      pdfFont =
        case maybePdfFont of
          Just pdfFont' -> pdfFont'
          _ ->
            PdfFont
              { pdfFontObjId = fontObjId,
                pdfFontName = T.concat ["F", intToText fontObjId],
                pdfFontStandardFont = stdFont
              }
      nextObjId =
        if fontAlreadyAdded
          then _pdfDocumentNextObjId pdfDoc
          else _pdfDocumentNextObjId pdfDoc + 1
      (initPages, lastPage) = pdfPagesTuple pdfDoc
      (prefixContents, lastText, suffixContents) = pdfTextsTuple lastPage
  execute (ActionTextFontSize fontSize) pdfDoc =
    pdfDoc
      & (pdfDocumentPages . pdfPagesKids) .~ initPages
        ++ [ lastPage
               { pdfPageContents =
                   (pdfPageContents lastPage)
                     { pdfContentsStreamContents =
                         prefixContents
                           ++ [lastText {pdfTextFontSize = Just fontSize}]
                           ++ suffixContents
                     }
               }
           ]
    where
      (initPages, lastPage) = pdfPagesTuple pdfDoc
      (prefixContents, lastText, suffixContents) = pdfTextsTuple lastPage
  execute (ActionTextPos x y) pdfDoc =
    pdfDoc
      & (pdfDocumentPages . pdfPagesKids) .~ initPages
        ++ [ lastPage
               { pdfPageContents =
                   (pdfPageContents lastPage)
                     { pdfContentsStreamContents =
                         prefixContents
                           ++ [lastText {pdfTextX = x, pdfTextY = y}]
                           ++ suffixContents
                     }
               }
           ]
    where
      (initPages, lastPage) = pdfPagesTuple pdfDoc
      (prefixContents, lastText, suffixContents) = pdfTextsTuple lastPage
  execute (ActionTextColor color) pdfDoc =
    pdfDoc
      & (pdfDocumentPages . pdfPagesKids) .~ initPages
        ++ [ lastPage
               { pdfPageContents =
                   (pdfPageContents lastPage)
                     { pdfContentsStreamContents =
                         prefixContents
                           ++ [lastText {pdfTextColor = Just color}]
                           ++ suffixContents
                     }
               }
           ]
    where
      (initPages, lastPage) = pdfPagesTuple pdfDoc
      (prefixContents, lastText, suffixContents) = pdfTextsTuple lastPage
  execute (ActionTextFillOpacity fillOpacity) pdfDoc =
    pdfDoc
      & pdfDocumentNextObjId .~ nextObjId
      & pdfDocumentExtGStates .~ L.nub (_pdfDocumentExtGStates pdfDoc ++ [pdfExtGState])
      & (pdfDocumentPages . pdfPagesKids) .~ initPages
        ++ [ lastPage
               { pdfPageResources =
                   (pdfPageResources lastPage)
                     { pdfResourcesExtGStates =
                         L.nub $
                           ( pdfResourcesExtGStates . pdfPageResources $
                               lastPage
                           )
                             ++ [pdfExtGState]
                     },
                 pdfPageContents =
                   (pdfPageContents lastPage)
                     { pdfContentsStreamContents =
                         prefixContents
                           ++ [lastText {pdfTextFillOpacity = Just fillOpacity}]
                           ++ suffixContents
                     }
               }
           ]
    where
      extGStateObjId = _pdfDocumentNextObjId pdfDoc
      maybePdfExtGState = findPdfExtGState fillOpacity pdfDoc
      extGStateAlreadyAdded = M.isJust maybePdfExtGState
      pdfExtGState =
        case maybePdfExtGState of
          Just pdfExtGState' -> pdfExtGState'
          _ ->
            PdfExtGState
              { pdfExtGStateObjId = extGStateObjId,
                pdfExtGStateName = T.concat ["GS", intToText extGStateObjId],
                pdfExtGStateFillOpacity = Just fillOpacity
              }
      nextObjId =
        if extGStateAlreadyAdded
          then _pdfDocumentNextObjId pdfDoc
          else _pdfDocumentNextObjId pdfDoc + 1
      (initPages, lastPage) = pdfPagesTuple pdfDoc
      (prefixContents, lastText, suffixContents) = pdfTextsTuple lastPage
  execute ActionMoveDown pdfDoc =
    pdfDoc
      & (pdfDocumentPages . pdfPagesKids) .~ initPages
        ++ [ lastPage
               { pdfPageCurrentPos =
                   PdfPos
                     (pdfTextX lastText)
                     (pdfTextY lastText + currentLineHeight)
               }
           ]
    where
      (initPages, lastPage) = pdfPagesTuple pdfDoc
      (_, lastText, _) = pdfTextsTuple lastPage
      currentLineHeight =
        case (pdfTextStandardFont lastText, pdfTextFontSize lastText) of
          (Just stdFont, Just fontSize) -> fontLineHeight stdFont fontSize
          _ -> 0
  execute ActionPath pdfDoc =
    pdfDoc
      & (pdfDocumentPages . pdfPagesKids) .~ initPages
        ++ [ lastPage
               { pdfPageContents =
                   (pdfPageContents lastPage)
                     { pdfContentsStreamContents =
                         pdfContentsStreamContents
                           (pdfPageContents lastPage)
                           ++ [ PdfPath
                                  { pdfPathPoints = [],
                                    pdfPathWidth = Nothing,
                                    pdfPathDoStroke = Nothing
                                  }
                              ]
                     }
               }
           ]
    where
      (initPages, lastPage) = pdfPagesTuple pdfDoc
  execute (ActionPathPoint x y) pdfDoc =
    pdfDoc
      & (pdfDocumentPages . pdfPagesKids) .~ initPages
        ++ [ lastPage
               { pdfPageContents =
                   (pdfPageContents lastPage)
                     { pdfContentsStreamContents =
                         prefixContents
                           ++ [ lastPath
                                  { pdfPathPoints =
                                      pdfPathPoints lastPath ++ [PdfPos x y]
                                  }
                              ]
                           ++ suffixContents
                     }
               }
           ]
    where
      (initPages, lastPage) = pdfPagesTuple pdfDoc
      (prefixContents, lastPath, suffixContents) = pdfPathsTuple lastPage
  execute (ActionPathWidth w) pdfDoc =
    pdfDoc
      & (pdfDocumentPages . pdfPagesKids) .~ initPages
        ++ [ lastPage
               { pdfPageContents =
                   (pdfPageContents lastPage)
                     { pdfContentsStreamContents =
                         prefixContents
                           ++ [lastPath {pdfPathWidth = Just w}]
                           ++ suffixContents
                     }
               }
           ]
    where
      (initPages, lastPage) = pdfPagesTuple pdfDoc
      (prefixContents, lastPath, suffixContents) = pdfPathsTuple lastPage
  execute ActionPathStroke pdfDoc =
    pdfDoc
      & (pdfDocumentPages . pdfPagesKids) .~ initPages
        ++ [ lastPage
               { pdfPageContents =
                   (pdfPageContents lastPage)
                     { pdfContentsStreamContents =
                         prefixContents
                           ++ [lastPath {pdfPathDoStroke = Just True}]
                           ++ suffixContents
                     }
               }
           ]
    where
      (initPages, lastPage) = pdfPagesTuple pdfDoc
      (prefixContents, lastPath, suffixContents) = pdfPathsTuple lastPage

import qualified Data.List as L

main :: IO ()
main = do
  print $
    pdfTextsTuple $
    PdfPage
      {pdfPageContents = PdfContents {pdfContentsStreamContents = [PdfText 1]}}
  print $
    pdfTextsTuple $
    PdfPage
      { pdfPageContents =
          PdfContents {pdfContentsStreamContents = [PdfText 1, PdfText 2]}
      }
  print $
    pdfTextsTuple $
    PdfPage
      { pdfPageContents =
          PdfContents
            {pdfContentsStreamContents = [PdfText 1, PdfText 2, PdfText 3]}
      }
  print $
    pdfTextsTuple $
    PdfPage
      { pdfPageContents =
          PdfContents
            {pdfContentsStreamContents = [PdfText 1, PdfText 2, PdfText 3]}
      }
  print $
    pdfTextsTuple $
    PdfPage
      { pdfPageContents =
          PdfContents
            {pdfContentsStreamContents = [PdfText 1, PdfText 2, PdfText 3]}
      }
  print $
    pdfTextsTuple $
    PdfPage
      { pdfPageContents =
          PdfContents
            {pdfContentsStreamContents = [PdfPath 1, PdfPath 2, PdfText 3]}
      }
  print $
    pdfTextsTuple $
    PdfPage
      { pdfPageContents =
          PdfContents
            {pdfContentsStreamContents = [PdfText 1, PdfText 2, PdfPath 3]}
      }
  print $
    pdfTextsTuple $
    PdfPage
      { pdfPageContents =
          PdfContents
            {pdfContentsStreamContents = [PdfText 1, PdfPath 2, PdfPath 3]}
      }
  print $
    pdfTextsTuple $
    PdfPage
      { pdfPageContents =
          PdfContents
            {pdfContentsStreamContents = [PdfPath 1, PdfText 2, PdfPath 3]}
      }
  print $
    pdfTextsTuple $
    PdfPage
      { pdfPageContents =
          PdfContents {pdfContentsStreamContents = [PdfPath 1, PdfText 2]}
      }
  print $
    pdfTextsTuple $
    PdfPage
      {pdfPageContents = PdfContents {pdfContentsStreamContents = [PdfPath 1]}}
  return ()

data PdfPage = PdfPage
  { pdfPageContents :: PdfContents
  } deriving (Eq, Show)

data PdfContents = PdfContents
  { pdfContentsStreamContents :: [PdfStreamContent]
  } deriving (Eq, Show)

data PdfStreamContent
  = PdfText Int
  | PdfPath Int
  deriving (Eq, Show)

pdfTextsTuple ::
     PdfPage -> ([PdfStreamContent], PdfStreamContent, [PdfStreamContent])
pdfTextsTuple = pdfStreamContentTuple isPdfText

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

isPdfText :: PdfStreamContent -> Bool
isPdfText pdfStreamContent =
  case pdfStreamContent of
    PdfText {} -> True
    _ -> False

{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}
module Main where

import Control.Applicative
import Snap
import Snap.Snaplet.Heist
import Control.Lens
import Snap.Core
import Snap.Util.FileServe
import Snap.Http.Server
import Text.LaTeX hiding (render)
import Text.LaTeX.Base.Class (fromLaTeX)
import Text.LaTeX.Packages.Inputenc
import Data.Text as DT hiding (map, concat, filter, tail)
import Data.Text.Encoding
import System.Process
import System.Exit 

data Notes = Notes { _heist :: Snaplet (Heist Notes)
                   }
makeLenses ''Notes

instance HasHeist Notes where
  heistLens = subSnaplet heist

main :: IO ()
main = do
  (_, site', _) <- runSnaplet Nothing notesInit
  quickHttpServe site'

-- | Gets called when user fetches index of homepage, 
-- gets the notes and author and creates the PDF document
indexHandler :: Handler Notes Notes ()
indexHandler = do
  notes  <- getParam "note"
  author <- getParam "author"
  case notes of
    Just no -> do
      case author of
        Just au -> makePDF au no
        Nothing -> makePDF "" no
    Nothing -> render "index"
  where
    makePDF au no = do
      liftIO $ renderFile "note.tex" $ createPDF (decodeUtf8 au) 
                                       ((split (=='\n') . decodeUtf8) no)
      (eCode, _, _) <- liftIO $ runPdflatex
      case eCode of
        (ExitFailure x) -> render "indexerr"
        (ExitSuccess)   -> do
          (eCode2, _, _) <- liftIO $ runCopy
          case eCode2 of
            (ExitFailure x) -> render "indexerr"
            (ExitSuccess)   -> render "pdfpage"
    runPdflatex = readProcessWithExitCode "pdflatex" ["note.tex"] ""
    runCopy     = readProcessWithExitCode "cp" ["note.pdf", "static/"] ""

-- | Initializer for notes snaplet
notesInit :: SnapletInit Notes Notes
notesInit = makeSnaplet "notes" "Note maker" Nothing $ do 
  h <- nestSnaplet "heist" heist $ heistInit "templates"
  addRoutes [("static", serveDirectory "static")
            , ("css", serveDirectory "css")
            , ("", indexHandler) ]
  return $ Notes { _heist = h }

-- | Creates the LaTeX PDF
createPDF :: Text -> [Text] -> LaTeX
createPDF a s = thePreamble a <> document (maketitle <> (theBody s))

-- | The preamble for the latex document
thePreamble :: Text -> LaTeX
thePreamble au =
  documentclass [] article <> 
  usepackage [utf8] inputenc <> -- Enable UTF8 encoding for scandinavic chars
  title "Notes" <> author (toLatex au)

-- | Creates a body for the latex document
theBody :: [Text] -> LaTeX
theBody []     = toLatex ""
theBody (n:ns) = (addSyntax . unpack) n <> par <> theBody ns
  where
    addSyntax []              = toLatex ""
    addSyntax q@(x:y:z:z':xs) = case x of
      '\\'      -> rawLatex q
      '}'       -> escapeChar (tail q)
      otherwise -> if (x=='#' && y=='#')
                   then parseSyn [z,z'] xs
                   else fromString [x] <> 
                        addSyntax (tail q)
    addSyntax (x:xs)          = case x of
      '\\'      -> rawLatex $ x:xs
      '}'       -> escapeChar xs 
      otherwise -> fromString (x:xs)
    parseSyn c xs = case c of
      ('c':r)     -> indent <> addSyntax (r++xs)
      ('r':r)     -> rawLatex $ r++xs
      ('b':'r':r) -> bigskip <> addSyntax (r++xs)
      ('n':'p':r) -> newpage <> addSyntax (r++xs)
      otherwise   -> fromString "#" <> addSyntax ("#"++c++xs)
    escapeChar []     = fromString ""
    escapeChar (x:xs) = fromString [x] <> addSyntax xs
    rawLatex = raw . pack

-- | Function to convert from 'Text' to 'LaTeX'
toLatex :: Text -> LaTeX
toLatex = fromString . unpack


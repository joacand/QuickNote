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
      res <- liftIO $ runPdflatex
      res2 <- liftIO $ runCopy
      render "pdfpage"
    runPdflatex = readProcess "pdflatex" ["note.tex"] ""
    runCopy     = readProcess "cp" ["note.pdf", "static/"] ""

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
    addSyntax q@(x:y:z:z':xs) = if (x=='\\') 
                                then escapeChar (tail q) 
                                else if (x=='#' && y=='#')
                                     then parseSyn [z,z'] xs
                                     else fromString [x] <> 
                                          addSyntax (tail q)
    addSyntax (x:xs)          = if (x=='\\') then escapeChar xs 
                                             else fromString (x:xs)
    parseSyn c xs = case c of
      ('c':r)     -> indent <> addSyntax (r++xs)
      ('r':r)     -> (raw . pack) (r++xs)
      ('b':'r':r) -> bigskip <> addSyntax (r++xs)
      ('n':'p':r) -> newpage <> addSyntax (r++xs)
      otherwise   -> fromString "#" <> addSyntax ("#"++c++xs)
    escapeChar []     = fromString ""
    escapeChar (x:xs) = fromString [x] <> addSyntax xs

-- | Function to convert from 'Text' to 'LaTeX'
toLatex :: Text -> LaTeX
toLatex = fromString . unpack


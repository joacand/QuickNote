{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}
module Main where

import Control.Applicative
import Snap
import Snap.Snaplet.Heist
import Heist
import Heist.Interpreted
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
import Data.IORef
import Data.Maybe

data Notes = Notes { _heist :: Snaplet (Heist Notes)
                   , _notenr :: IORef Integer
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
indexHandler = ifTop ( indexHandler' )
               <|> error404

-- | Helper function to 'indexHandler'
indexHandler' :: Handler Notes Notes ()
indexHandler' = do
  title  <- getParam "title"
  author <- getParam "author"
  notes  <- getParam "note"
  case notes of
    Just no -> do
      noteRef <- use notenr
      noteNumber <- liftIO $ readIORef noteRef
      makePDF (fromMaybe "" title) (fromMaybe "" author) no 
              ("note" ++ show noteNumber) noteRef
    Nothing -> renderWithSplices "index" (errorSplice "" "")
  where
    makePDF ti au no note noteRef = do
      liftIO $ renderFile ("tmp/"++note++".tex") $ createPDF (decodeUtf8 ti) 
                          (decodeUtf8 au) ((split (=='\n') . decodeUtf8) no)
      (eCode, _, _) <- liftIO $ runPdflatex ("tmp/"++note ++ ".tex")
      case eCode of
        (ExitFailure x) -> renderError no
        (ExitSuccess)   -> do
          (eCode2, _, _) <- liftIO $ runCopy ("tmp/"++note ++ ".pdf")
          case eCode2 of
            (ExitFailure x) -> renderError no
            (ExitSuccess)   -> do
              liftIO $ modifyIORef' noteRef (+1)
              notenr .= noteRef
              renderWithSplices "pdfpage" (noteSplice (note++".pdf"))
    renderError no = renderWithSplices "index" (errorSplice 
                     ("Error when compiling to LaTeX. Are you sure you typed \
                      \your LaTeX code correctly?") (decodeUtf8 no))
    runPdflatex note = readProcessWithExitCode "pdflatex" 
                       ["-output-directory=tmp", note] ""
    runCopy     note = readProcessWithExitCode "cp" [note, "static/"] ""

-- | Render 404 page
error404 :: Handler Notes Notes ()
error404 = render "404"

-- | Initializer for notes snaplet
notesInit :: SnapletInit Notes Notes
notesInit = makeSnaplet "notes" "Note maker" Nothing $ do 
  h <- nestSnaplet "heist" heist $ heistInit "templates"
  addRoutes [ ("static", serveDirectory "static")
            , ("css", serveDirectory "css")
            , ("", indexHandler) ]
  newRef <- liftIO $ newIORef 0
  return $ Notes { _heist = h, _notenr = newRef }

-- | Splice used to pass an error message and notes to the index template
errorSplice :: (Monad m) => String -> Text -> Splices (HeistT n m Template)
errorSplice errMsg notes = do
  "error" ## textSplice (DT.pack errMsg)
  "oldnotes" ## textSplice notes

-- | Splice used to pass the correct filename to the PDF template
noteSplice :: (Monad m) => String -> Splices (HeistT n m Template)
noteSplice n = do
  "note" ## textSplice (DT.pack n)

-- | Creates the LaTeX PDF
createPDF :: Text -> Text -> [Text] -> LaTeX
createPDF t a s = thePreamble t a <> document (maketitle <> (theBody s))

-- | The preamble for the latex document
thePreamble :: Text -> Text -> LaTeX
thePreamble ti au =
  documentclass [] article <> 
  usepackage [utf8] inputenc <> -- Enable UTF8 encoding for scandinavic chars
  title (toLatex ti) <> author (toLatex au)

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


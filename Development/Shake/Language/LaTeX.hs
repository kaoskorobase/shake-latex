-- | This module provides utilities for building LaTeX source files with the Shake build system.
module Development.Shake.Language.LaTeX (
    pdflatex
  , mkAcronyms
  , mkHyphenations
  , svgToPdf
  , grep
) where

import Control.Arrow (second)
import Control.Monad (when)
import Control.Monad.IO.Class (MonadIO(..))
import Data.Char (isSpace)
import Data.List (isPrefixOf, stripPrefix)
import Data.Maybe (isJust)
import Development.Shake
import Development.Shake.FilePath
import System.Directory (getHomeDirectory)
import System.Exit (ExitCode(ExitSuccess))
import Text.Regex

matches :: Regex -> String -> Bool
matches regex = isJust . matchRegex regex

grep :: MonadIO m => Regex -> FilePath -> m [String]
grep regex file = liftIO $
  readFile file >>= return . filter (matches regex) . lines

pdflatex :: FilePath -> FilePath -> Action ()
pdflatex tex out = do
  let buildDir = takeDirectory out
      log = out -<.> "log"
      pdflatex options file =
        command_ [] "pdflatex" $ [ "-jobname=" ++ dropExtension out
                                 -- , "-output-directory=" ++ buildDir
                                 , "-interaction=nonstopmode"
                                 , "-file-line-error" ]
                                 ++ options
                                 ++ [file]

  -- Run pdflatex and record dependencies
  pdflatex ["-recorder"] tex
  -- Avoid a dependency on the fls file because it changes in every run
  fls <- liftIO $ readFile $ out -<.> "fls"
  need $ -- Don't depend on files that change in every run
         filter (not . flip elem [".aux", ".bbl", ".out"] . takeExtension)
       $ [ x | Just x <- map (stripPrefix "INPUT ") (lines fls) ]

  let whenFileContains regex file action = do
        b <- grep (mkRegexWithOpts regex True False) file >>= return . not . null
        when b action

  -- Run makeindex if necessary
  whenFileContains "^[^%]*\\makeindex" tex $
    command_ [] "makeindex" [
        "-o", out -<.> "ind"
      , "-t", out -<.> "ilg"
      , out -<.> "idx" ]

  -- Run bibtex if necessary
  whenFileContains "No file.*\\.bbl|Citation.*undefined" log $
    command_ [Cwd buildDir] "bibtex" [takeFileName out -<.> "aux"]

  let rerun = whenFileContains
              -- From http://www.cs.berkeley.edu/~jaein/notes/Makefile.pdflatex
              "(There were undefined references|Rerun to get (cross-references|the bars) right|Citation.*undefined)"
              log
              (pdflatex [] tex)

  rerun
  rerun

mapFileLines :: (String -> String) -> FilePath -> FilePath -> Action ()
mapFileLines f inFile outFile =
  readFileLines inFile
    >>= return . map f
    >>= writeFileLines outFile

mkAcronyms :: FilePath -> FilePath -> Action ()
mkAcronyms =
  mapFileLines
    (\x -> let (acr, def) = second (dropWhile isSpace . tail)
                          . break (==':') $ x
           in concat ["\\newacronym{", acr, "}{", acr, "}{", def, "}"])

mkHyphenations :: FilePath -> FilePath -> Action ()
mkHyphenations =
  mapFileLines
    (\x -> concat ["\\hyphenation{", x, "}"])

svgToPdf :: FilePath -> FilePath -> Action ()
svgToPdf svg pdf = do
  command_ [] "/Applications/Inkscape.app/Contents/Resources/bin/inkscape"
              [ "--export-pdf=" ++ pdf
              , svg ]

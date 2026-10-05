module Main where

import System.Environment (getArgs)
import System.Exit (exitFailure)
import System.IO (hPutStrLn, stderr)
import Parser (parseSongSheet)
import HtmlRenderer (generateHtml)

main :: IO ()
main = do
  args <- getArgs
  case args of
    ["-h"] -> printHelp
    ["--help"] -> printHelp
    [inFile] -> compileFile inFile Nothing
    [inFile, "-o", outFile] -> compileFile inFile (Just outFile)
    [inFile, outFile] -> compileFile inFile (Just outFile)
    [] -> do
      input <- getContents
      let song = parseSongSheet input
      putStr (generateHtml song)
    _ -> do
      hPutStrLn stderr "Error: Invalid arguments."
      printHelp
      exitFailure

printHelp :: IO ()
printHelp = putStrLn $ unlines
  [ "ChordBook CLI Compiler - Compile song sheets to beautiful HTML"
  , ""
  , "Usage:"
  , "  chordbook <input.chord> [-o <output.html>]"
  , "  chordbook < <input.chord> > <output.html>"
  , ""
  , "Options:"
  , "  -h, --help      Show this help message"
  , "  -o FILE         Specify output HTML file path"
  ]

compileFile :: FilePath -> Maybe FilePath -> IO ()
compileFile inFile mOutFile = do
  content <- readFile inFile
  let song = parseSongSheet content
      html = generateHtml song
  case mOutFile of
    Just outPath -> do
      writeFile outPath html
      putStrLn $ "Successfully compiled " ++ inFile ++ " -> " ++ outPath
    Nothing      -> putStr html

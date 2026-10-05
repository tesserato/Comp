module Main where

import System.Environment (getArgs)
import System.Exit (exitFailure)
import System.IO
  ( hPutStrLn
  , hSetEncoding
  , stderr
  , stdin
  , stdout
  , utf8
  , withFile
  , IOMode(ReadMode, WriteMode)
  , hGetContents
  , hPutStr
  )
import Parser (parseSongSheet)
import HtmlRenderer (generateHtml)

main :: IO ()
main = do
  hSetEncoding stdin utf8
  hSetEncoding stdout utf8
  hSetEncoding stderr utf8
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

readUtf8File :: FilePath -> IO String
readUtf8File path =
  withFile path ReadMode $ \h -> do
    hSetEncoding h utf8
    hGetContents h >>= \s -> length s `seq` return s

writeUtf8File :: FilePath -> String -> IO ()
writeUtf8File path content =
  withFile path WriteMode $ \h -> do
    hSetEncoding h utf8
    hPutStr h content

compileFile :: FilePath -> Maybe FilePath -> IO ()
compileFile inFile mOutFile = do
  content <- readUtf8File inFile
  let song = parseSongSheet content
      html = generateHtml song
  case mOutFile of
    Just outPath -> do
      writeUtf8File outPath html
      putStrLn $ "Successfully compiled " ++ inFile ++ " -> " ++ outPath
    Nothing      -> putStr html

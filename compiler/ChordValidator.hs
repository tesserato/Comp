module ChordValidator
  ( validateChord
  , isChordLine
  , locateChords
  , Chord(..)
  , PlacedChord(..)
  , RootNote(..)
  , Accidental(..)
  , ChordQuality(..)
  ) where

import AST
import Data.Char (isSpace)
import qualified Text.Parsec as P
import Text.Parsec ((<|>))

type Parser = P.Parsec String ()

parseRootNote :: Parser (RootNote, Accidental)
parseRootNote = do
  r <- P.oneOf "ABCDEFG"
  let root = case r of
        'A' -> A; 'B' -> B; 'C' -> C; 'D' -> D
        'E' -> E; 'F' -> F; 'G' -> G
        _   -> error "unreachable"
  acc <- (P.char '#' >> return Sharp)
     <|> (P.char '♯' >> return Sharp)
     <|> (P.char 'b' >> return Flat)
     <|> (P.char '♭' >> return Flat)
     <|> return Natural
  return (root, acc)

parseChordQuality :: Parser ChordQuality
parseChordQuality =
      P.try (P.string "maj9"  >> return Major9)
  <|> P.try (P.string "maj7"  >> return Major7)
  <|> P.try (P.string "maj"   >> return Major)
  <|> P.try (P.string "min9"  >> return Minor9)
  <|> P.try (P.string "min7"  >> return Minor7)
  <|> P.try (P.string "min"   >> return Minor)
  <|> P.try (P.string "m9"    >> return Minor9)
  <|> P.try (P.string "m7b5"  >> return HalfDiminished)
  <|> P.try (P.string "m7"    >> return Minor7)
  <|> P.try (P.string "m6"    >> return Minor6)
  <|> P.try (P.string "m"     >> return Minor)
  <|> P.try (P.string "7sus4" >> return SevenSus4)
  <|> P.try (P.string "sus4"  >> return Sus4)
  <|> P.try (P.string "sus2"  >> return Sus2)
  <|> P.try (P.string "sus"   >> return Sus4)
  <|> P.try (P.string "dim7"  >> return Diminished7)
  <|> P.try (P.string "dim"   >> return Diminished)
  <|> P.try (P.string "aug7"  >> return Augmented7)
  <|> P.try (P.string "aug"   >> return Augmented)
  <|> P.try (P.string "add9"  >> return Add9)
  <|> P.try (P.string "add2"  >> return Add2)
  <|> P.try (P.string "add11" >> return Add11)
  <|> P.try (P.string "13"    >> return Thirteenth)
  <|> P.try (P.string "11"    >> return Eleventh)
  <|> P.try (P.string "9"     >> return Ninth)
  <|> P.try (P.string "7"     >> return Dominant7)
  <|> P.try (P.string "6"     >> return Sixth)
  <|> P.try (P.string "5"     >> return PowerChord)
  <|> P.try (P.char '+'       >> return Augmented)
  <|> P.try (P.char 'o'       >> return Diminished)
  <|> P.try (P.char 'ø'       >> return HalfDiminished)
  <|> P.try (P.char 'Δ'       >> return Major7)
  <|> return Major

parseBassNote :: Parser (RootNote, Accidental)
parseBassNote = P.char '/' >> parseRootNote

parseChordGrammar :: Parser Chord
parseChordGrammar = do
  root <- parseRootNote
  qual <- parseChordQuality
  bass <- P.optionMaybe parseBassNote
  P.eof
  return $ Chord root qual bass

validateChord :: String -> Maybe Chord
validateChord str =
  case P.parse parseChordGrammar "" str of
    Right chord -> Just chord
    Left _      -> Nothing

locateChords :: String -> [PlacedChord]
locateChords line = go 0 line
  where
    go _ [] = []
    go col (c:cs)
      | isSpace c = go (col + 1) cs
      | otherwise =
          let chordWord = c : takeWhile (not . isSpace) cs
              nextCs    = dropWhile (not . isSpace) cs
              parsed    = validateChord chordWord
          in PlacedChord col chordWord parsed : go (col + length chordWord) nextCs

isChordLine :: String -> Bool
isChordLine line
  | all isSpace line = False
  | otherwise =
      let tokens = words line
      in not (null tokens) && all isChordToken tokens
  where
    isChordToken tok =
      tok `elem` ["|", "||", "/", "//", "%", "N.C.", "-", "(", ")"] ||
      validateChord tok /= Nothing

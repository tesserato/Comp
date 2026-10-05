module Parser (parseSongSheet) where

import AST
import ChordValidator (isChordLine, locateChords)
import Data.Char (isSpace)
import Data.List (dropWhileEnd, isPrefixOf)
import Data.Maybe (mapMaybe)

parseSongSheet :: String -> Song
parseSongSheet input =
  let allLines = lines input
      (metaLines, bodyLines) = spanMetadata allLines
      metadata = mapMaybe parseMetaLine metaLines
      sections = parseSections bodyLines
  in Song metadata sections

spanMetadata :: [String] -> ([String], [String])
spanMetadata = go []
  where
    go acc [] = (reverse acc, [])
    go acc (l:ls)
      | isMetaLine l = go (l:acc) ls
      | all isSpace l && null acc = go acc ls
      | all isSpace l = (reverse acc, ls)
      | otherwise = (reverse acc, l:ls)

isMetaLine :: String -> Bool
isMetaLine line =
  case break (== ':') line of
    (k, v) | not (null k) && not (null v) && not (isSpace (head k)) ->
      all (\c -> not (isSpace c) || c == ' ') k && not (isPrefixOf "[" (trim k))
    _ -> False

parseMetaLine :: String -> Maybe (String, String)
parseMetaLine line =
  case break (== ':') line of
    (k, ':':v) -> Just (trim k, trim v)
    _          -> Nothing

trim :: String -> String
trim = dropWhile isSpace . dropWhileEnd isSpace

parseSections :: [String] -> [Section]
parseSections rawLines =
  let cleanLines = dropWhile (all isSpace) rawLines
  in groupIntoSections cleanLines

isSectionHeader :: String -> Bool
isSectionHeader l =
  let s = trim l
  in (isPrefixOf "[" s && isPrefixOf "]" (reverse s)) ||
     (isPrefixOf "==" s && isPrefixOf "==" (reverse s))

extractSectionHeader :: String -> String
extractSectionHeader l =
  let s = trim l
  in if isPrefixOf "[" s && isPrefixOf "]" (reverse s)
     then init (tail s)
     else trim (filter (/= '=') s)

groupIntoSections :: [String] -> [Section]
groupIntoSections [] = []
groupIntoSections (l:ls)
  | all isSpace l = groupIntoSections ls
  | isSectionHeader l =
      let name = extractSectionHeader l
          (body, rest) = span (not . isSectionHeader) ls
          items = parseSectionBody body
      in Section (Just name) items : groupIntoSections rest
  | otherwise =
      let (body, rest) = span (not . isSectionHeader) (l:ls)
          items = parseSectionBody body
      in Section Nothing items : groupIntoSections rest

parseSectionBody :: [String] -> [SectionItem]
parseSectionBody [] = []
parseSectionBody (l1:l2:rest)
  | all isSpace l1 = parseSectionBody (l2:rest)
  | isComment l1   = CommentLine (trim (dropCommentMarker l1)) : parseSectionBody (l2:rest)
  | isChordLine l1 && not (isChordLine l2) && not (all isSpace l2) && not (isComment l2) =
      PairedLine (alignChordsAndLyrics l1 l2) : parseSectionBody rest
  | isChordLine l1 =
      ChordOnlyLine (locateChords l1) : parseSectionBody (l2:rest)
  | otherwise =
      LyricOnlyLine l1 : parseSectionBody (l2:rest)
parseSectionBody [l]
  | all isSpace l = []
  | isComment l   = [CommentLine (trim (dropCommentMarker l))]
  | isChordLine l = [ChordOnlyLine (locateChords l)]
  | otherwise     = [LyricOnlyLine l]

isComment :: String -> Bool
isComment l =
  let s = trim l
  in isPrefixOf "#" s || isPrefixOf "//" s

dropCommentMarker :: String -> String
dropCommentMarker l =
  let s = trim l
  in if isPrefixOf "//" s then drop 2 s
     else if isPrefixOf "#" s then drop 1 s
     else s

alignChordsAndLyrics :: String -> String -> [ChordSyllable]
alignChordsAndLyrics chordLine lyricLine =
  let chords = locateChords chordLine
  in sliceByChords 0 chords lyricLine

sliceByChords :: Int -> [PlacedChord] -> String -> [ChordSyllable]
sliceByChords currPos [] lyrics =
  if currPos < length lyrics
  then [ChordSyllable Nothing (drop currPos lyrics)]
  else []
sliceByChords currPos (pc:pcs) lyrics
  | chordCol pc > currPos =
      let preLyric = take (chordCol pc - currPos) (drop currPos lyrics)
          currPos' = chordCol pc
      in (if null preLyric then [] else [ChordSyllable Nothing preLyric]) ++
         sliceByChords currPos' (pc:pcs) lyrics
  | otherwise =
      let nextPos = case pcs of
            []     -> max (chordCol pc + length (chordName pc)) (length lyrics)
            (p2:_) -> chordCol p2
          chordPart = Just (chordName pc, chordParsed pc)
          lyricSlice = take (nextPos - chordCol pc) (drop (chordCol pc) lyrics)
      in ChordSyllable chordPart lyricSlice : sliceByChords nextPos pcs lyrics

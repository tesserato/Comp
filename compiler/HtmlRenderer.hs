module HtmlRenderer (generateHtml) where

import AST
import CSS (cssStyles, themeScript)
import Fingering (solveChordDiagram, deriveChordDiagram)
import SvgRenderer (renderChordSvg)
import Data.Char (isSpace, toLower)
import Data.List (nub)
import Data.Maybe (mapMaybe)

type ChordMap = [(String, ChordDiagram)]

buildChordMap :: Song -> ChordMap
buildChordMap song =
  let usedChords = nub (collectSongPlacedChords song)
      customs = songCustomChords song
      resolveChord (name, parsed) =
        case lookup name customs of
          Just (FretDef frets) ->
            Just (name, solveChordDiagram frets)
          Nothing ->
            case parsed of
              Just chord -> case deriveChordDiagram chord of
                Just diagram -> Just (name, diagram)
                Nothing      -> Nothing
              Nothing    -> Nothing
  in mapMaybe resolveChord usedChords

collectSongPlacedChords :: Song -> [(String, Maybe Chord)]
collectSongPlacedChords song =
  concatMap collectSectionPlacedChords (songSections song)
  where
    collectSectionPlacedChords sec = concatMap collectItemPlacedChords (sectionItems sec)
    collectItemPlacedChords (PairedLine syllables) =
      [ (name, parsed) | ChordSyllable (Just (name, parsed)) _ <- syllables ]
    collectItemPlacedChords (ChordOnlyLine chords) =
      [ (chordName pc, chordParsed pc) | pc <- chords ]
    collectItemPlacedChords _ = []

escapeHtml :: String -> String
escapeHtml [] = []
escapeHtml ('&':cs) = "&amp;" ++ escapeHtml cs
escapeHtml ('<':cs) = "&lt;" ++ escapeHtml cs
escapeHtml ('>':cs) = "&gt;" ++ escapeHtml cs
escapeHtml ('"':cs) = "&quot;" ++ escapeHtml cs
escapeHtml ('\'':cs) = "&#39;" ++ escapeHtml cs
escapeHtml (c:cs) = c : escapeHtml cs

generateHtml :: Song -> String
generateHtml song =
  let chordMap = buildChordMap song
  in unlines
    [ "<!DOCTYPE html>"
    , "<html lang=\"en\">"
    , "<head>"
    , "  <meta charset=\"UTF-8\">"
    , "  <meta name=\"viewport\" content=\"width=device-width, initial-scale=1.0\">"
    , "  <title>" ++ escapeHtml (getTitle song) ++ "</title>"
    , "  <script>"
    , themeScript
    , "  </script>"
    , "  <style>"
    , cssStyles
    , "  </style>"
    , "</head>"
    , "<body>"
    , "  <main class=\"sheet\">"
    , "    <button class=\"theme-toggle\" onclick=\"toggleTheme()\" aria-label=\"Toggle theme\" title=\"Toggle light / dark\">"
    , "      <span class=\"icon-moon\">&#9790;</span><span class=\"icon-sun\">&#9788;</span>"
    , "    </button>"
    , renderHeader song
    , renderBody song chordMap
    , "  </main>"
    , "</body>"
    , "</html>"
    ]

getTitle :: Song -> String
getTitle song =
  case lookup "Title" (songMetadata song) of
    Just t  -> t
    Nothing -> case songMetadata song of
                 ((_, v):_) -> v
                 []         -> "Untitled Song"


renderHeader :: Song -> String
renderHeader song =
  let meta = songMetadata song
      title = maybe "Untitled Song" id (lookup "Title" meta)
      artist = lookup "Artist" meta
      key = lookup "Key" meta
      capo = lookup "Capo" meta
      tempo = lookup "Tempo" meta
      otherMeta = filter (\(k, _) -> map toLower k `notElem` ["title", "artist", "key", "capo", "tempo"]) meta
  in unlines $
    [ "    <header class=\"song-header\">"
    , "      <h1 class=\"song-title\">" ++ escapeHtml title ++ "</h1>"
    ] ++
    (case artist of
       Just a  -> [ "      <div class=\"song-artist\">" ++ escapeHtml a ++ "</div>" ]
       Nothing -> []) ++
    [ "      <div class=\"song-badges\">" ] ++
    (case key of
       Just k  -> [ "        <span class=\"badge badge-key\"><span class=\"badge-label\">KEY</span> " ++ escapeHtml k ++ "</span>" ]
       Nothing -> []) ++
    (case capo of
       Just c  -> [ "        <span class=\"badge badge-capo\"><span class=\"badge-label\">CAPO</span> " ++ escapeHtml c ++ "</span>" ]
       Nothing -> []) ++
    (case tempo of
       Just t  -> [ "        <span class=\"badge badge-tempo\"><span class=\"badge-label\">TEMPO</span> " ++ escapeHtml t ++ "</span>" ]
       Nothing -> []) ++
    map (\(k, v) -> "        <span class=\"badge badge-custom\"><span class=\"badge-label\">" ++ escapeHtml k ++ "</span> " ++ escapeHtml v ++ "</span>") otherMeta ++
    [ "      </div>"
    , "    </header>"
    ]

renderBody :: Song -> ChordMap -> String
renderBody song chordMap = unlines $
  [ "    <section class=\"song-content\">" ] ++
  map (renderSection chordMap) (songSections song) ++
  [ "    </section>" ]

renderSection :: ChordMap -> Section -> String
renderSection chordMap (Section Nothing [ChordPaletteItem]) =
  renderChordPalette chordMap
renderSection chordMap (Section mName items) =
  let renderedItems = filter (not . all isSpace) (map (renderItem chordMap) items)
  in if null renderedItems
     then ""
     else unlines $
       [ "      <div class=\"song-section\">" ] ++
       (case mName of
          Just name -> [ "        <h2 class=\"section-title\">" ++ escapeHtml name ++ "</h2>" ]
          Nothing   -> []) ++
       [ "        <div class=\"section-lines\">" ] ++
       renderedItems ++
       [ "        </div>"
       , "      </div>"
       ]

renderItem :: ChordMap -> SectionItem -> String
renderItem chordMap (PairedLine syllables) =
  "          <div class=\"chord-lyric-row\">" ++
  concatMap (renderSyllable chordMap) syllables ++
  "</div>"
renderItem chordMap (ChordOnlyLine chords) =
  "          <div class=\"chord-only-row\">" ++
  concatMap (renderPlacedChord chordMap) chords ++
  "</div>"
renderItem _ (LyricOnlyLine lyric) =
  "          <div class=\"lyric-only-row\">" ++ escapeHtml lyric ++ "</div>"
renderItem _ (CommentLine _) = ""
renderItem chordMap ChordPaletteItem =
  renderChordPalette chordMap

renderSyllable :: ChordMap -> ChordSyllable -> String
renderSyllable chordMap (ChordSyllable mChord lyric) =
  let chordHtml = case mChord of
        Just (name, parsed) ->
          renderChordWithHover chordMap name (parsed /= Nothing)
        Nothing -> "<span class=\"chord-spacer\">&nbsp;</span>"
      lyricText = if null lyric then "&nbsp;" else escapeHtml lyric
  in "<span class=\"chord-lyric-pair\">" ++ chordHtml ++ "<span class=\"lyric\">" ++ lyricText ++ "</span></span>"

renderPlacedChord :: ChordMap -> PlacedChord -> String
renderPlacedChord chordMap (PlacedChord _ name parsed) =
  "<span class=\"chord-tab-item\">" ++ renderChordWithHover chordMap name (parsed /= Nothing) ++ "</span>"

renderChordWithHover :: ChordMap -> String -> Bool -> String
renderChordWithHover chordMap name isValid =
  let valClass = if isValid then "chord-valid" else "chord-custom"
  in case lookup name chordMap of
       Just diag ->
          "<span class=\"chord-with-hover\"><span class=\"chord " ++ valClass ++ "\">" ++ escapeHtml name ++ "</span><div class=\"chord-popover\">" ++ renderChordSvg name diag ++ "</div></span>"
       Nothing ->
         if isValid
         then "<span class=\"chord-with-hover\"><span class=\"chord " ++ valClass ++ "\">" ++ escapeHtml name ++ "</span><div class=\"chord-popover chord-popover-text\"><strong>" ++ escapeHtml name ++ "</strong><span>Recognized chord. Add a custom voicing to show a diagram.</span></div></span>"
         else "<span class=\"chord " ++ valClass ++ "\">" ++ escapeHtml name ++ "</span>"

renderChordPalette :: ChordMap -> String
renderChordPalette [] = ""
renderChordPalette chordMap = unlines $
  [ "      <div class=\"song-chords-palette\">"
  , "        <div class=\"palette-title\">Chords in this song</div>"
  , "        <div class=\"palette-grid\">"
  ] ++
  [ "          <div class=\"palette-item\">" ++ renderChordSvg name diag ++ "</div>"
  | (name, diag) <- chordMap
  ] ++
  [ "        </div>"
  , "      </div>"
  ]


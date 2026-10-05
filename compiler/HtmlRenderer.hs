module HtmlRenderer (generateHtml) where

import AST
import CSS (cssStyles)
import Data.Char (toLower)

escapeHtml :: String -> String
escapeHtml [] = []
escapeHtml ('&':cs) = "&amp;" ++ escapeHtml cs
escapeHtml ('<':cs) = "&lt;" ++ escapeHtml cs
escapeHtml ('>':cs) = "&gt;" ++ escapeHtml cs
escapeHtml ('"':cs) = "&quot;" ++ escapeHtml cs
escapeHtml ('\'':cs) = "&#39;" ++ escapeHtml cs
escapeHtml (c:cs) = c : escapeHtml cs

generateHtml :: Song -> String
generateHtml song = unlines
  [ "<!DOCTYPE html>"
  , "<html lang=\"en\">"
  , "<head>"
  , "  <meta charset=\"UTF-8\">"
  , "  <meta name=\"viewport\" content=\"width=device-width, initial-scale=1.0\">"
  , "  <title>" ++ escapeHtml (getTitle song) ++ "</title>"
  , "  <style>"
  , cssStyles
  , "  </style>"
  , "</head>"
  , "<body>"
  , "  <main class=\"chordbook-container\">"
  , renderHeader song
  , renderBody song
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

renderBody :: Song -> String
renderBody song = unlines $
  [ "    <section class=\"song-content\">" ] ++
  map renderSection (songSections song) ++
  [ "    </section>" ]

renderSection :: Section -> String
renderSection (Section mName items) = unlines $
  [ "      <div class=\"song-section\">" ] ++
  (case mName of
     Just name -> [ "        <h2 class=\"section-title\">" ++ escapeHtml name ++ "</h2>" ]
     Nothing   -> []) ++
  [ "        <div class=\"section-lines\">" ] ++
  map renderItem items ++
  [ "        </div>"
  , "      </div>"
  ]

renderItem :: SectionItem -> String
renderItem (PairedLine syllables) =
  "          <div class=\"chord-lyric-row\">" ++
  concatMap renderSyllable syllables ++
  "</div>"
renderItem (ChordOnlyLine chords) =
  "          <div class=\"chord-only-row\">" ++
  concatMap renderPlacedChord chords ++
  "</div>"
renderItem (LyricOnlyLine lyric) =
  "          <div class=\"lyric-only-row\">" ++ escapeHtml lyric ++ "</div>"
renderItem (CommentLine comment) =
  "          <div class=\"comment-row\">" ++ escapeHtml comment ++ "</div>"

renderSyllable :: ChordSyllable -> String
renderSyllable (ChordSyllable mChord lyric) =
  let chordHtml = case mChord of
        Just (name, parsed) ->
          let valClass = if parsed /= Nothing then "chord-valid" else "chord-custom"
          in "<span class=\"chord " ++ valClass ++ "\">" ++ escapeHtml name ++ "</span>"
        Nothing -> "<span class=\"chord-spacer\">&nbsp;</span>"
      lyricText = if null lyric then "&nbsp;" else escapeHtml lyric
  in "<span class=\"chord-lyric-pair\">" ++ chordHtml ++ "<span class=\"lyric\">" ++ lyricText ++ "</span></span>"

renderPlacedChord :: PlacedChord -> String
renderPlacedChord (PlacedChord _ name parsed) =
  let valClass = if parsed /= Nothing then "chord-valid" else "chord-custom"
  in "<span class=\"chord-tab-item\"><span class=\"chord " ++ valClass ++ "\">" ++ escapeHtml name ++ "</span></span>"

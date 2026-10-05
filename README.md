# ChordBook Markup Language & Compiler System

ChordBook is an elegant plain-text markup language designed to compile song sheets into publication-grade HTML chordbooks, accompanied by a VS Code syntax highlighting extension.

---

## 1. Syntax & Grammar Rules

A ChordBook sheet (`.chord`, `.chords`, `.crd`) consists of:

### 1. Metadata Block (Top of File)
Sequential lines at the beginning of the file matching `Key: Value`:
```chord
Title: Hotel California
Artist: Eagles
Key: Bm
Capo: 7th fret
Tempo: 75 bpm
```

### 2. Section Headers
Optional section headings in bracketed or markdown style:
```chord
[Verse 1]
== Chorus ==
```

### 3. Song Blocks (Alternating Chord Lines & Lyric Lines)
Song blocks consist of alternating rows of chord lines and lyric lines, where chord positions align with the corresponding lyrics below them:
```chord
Bm                     F#7
On a dark desert highway, cool wind in my hair
A                      E
Warm smell of colitas, rising up through the air
```

Chord-only rows (e.g., intros, instrumental breaks) are cleanly recognized as tab blocks:
```chord
Bm  F#7  A  E  G  D  Em  F#7
```

Line comments are supported using `#` or `//`.

---

## 2. Haskell CLI Compiler (`chordbook`)

### Architecture
- **`AST.hs`**: Abstract Syntax Tree describing songs, metadata, sections, chord/lyric lines, and musical structures (roots, accidentals, qualities, bass inversion).
- **`ChordValidator.hs`**: Parsec-based chord validator parsing standard and complex chords (`C#m7b5`, `Fmaj7/A`, `Dsus4`, `G7`, `Bbadd9`, etc.) and determining chord-line contexts.
- **`Parser.hs`**: Tokenizer and alignment engine that pairs chords directly with lyric syllables based on character column positioning.
- **`CSS.hs`**: Responsive, publication-ready modern dark theme stylesheet with automated print stylesheet for sheet music printing.
- **`HtmlRenderer.hs`**: Generates clean, semantic HTML markup with chord badges, inline chord-lyric pairs, and metadata tags.
- **`Main.hs`**: CLI interface supporting standard arguments (`-o <file>`) and UNIX pipelines (`stdin` / `stdout`).

### Usage
```bash
# Compile to a file
chordbook input.chord -o song.html

# Pipe input and output
chordbook < input.chord > song.html
```

---

## 3. VS Code Extension Core

The VS Code extension core provides immediate syntax highlighting:
- **`extension/package.json`**: Language contribution registering `.chord`, `.chords`, and `.crd` extensions.
- **`extension/language-configuration.json`**: Bracket matching and auto-closing configurations.
- **`extension/syntaxes/chordbook.tmLanguage.json`**: TextMate grammar utilizing lookaheads and token matching to capture:
  - Top-of-file metadata keys and values (`entity.name.tag.metadata.key`)
  - Section headers (`markup.heading`)
  - Contextual chord lines and individual chord anatomy (roots, accidentals, chord qualities, slash bass notes).

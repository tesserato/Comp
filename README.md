# Comp (Chord Sheet Compiler)

Comp is an elegant plain-text markup language designed to compile song sheets into publication-grade HTML chordbooks with CifraClub-style interactive chord diagrams, optimal biomechanical fingering algorithms, and a VS Code syntax highlighting extension.

---

## 1. Syntax & Grammar Rules

A Comp song sheet (`.chord`, `.chords`, `.crd`) consists of:

### 1. Metadata Block (Top of File)
Sequential lines at the beginning of the file matching `Key: Value`:
```chord
Title: Karma Police
Artist: Radiohead
Key: Am
Capo: None
```

### 2. Custom Chord Definitions (`chordname:xxxxxx`)
Define or override guitar voicings anywhere in the file with string frets from Low E to High E:
```chord
D/F#:200232
G/F#:2x0003
Bm/D:xx0432
```
- Digits represent frets (`0` for open string, numbers for fret position).
- `x` or `X` indicates a muted string.
- Frets can also be space/comma separated for frets $\ge 10$ (e.g. `C:x 3 5 5 5 3`).

### 3. Section Headers
Optional section headings in bracketed or markdown style:
```chord
[Intro]
[Verse 1]
== Chorus ==
```

### 4. Song Blocks (Alternating Chord Lines & Lyric Lines)
Write chords directly above their corresponding lyric syllables. Comp automatically calculates character offsets to align them vertically:
```chord
Am         D/F#   Em
Karma police, arrest this man
   G            Am
He talks in maths
```

Chord-only rows (e.g., intros, instrumental breaks) are formatted as clean tab blocks:
```chord
Am  D/F#  Em  G  Am  F  Em  G
```

Line comments are supported using `#` or `//`.

---

## 2. Interactive Features & Optimal Fingering Algorithm

### Hover Chord Diagrams (CifraClub Style)
- Hovering over any chord in the rendered HTML displays an interactive SVG chord diagram popover showing:
  - Exact string lines and fret positions
  - Nut or fret-number markers (`3fr`, `7fr`)
  - Open string (`○`) and muted string (`×`) indicators
  - Barre clamps with span across frets
  - Finger placement numbers (`1`=Index, `2`=Middle, `3`=Ring, `4`=Pinky)
- Chords used in the song are automatically summarized in a visual chord palette at the bottom of the page.

### Biomechanical Optimal Fingering Algorithm
Based on research in algorithmic guitar ergonomics (Grozman / Sayegh / Radicioni):
- **Candidate Generation**: Evaluates permutations of 4 left-hand fingers across fret positions and identifies optimal full/partial barre opportunities for finger 1.
- **Biomechanical Cost Function**:
  $$\text{Cost} = W_{\text{span}} \cdot \Delta\text{fret} + \sum W_{\text{stretch}} \cdot |\Delta\text{dist} - \text{naturalDist}| + \sum W_{\text{fingerEffort}} + \text{BarrePenalty}$$
- **Physical Constraints**: Enforces physiological hand rules preventing impossible finger crossings.
- **Finger Effort Weighting**: Penalizes awkward finger utilization (e.g. overusing the pinky when the ring finger is ergonomically superior).

---

## 3. Haskell CLI Compiler (`chordbook`)

### Architecture
- **`AST.hs`**: Abstract Syntax Tree capturing songs, custom chord definitions (`FretDef`), chord diagrams, sections, and lyric alignments.
- **`Fingering.hs`**: Biomechanical optimal fingering solver and comprehensive chord library.
- **`SvgRenderer.hs`**: Pure SVG generator creating crisp vector chord diagrams with barre markers, string states, and finger dots.
- **`Parser.hs`**: Parser supporting top metadata, inline `chordname:xxxxxx` definitions, and syllable alignment.
- **`CSS.hs`**: Responsive dark theme stylesheet with CifraClub popovers and print stylesheets.
- **`HtmlRenderer.hs`**: Generates self-contained HTML with inline interactive chord popovers.
- **`Main.hs`**: CLI interface supporting standard arguments (`-o <file>`) and UNIX pipelines (`stdin` / `stdout`).

### Usage
```bash
# Compile song sheet to HTML
chordbook examples/karma_police.chord -o karma_police.html

# Pipe through stdin / stdout
chordbook < song.chord > song.html
```

---

## 4. VS Code Extension Core

- **`extension/package.json`**: Contributes the `chordbook` language ID (`.chord`, `.chords`, `.crd`).
- **`extension/language-configuration.json`**: Comment and bracket auto-closing rules.
- **`extension/syntaxes/chordbook.tmLanguage.json`**: TextMate grammar highlighting metadata, custom chord definitions (`chordname:xxxxxx`), section headers, and chord anatomy.

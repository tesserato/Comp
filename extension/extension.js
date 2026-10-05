const vscode = require('vscode');

const CHORD_LIBRARY = {
  'C':      [null, 3, 2, 0, 1, 0],
  'Cmaj7':  [null, 3, 2, 0, 0, 0],
  'C7':     [null, 3, 2, 3, 1, 0],
  'Cm':     [null, 3, 5, 5, 4, 3],
  'Cm7':    [null, 3, 5, 3, 4, 3],
  'D':      [null, null, 0, 2, 3, 2],
  'Dm':     [null, null, 0, 2, 3, 1],
  'D7':     [null, null, 0, 2, 1, 2],
  'Dmaj7':  [null, null, 0, 2, 2, 2],
  'Dsus2':  [null, null, 0, 2, 3, 0],
  'Dsus4':  [null, null, 0, 2, 3, 3],
  'E':      [0, 2, 2, 1, 0, 0],
  'Em':     [0, 2, 2, 0, 0, 0],
  'E7':     [0, 2, 0, 1, 0, 0],
  'Em7':    [0, 2, 2, 0, 3, 0],
  'Esus4':  [0, 2, 2, 2, 0, 0],
  'F':      [1, 3, 3, 2, 1, 1],
  'Fm':     [1, 3, 3, 1, 1, 1],
  'Fmaj7':  [null, null, 3, 2, 1, 0],
  'F#':     [2, 4, 4, 3, 2, 2],
  'F#m':    [2, 4, 4, 2, 2, 2],
  'F#7':    [2, 4, 2, 3, 2, 2],
  'G':      [3, 2, 0, 0, 0, 3],
  'Gm':     [3, 5, 5, 3, 3, 3],
  'G7':     [3, 2, 0, 0, 0, 1],
  'Gsus4':  [3, 2, 0, 0, 1, 3],
  'A':      [null, 0, 2, 2, 2, 0],
  'Am':     [null, 0, 2, 2, 1, 0],
  'A7':     [null, 0, 2, 0, 2, 0],
  'Am7':    [null, 0, 2, 0, 1, 0],
  'Amaj7':  [null, 0, 2, 1, 2, 0],
  'Asus2':  [null, 0, 2, 2, 0, 0],
  'Asus4':  [null, 0, 2, 2, 3, 0],
  'B':      [null, 2, 4, 4, 4, 2],
  'Bm':     [null, 2, 4, 4, 3, 2],
  'B7':     [null, 2, 1, 2, 0, 2],
  'Bm7':    [null, 2, 4, 2, 3, 2],
  'Bb':     [null, 1, 3, 3, 3, 1],
  'Bbm':    [null, 1, 3, 3, 2, 1]
};

function solveFingering(frets) {
  const pts = [];
  for (let s = 0; s < 6; s++) {
    const f = frets[s];
    if (f !== null && f > 0) {
      pts.push({ str: s + 1, fret: f });
    }
  }

  if (pts.length === 0) {
    return { fingers: [null, null, null, null, null, null], barres: [], baseFret: 1 };
  }

  const minFret = Math.min(...pts.map(p => p.fret));
  const maxFret = Math.max(...pts.map(p => p.fret));
  const baseFret = maxFret <= 4 ? 1 : minFret;

  const barreNotes = pts.filter(p => p.fret === minFret);
  const otherNotes = pts.filter(p => p.fret > minFret).sort((a, b) => a.fret - b.fret);

  if (barreNotes.length >= 2 && otherNotes.length <= 3) {
    const minStr = Math.min(...barreNotes.map(p => p.str));
    const maxStr = Math.max(...barreNotes.map(p => p.str));
    const fingerArr = [null, null, null, null, null, null];
    for (const p of barreNotes) fingerArr[p.str - 1] = 1;
    let nextFinger = 2;
    for (const p of otherNotes) {
      fingerArr[p.str - 1] = nextFinger++;
    }
    return { fingers: fingerArr, barres: [{ fret: minFret, minStr, maxStr }], baseFret };
  }

  const sorted = [...pts].sort((a, b) => a.fret - b.fret);
  const fingerArr = [null, null, null, null, null, null];
  let fNum = 1;
  for (const p of sorted) {
    fingerArr[p.str - 1] = Math.min(4, fNum++);
  }
  return { fingers: fingerArr, barres: [], baseFret };
}

function renderHoverContent(chordName, frets) {
  const { fingers, barres, baseFret } = solveFingering(frets);
  const stringNames = ['E', 'A', 'D', 'G', 'B', 'e'];
  const headerSymbols = frets.map(f => f === null ? 'x' : f === 0 ? 'o' : ' ').join('   ');

  let asciiGrid = `  ${chordName} Chord Diagram\n\n`;
  asciiGrid += `   ${headerSymbols}\n`;
  asciiGrid += `  ${baseFret === 1 ? '==+===+===+===+===+==' : '--+---+---+---+---+--'}\n`;

  const numFrets = 4;
  for (let f = 0; f < numFrets; f++) {
    const currentFret = baseFret + f;
    let row = '';
    const isBarreFret = barres.some(b => b.fret === currentFret);

    for (let s = 0; s < 6; s++) {
      const fretVal = frets[s];
      if (fretVal === currentFret) {
        const finger = fingers[s];
        row += finger ? ` ${finger} ` : ' o ';
      } else if (isBarreFret) {
        const b = barres.find(b => b.fret === currentFret);
        if (s + 1 >= b.minStr && s + 1 <= b.maxStr) {
          row += '===';
        } else {
          row += '---';
        }
      } else {
        row += '---';
      }
      if (s < 5) row += '|';
    }
    const fretLabel = baseFret > 1 && f === 0 ? `  ${currentFret}fr` : '';
    asciiGrid += `  ${row}${fretLabel}\n`;
  }

  const fretStr = frets.map(f => f === null ? 'x' : f.toString()).join('');
  const fingerSummary = fingers.map((f, i) => {
    if (frets[i] === null) return `${stringNames[i]}: muted`;
    if (frets[i] === 0) return `${stringNames[i]}: open`;
    const fNames = ['', 'Index', 'Middle', 'Ring', 'Pinky'];
    return `${stringNames[i]}: fret ${frets[i]} (${fNames[f] || 'finger ' + f})`;
  }).join(' | ');

  const md = new vscode.MarkdownString();
  md.isTrusted = true;
  md.appendMarkdown(`### **${chordName}** \`[${fretStr}]\`\n\n`);
  md.appendCodeblock(asciiGrid, 'text');
  md.appendMarkdown(`\n**Fingering:** ${fingerSummary}\n`);
  return md;
}

function findCustomChords(document) {
  const map = new Map();
  const lineCount = document.lineCount;
  for (let i = 0; i < lineCount; i++) {
    const rawLine = document.lineAt(i).text.trim();
    if (!rawLine || rawLine.startsWith('#') || rawLine.startsWith('//')) continue;
    const colonIdx = rawLine.indexOf(':');
    if (colonIdx > 0) {
      const name = rawLine.substring(0, colonIdx).trim();
      const val = rawLine.substring(colonIdx + 1).trim();
      const frets = parseFretSpec(val);
      if (frets && frets.length === 6) {
        map.set(name, frets);
      }
    }
  }
  return map;
}

function parseFretSpec(str) {
  if (str.includes(' ') || str.includes(',')) {
    const tokens = str.replace(/,/g, ' ').trim().split(/\s+/);
    if (tokens.length !== 6) return null;
    return tokens.map(t => t.toUpperCase() === 'X' ? null : parseInt(t, 10));
  }
  if (str.length === 6) {
    return str.split('').map(c => c.toUpperCase() === 'X' ? null : parseInt(c, 10));
  }
  return null;
}

const CHORD_REGEX = /(?:^|(?<=\s|[|/%~()\[\]]))([A-G][b#♯♭]?(?:maj9|maj7|maj|min9|min7|min|m9|m7b5|m7|m6|m|7sus4|sus4|sus2|sus|dim7|dim|aug7|aug|add9|add2|add11|13|11|9|7|6|5|[+oøΔ])?(?:\/[A-G][b#♯♭]?)?)(?=\s|[|/%~()\[\],.:]|$)/g;

function activate(context) {
  const hoverProvider = vscode.languages.registerHoverProvider('chordbook', {
    provideHover(document, position) {
      const line = position.line;
      const col = position.character;
      const lineText = document.lineAt(line).text;
      if (/^\s*(#|\/\/)/.test(lineText)) return null;

      let matchedChord = null;
      let matchedRange = null;

      // 1. If user is hovering over a definition line itself: "D/F#:200232"
      const colonIdx = lineText.indexOf(':');
      if (colonIdx > 0) {
        const potentialKey = lineText.substring(0, colonIdx).trim();
        const potentialVal = lineText.substring(colonIdx + 1).trim();
        const frets = parseFretSpec(potentialVal);
        if (frets && frets.length === 6) {
          const keyStart = lineText.indexOf(potentialKey);
          const keyEnd = keyStart + potentialKey.length;
          if (col >= keyStart && col <= keyEnd) {
            matchedChord = potentialKey;
            matchedRange = new vscode.Range(line, keyStart, line, keyEnd);
            return new vscode.Hover(renderHoverContent(matchedChord, frets), matchedRange);
          }
        }
      }

      // 2. Scan for chords on the line
      CHORD_REGEX.lastIndex = 0;
      let m;
      while ((m = CHORD_REGEX.exec(lineText)) !== null) {
        const chordName = m[1];
        const start = m.index + (m[0].length - chordName.length);
        const end = start + chordName.length;
        if (col >= start && col <= end) {
          matchedChord = chordName;
          matchedRange = new vscode.Range(line, start, line, end);
          break;
        }
      }

      // 3. Fallback: try word range at position
      if (!matchedChord) {
        const wordRange = document.getWordRangeAtPosition(position, /[A-G][b#♯♭]?(?:maj9|maj7|maj|min9|min7|min|m9|m7b5|m7|m6|m|7sus4|sus4|sus2|sus|dim7|dim|aug7|aug|add9|add2|add11|13|11|9|7|6|5|[+oøΔ])?(?:\/[A-G][#b♯♭]?)?/);
        if (wordRange) {
          matchedChord = document.getText(wordRange);
          matchedRange = wordRange;
        }
      }

      if (!matchedChord) return null;

      const customChords = findCustomChords(document);
      let frets = customChords.get(matchedChord);
      if (!frets) {
        frets = CHORD_LIBRARY[matchedChord];
      }

      if (frets) {
        return new vscode.Hover(renderHoverContent(matchedChord, frets), matchedRange);
      }
      return null;
    }
  });

  context.subscriptions.push(hoverProvider);
}

function deactivate() {}

module.exports = {
  activate,
  deactivate
};


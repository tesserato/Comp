const vscode = require('vscode');

const ROOT_PITCH = { C: 0, D: 2, E: 4, F: 5, G: 7, A: 9, B: 11 };
const STANDARD_TUNING = [4, 9, 2, 7, 11, 4]; // E A D G B e
const QUALITY_INTERVALS = [
  ['maj9', [0, 4, 7, 11, 14]],
  ['maj7', [0, 4, 7, 11]],
  ['maj', [0, 4, 7]],
  ['min9', [0, 3, 7, 10, 14]],
  ['min7', [0, 3, 7, 10]],
  ['min', [0, 3, 7]],
  ['m9', [0, 3, 7, 10, 14]],
  ['m7b5', [0, 3, 6, 10]],
  ['m7', [0, 3, 7, 10]],
  ['m6', [0, 3, 7, 9]],
  ['m', [0, 3, 7]],
  ['7sus4', [0, 5, 7, 10]],
  ['sus4', [0, 5, 7]],
  ['sus2', [0, 2, 7]],
  ['sus', [0, 5, 7]],
  ['dim7', [0, 3, 6, 9]],
  ['dim', [0, 3, 6]],
  ['aug7', [0, 4, 8, 10]],
  ['aug', [0, 4, 8]],
  ['add11', [0, 4, 7, 17]],
  ['add9', [0, 4, 7, 14]],
  ['add2', [0, 2, 4, 7]],
  ['13', [0, 4, 7, 10, 14, 21]],
  ['11', [0, 4, 7, 10, 14, 17]],
  ['9', [0, 4, 7, 10, 14]],
  ['7', [0, 4, 7, 10]],
  ['6', [0, 4, 7, 9]],
  ['5', [0, 7]],
  ['+', [0, 4, 8]],
  ['o', [0, 3, 6]],
  ['ø', [0, 3, 6, 10]],
  ['Δ', [0, 4, 7, 11]],
  ['', [0, 4, 7]]
];

function pitchClass(note) {
  const m = /^([A-G])([b#♯♭]?)$/.exec(note);
  if (!m) return null;
  const accidental = m[2] === '#' || m[2] === '♯' ? 1 : (m[2] === 'b' || m[2] === '♭' ? -1 : 0);
  return (ROOT_PITCH[m[1]] + accidental + 12) % 12;
}

function parseChord(chordName) {
  const m = /^([A-G][b#♯♭]?)(.*?)(?:\/([A-G][b#♯♭]?))?$/.exec(chordName);
  if (!m) return null;
  const rootPc = pitchClass(m[1]);
  const qualityText = m[2] || '';
  const bassPc = m[3] ? pitchClass(m[3]) : rootPc;
  const quality = QUALITY_INTERVALS.find(([q]) => q === qualityText);
  if (rootPc === null || bassPc === null || !quality) return null;
  const tones = [...new Set(quality[1].map(i => (rootPc + i) % 12))];
  return { tones, bassPc };
}

function deriveVoicing(chordName) {
  const chord = parseChord(chordName);
  if (!chord) return null;
  const candidates = allVoicings(chord.tones).filter(shape => isUsableVoicing(chord.tones, chord.bassPc, shape));
  if (candidates.length === 0) return null;
  candidates.sort((a, b) => voicingCost(chord.tones, chord.bassPc, a) - voicingCost(chord.tones, chord.bassPc, b));
  return candidates[0];
}

function allVoicings(tones) {
  let shapes = [[]];
  for (const openPc of STANDARD_TUNING) {
    const options = [null];
    for (let fret = 0; fret <= 12; fret++) {
      if (tones.includes((openPc + fret) % 12)) options.push(fret);
    }
    const next = [];
    for (const prefix of shapes) {
      for (const opt of options) next.push([...prefix, opt]);
    }
    shapes = next;
  }
  return shapes.filter(shape => countSounding(shape) >= 3 && fretSpan(shape) <= 4);
}

function isUsableVoicing(tones, bassPc, shape) {
  const pcs = soundingPitchClasses(shape);
  const required = tones.slice(0, Math.min(3, tones.length));
  return pcs.length > 0 && lowestPitchClass(shape) === bassPc && required.every(pc => pcs.includes(pc));
}

function soundingPitchClasses(shape) {
  return [...new Set(shape.flatMap((fret, i) => fret === null ? [] : [(STANDARD_TUNING[i] + fret) % 12]))];
}

function lowestPitchClass(shape) {
  for (let i = 0; i < shape.length; i++) {
    if (shape[i] !== null) return (STANDARD_TUNING[i] + shape[i]) % 12;
  }
  return null;
}

function countSounding(shape) {
  return shape.filter(f => f !== null).length;
}

function fretSpan(shape) {
  const fretted = shape.filter(f => f !== null && f > 0);
  if (fretted.length === 0) return 0;
  return Math.max(...fretted) - Math.min(...fretted);
}

function voicingCost(tones, bassPc, shape) {
  const fretted = shape.filter(f => f !== null && f > 0);
  const mutedPenalty = shape.filter(f => f === null).length * 1.4;
  const fretPenalty = fretted.reduce((a, b) => a + b, 0) * 0.18;
  const spanPenalty = fretSpan(shape) * 2.2;
  const pcs = soundingPitchClasses(shape);
  const coverageBonus = tones.filter(pc => pcs.includes(pc)).length * -1.5;
  const bassBonus = lowestPitchClass(shape) === bassPc ? -8 : 20;
  return mutedPenalty + fretPenalty + spanPenalty + coverageBonus + bassBonus;
}


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

function renderRecognizedChordHover(chordName) {
  const md = new vscode.MarkdownString();
  md.appendMarkdown(`### **${chordName}**\n\n`);
  md.appendMarkdown('Recognized chord. Add a custom voicing such as `D9/F#:200232` to show a diagram.');
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
        frets = deriveVoicing(matchedChord);
      }

      if (frets) {
        return new vscode.Hover(renderHoverContent(matchedChord, frets), matchedRange);
      }
      return new vscode.Hover(renderRecognizedChordHover(matchedChord), matchedRange);
    }
  });

  context.subscriptions.push(hoverProvider);
}

function deactivate() {}

module.exports = {
  activate,
  deactivate
};


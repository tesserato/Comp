module AST where

data Song = Song
  { songMetadata       :: [(String, String)]
  , songCustomChords   :: [(String, FretDef)]
  , songSections       :: [Section]
  } deriving (Show, Eq)

data Section = Section
  { sectionName :: Maybe String
  , sectionItems :: [SectionItem]
  } deriving (Show, Eq)

data SectionItem
  = PairedLine [ChordSyllable]
  | ChordOnlyLine [PlacedChord]
  | LyricOnlyLine String
  | CommentLine String
  deriving (Show, Eq)

data PlacedChord = PlacedChord
  { chordCol    :: Int
  , chordName   :: String
  , chordParsed :: Maybe Chord
  } deriving (Show, Eq)

-- Custom defined chord diagram: string frets (6 strings: E A D G B E, e.g. "x32010" or [Nothing, Just 3, Just 2, Just 0, Just 1, Just 0])
data FretDef = FretDef
  { fretStrings :: [Maybe Int]
  } deriving (Show, Eq)

-- Optimal fingering for 6 strings: 0=open, Nothing=mute, Just 1..4 = finger (1=index, 2=middle, 3=ring, 4=pinky)
data ChordDiagram = ChordDiagram
  { diagFrets    :: [Maybe Int]         -- 6 strings, low E to high E
  , diagFingers  :: [Maybe Int]         -- 6 strings, finger assigned: Nothing (mute/open), Just 1..4
  , diagBarres   :: [(Int, Int, Int)]   -- (fret, startString, endString) e.g. fret 1 from string 1 to 6
  , diagBaseFret :: Int                 -- offset if higher up the neck (1-based)
  } deriving (Show, Eq)

data ChordSyllable = ChordSyllable
  { csChord :: Maybe (String, Maybe Chord)
  , csLyric :: String
  } deriving (Show, Eq)

data RootNote = A | B | C | D | E | F | G
  deriving (Show, Eq, Enum, Bounded)

data Accidental = Natural | Sharp | Flat
  deriving (Show, Eq)

data ChordQuality
  = Major
  | Minor
  | Dominant7
  | Major7
  | Minor7
  | Diminished
  | Diminished7
  | HalfDiminished
  | Augmented
  | Augmented7
  | Sus2
  | Sus4
  | Sus24
  | SevenSus4
  | Add9
  | Add2
  | Add11
  | Sixth
  | Minor6
  | Ninth
  | Major9
  | Minor9
  | Eleventh
  | Thirteenth
  | PowerChord
  deriving (Show, Eq)

data Chord = Chord
  { chordRoot    :: (RootNote, Accidental)
  , chordQuality :: ChordQuality
  , chordBass    :: Maybe (RootNote, Accidental)
  } deriving (Show, Eq)

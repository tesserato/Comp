module AST where

data Song = Song
  { songMetadata :: [(String, String)]
  , songSections :: [Section]
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

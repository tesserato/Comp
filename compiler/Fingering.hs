module Fingering
  ( FingeringResult(..)
  , solveOptimalFingering
  , solveChordDiagram
  , deriveChordDiagram
  ) where

import AST
import Data.List (minimumBy, nub, sortOn)
import Data.Ord (comparing)
import Data.Maybe (mapMaybe)

data FingeringResult = FingeringResult
  { resFingers  :: [Maybe Int]         -- 6 strings: Nothing (open/muted), Just 1..4
  , resBarres   :: [(Int, Int, Int)]   -- (fret, startString, endString)
  , resBaseFret :: Int                 -- minimum fret offset if higher up neck
  , resCost     :: Double
  } deriving (Show, Eq)

data FretPoint = FretPoint
  { fpString :: Int -- 1 to 6
  , fpFret   :: Int -- > 0
  } deriving (Show, Eq)

fingerEffortWeight :: Int -> Double
fingerEffortWeight 1 = 1.0
fingerEffortWeight 2 = 1.0
fingerEffortWeight 3 = 1.2
fingerEffortWeight 4 = 1.5
fingerEffortWeight _ = 2.0

solveOptimalFingering :: [Maybe Int] -> FingeringResult
solveOptimalFingering frets =
  let frettedPoints = [ FretPoint str fret
                      | (str, mFret) <- zip [1..6] frets
                      , Just fret <- [mFret]
                      , fret > 0
                      ]
      minFret = if null frettedPoints then 1 else minimum (map fpFret frettedPoints)
      maxFret = if null frettedPoints then 1 else maximum (map fpFret frettedPoints)
      baseFret = if maxFret <= 4 then 1 else minFret
  in if null frettedPoints
     then FingeringResult (replicate 6 Nothing) [] 1 0.0
     else
       let candidates = generateCandidates frettedPoints
       in case candidates of
            [] -> fallbackFingering frets frettedPoints baseFret
            _  ->
              let best = minimumBy (comparing resCost) candidates
              in best { resBaseFret = baseFret }

generateCandidates :: [FretPoint] -> [FingeringResult]
generateCandidates pts =
  let nonBarre = generateNonBarreCandidates pts
      barre    = generateBarreCandidates pts
  in nonBarre ++ barre

generateNonBarreCandidates :: [FretPoint] -> [FingeringResult]
generateNonBarreCandidates pts
  | length pts > 4 = []
  | otherwise =
      let sortedPts = sortOn fpFret pts
          allPerms = selectFingers (length pts) [1..4]
      in mapMaybe (scoreNonBarre sortedPts) allPerms

selectFingers :: Int -> [Int] -> [[Int]]
selectFingers 0 _ = [[]]
selectFingers _ [] = []
selectFingers n (f:fs) =
  map (f:) (selectFingers (n - 1) fs) ++ selectFingers n fs



scoreNonBarre :: [FretPoint] -> [Int] -> Maybe FingeringResult
scoreNonBarre pts fingers =
  let pairs = zip pts fingers
  in if isValidBiomechanicalHand pairs
     then
       let cost = computeBiomechanicalCost pairs False
           fingerArr = buildFingerArray pairs
       in Just (FingeringResult fingerArr [] 1 cost)
     else Nothing

generateBarreCandidates :: [FretPoint] -> [FingeringResult]
generateBarreCandidates pts =
  let frets = nub (map fpFret pts)
  in concatMap (tryBarreOnFret pts) frets

tryBarreOnFret :: [FretPoint] -> Int -> [FingeringResult]
tryBarreOnFret pts bFret =
  let barreNotes = filter (\p -> fpFret p == bFret) pts
      otherNotes = filter (\p -> fpFret p > bFret) pts
  in if length barreNotes < 2 || length otherNotes > 3
     then []
     else
       let minStr = minimum (map fpString barreNotes)
           maxStr = maximum (map fpString barreNotes)
           otherFingers = [2, 3, 4]
           otherPerms = selectFingers (length otherNotes) otherFingers
       in mapMaybe (scoreBarreCandidate bFret minStr maxStr barreNotes otherNotes) otherPerms

scoreBarreCandidate :: Int -> Int -> Int -> [FretPoint] -> [FretPoint] -> [Int] -> Maybe FingeringResult
scoreBarreCandidate bFret minStr maxStr barreNotes otherNotes otherFingers =
  let pairs = map (\p -> (p, 1)) barreNotes ++ zip otherNotes otherFingers
  in if isValidBiomechanicalHand pairs
     then
       let baseCost = computeBiomechanicalCost pairs True
           barreSpanPenalty = fromIntegral (maxStr - minStr) * 1.5 + 4.0
           fingerArr = buildFingerArray pairs
       in Just (FingeringResult fingerArr [(bFret, minStr, maxStr)] 1 (baseCost + barreSpanPenalty))
     else Nothing

isValidBiomechanicalHand :: [(FretPoint, Int)] -> Bool
isValidBiomechanicalHand pairs =
  let checkPair (p1, f1) (p2, f2) =
        if fpFret p1 < fpFret p2
        then f1 <= f2
        else if fpFret p1 > fpFret p2
        then f1 >= f2
        else True
      allPairs = [ (a, b) | a <- pairs, b <- pairs, a /= b ]
  in all (uncurry checkPair) allPairs

computeBiomechanicalCost :: [(FretPoint, Int)] -> Bool -> Double
computeBiomechanicalCost pairs _ =
  let fretSpanCost =
        let frets = map (fpFret . fst) pairs
        in if null frets then 0.0 else fromIntegral (maximum frets - minimum frets) * 3.0
      stretchCost = sum [ pairStretchCost a b | a <- pairs, b <- pairs, fst a /= fst b ]
      fingerEffort = sum [ fingerEffortWeight (snd p) | p <- pairs ]
  in fretSpanCost + stretchCost + fingerEffort

pairStretchCost :: (FretPoint, Int) -> (FretPoint, Int) -> Double
pairStretchCost (p1, f1) (p2, f2) =
  let dFret = abs (fpFret p2 - fpFret p1)
      idealDist = fromIntegral (abs (f2 - f1))
      distDiff = fromIntegral dFret - idealDist
  in if distDiff > 0 then distDiff * 2.5 else 0.0

buildFingerArray :: [(FretPoint, Int)] -> [Maybe Int]
buildFingerArray pairs =
  [ lookupFinger str pairs | str <- [1..6] ]
  where
    lookupFinger _ [] = Nothing
    lookupFinger s ((p, f):rest)
      | fpString p == s = Just f
      | otherwise       = lookupFinger s rest

fallbackFingering :: [Maybe Int] -> [FretPoint] -> Int -> FingeringResult
fallbackFingering _ pts baseFret =
  let sorted = sortOn fpFret pts
      assigned = zip sorted ([1..4] ++ repeat 4)
      fingerArr = [ case [ f | (p, f) <- assigned, fpString p == s ] of
                      (f:_) -> Just f
                      []    -> Nothing
                  | s <- [1..6]
                  ]
  in FingeringResult fingerArr [] baseFret 10.0

solveChordDiagram :: [Maybe Int] -> ChordDiagram
solveChordDiagram frets =
  let res = solveOptimalFingering frets
  in ChordDiagram frets (resFingers res) (resBarres res) (resBaseFret res)

deriveChordDiagram :: Chord -> Maybe ChordDiagram
deriveChordDiagram chord = solveChordDiagram <$> deriveVoicing chord

type PitchClass = Int

standardTuning :: [PitchClass]
standardTuning = [4, 9, 2, 7, 11, 4] -- E A D G B e

deriveVoicing :: Chord -> Maybe [Maybe Int]
deriveVoicing chord =
  let rootPc = notePitchClass (chordRoot chord)
      chordTones = chordPitchClasses chord
      bassPc = maybe rootPc notePitchClass (chordBass chord)
      allTones = nub (chordTones ++ [bassPc])
      candidates = filter (isUsableVoicing allTones rootPc bassPc) (allVoicings allTones)
  in case candidates of
       [] -> Nothing
       xs -> Just (minimumBy (comparing (voicingCost allTones bassPc)) xs)

allVoicings :: [PitchClass] -> [[Maybe Int]]
allVoicings tones =
  [ shape
  | shape <- sequence [stringOptions openPc tones | openPc <- standardTuning]
  , countSounding shape >= 4
  , fretSpan shape <= 4
  ]

stringOptions :: PitchClass -> [PitchClass] -> [Maybe Int]
stringOptions openPc tones =
  Nothing : [ Just fret | fret <- [0..12], ((openPc + fret) `mod` 12) `elem` tones ]

hasImpossibleBarreWithOpen :: [Maybe Int] -> Bool
hasImpossibleBarreWithOpen shape =
  let fretted = [ (fret, str) | (str, Just fret) <- zip [0..5] shape, fret > 0 ]
  in case fretted of
       [] -> False
       _  ->
         let minFret = minimum (map fst fretted)
             minNotes = filter (\(f, _) -> f == minFret) fretted
         in if length minNotes >= 2
            then
              let minStr = minimum (map snd minNotes)
                  maxStr = maximum (map snd minNotes)
              in any (\s -> shape !! s == Just 0) [minStr .. maxStr]
            else False

requiredChordTones :: [PitchClass] -> PitchClass -> PitchClass -> [PitchClass]
requiredChordTones allTones rootPc bassPc =
  let baseTones = filter (/= bassPc) allTones
  in if length baseTones <= 3
     then allTones
     else let perf5 = (rootPc + 7) `mod` 12
          in filter (/= perf5) allTones

isUsableVoicing :: [PitchClass] -> PitchClass -> PitchClass -> [Maybe Int] -> Bool
isUsableVoicing tones rootPc bassPc shape =
  let pcs = soundingPitchClasses shape
      required = requiredChordTones tones rootPc bassPc
  in not (hasImpossibleBarreWithOpen shape)
     && not (null pcs)
     && lowestPitchClass shape == Just bassPc
     && all (`elem` pcs) required
     && countSounding shape >= 4

soundingPitchClasses :: [Maybe Int] -> [PitchClass]
soundingPitchClasses shape = nub
  [ (openPc + fret) `mod` 12
  | (openPc, Just fret) <- zip standardTuning shape
  ]

lowestPitchClass :: [Maybe Int] -> Maybe PitchClass
lowestPitchClass shape = case [ (openPc + fret) `mod` 12 | (openPc, Just fret) <- zip standardTuning shape ] of
  []    -> Nothing
  pc:_  -> Just pc

countSounding :: [Maybe Int] -> Int
countSounding = length . filter (/= Nothing)

fretSpan :: [Maybe Int] -> Int
fretSpan shape =
  case [ fret | Just fret <- shape, fret > 0 ] of
    [] -> 0
    fs -> maximum fs - minimum fs

voicingCost :: [PitchClass] -> PitchClass -> [Maybe Int] -> Double
voicingCost tones bassPc shape =
  let fretted = [ fret | Just fret <- shape, fret > 0 ]
      sounding = countSounding shape
      maxFret = if null fretted then 0 else maximum fretted
      minFret = if null fretted then 0 else minimum fretted

      firstSounding = case [ idx | (idx, m) <- zip [0..5] shape, m /= Nothing ] of
                        (i:_) -> i
                        []    -> 0
      trailingMutes = length [ () | m <- drop firstSounding shape, m == Nothing ]
      internalMutePen = fromIntegral trailingMutes * 8.0
      contiguousPen = fromIntegral (internalMuteCount shape) * 15.0

      fullBarreBonus = if firstSounding == 0 && sounding == 6 then -4.0 else 0.0
      fiveStringBonus = if firstSounding == 1 && sounding == 5 then -3.0 else 0.0

      posPenalty = fromIntegral maxFret * 3.0 + (if minFret > 3 then fromIntegral minFret * 6.0 else 0.0)
      spanPen = fromIntegral (fretSpan shape) * 3.0

      expectedSounding = 6 - firstSounding
      fullnessPen = fromIntegral (expectedSounding - sounding) * 4.0

      pcs = soundingPitchClasses shape
      covBonus = fromIntegral (length (filter (`elem` pcs) tones)) * (-2.0)
      openBonus = fromIntegral (length [ () | Just 0 <- shape ]) * (-2.0)
      bassBonus = if lowestPitchClass shape == Just bassPc then -10.0 else 30.0
  in internalMutePen + contiguousPen + fullBarreBonus + fiveStringBonus + posPenalty + spanPen + fullnessPen + covBonus + openBonus + bassBonus

internalMuteCount :: [Maybe Int] -> Int
internalMuteCount shape =
  case dropWhile (== Nothing) (reverse (dropWhile (== Nothing) shape)) of
    []       -> 0
    sounding -> length [ () | Nothing <- sounding ]

notePitchClass :: (RootNote, Accidental) -> PitchClass
notePitchClass (root, acc) = (rootBase root + accidentalOffset acc) `mod` 12
  where
    rootBase C = 0
    rootBase D = 2
    rootBase E = 4
    rootBase F = 5
    rootBase G = 7
    rootBase A = 9
    rootBase B = 11
    accidentalOffset Natural = 0
    accidentalOffset Sharp = 1
    accidentalOffset Flat = -1

chordPitchClasses :: Chord -> [PitchClass]
chordPitchClasses chord =
  let root = notePitchClass (chordRoot chord)
  in nub [ (root + i) `mod` 12 | i <- qualityIntervals (chordQuality chord) ]

qualityIntervals :: ChordQuality -> [Int]
qualityIntervals Major = [0,4,7]
qualityIntervals Minor = [0,3,7]
qualityIntervals Dominant7 = [0,4,7,10]
qualityIntervals Major7 = [0,4,7,11]
qualityIntervals Minor7 = [0,3,7,10]
qualityIntervals Diminished = [0,3,6]
qualityIntervals Diminished7 = [0,3,6,9]
qualityIntervals HalfDiminished = [0,3,6,10]
qualityIntervals Augmented = [0,4,8]
qualityIntervals Augmented7 = [0,4,8,10]
qualityIntervals Sus2 = [0,2,7]
qualityIntervals Sus4 = [0,5,7]
qualityIntervals Sus24 = [0,2,5,7]
qualityIntervals SevenSus4 = [0,5,7,10]
qualityIntervals Add9 = [0,4,7,14]
qualityIntervals Add2 = [0,2,4,7]
qualityIntervals Add11 = [0,4,7,17]
qualityIntervals Sixth = [0,4,7,9]
qualityIntervals Minor6 = [0,3,7,9]
qualityIntervals Ninth = [0,4,7,10,14]
qualityIntervals Major9 = [0,4,7,11,14]
qualityIntervals Minor9 = [0,3,7,10,14]
qualityIntervals Eleventh = [0,4,7,10,14,17]
qualityIntervals Thirteenth = [0,4,7,10,14,21]
qualityIntervals PowerChord = [0,7]

module Fingering
  ( FingeringResult(..)
  , solveOptimalFingering
  , solveChordDiagram
  , defaultChordLibrary
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


defaultChordLibrary :: [(String, [Maybe Int])]
defaultChordLibrary =
  [ ("C",      [Nothing, Just 3, Just 2, Just 0, Just 1, Just 0])
  , ("Cmaj7",  [Nothing, Just 3, Just 2, Just 0, Just 0, Just 0])
  , ("C7",     [Nothing, Just 3, Just 2, Just 3, Just 1, Just 0])
  , ("Cm",     [Nothing, Just 3, Just 5, Just 5, Just 4, Just 3])
  , ("Cm7",    [Nothing, Just 3, Just 5, Just 3, Just 4, Just 3])
  , ("D",      [Nothing, Nothing, Just 0, Just 2, Just 3, Just 2])
  , ("Dm",     [Nothing, Nothing, Just 0, Just 2, Just 3, Just 1])
  , ("D7",     [Nothing, Nothing, Just 0, Just 2, Just 1, Just 2])
  , ("Dmaj7",  [Nothing, Nothing, Just 0, Just 2, Just 2, Just 2])
  , ("Dsus2",  [Nothing, Nothing, Just 0, Just 2, Just 3, Just 0])
  , ("Dsus4",  [Nothing, Nothing, Just 0, Just 2, Just 3, Just 3])
  , ("E",      [Just 0, Just 2, Just 2, Just 1, Just 0, Just 0])
  , ("Em",     [Just 0, Just 2, Just 2, Just 0, Just 0, Just 0])
  , ("E7",     [Just 0, Just 2, Just 0, Just 1, Just 0, Just 0])
  , ("Em7",    [Just 0, Just 2, Just 2, Just 0, Just 3, Just 0])
  , ("Esus4",  [Just 0, Just 2, Just 2, Just 2, Just 0, Just 0])
  , ("F",      [Just 1, Just 3, Just 3, Just 2, Just 1, Just 1])
  , ("Fm",     [Just 1, Just 3, Just 3, Just 1, Just 1, Just 1])
  , ("Fmaj7",  [Nothing, Nothing, Just 3, Just 2, Just 1, Just 0])
  , ("F#",     [Just 2, Just 4, Just 4, Just 3, Just 2, Just 2])
  , ("F#m",    [Just 2, Just 4, Just 4, Just 2, Just 2, Just 2])
  , ("F#7",    [Just 2, Just 4, Just 2, Just 3, Just 2, Just 2])
  , ("G",      [Just 3, Just 2, Just 0, Just 0, Just 0, Just 3])
  , ("Gm",     [Just 3, Just 5, Just 5, Just 3, Just 3, Just 3])
  , ("G7",     [Just 3, Just 2, Just 0, Just 0, Just 0, Just 1])
  , ("Gsus4",  [Just 3, Just 2, Just 0, Just 0, Just 1, Just 3])
  , ("A",      [Nothing, Just 0, Just 2, Just 2, Just 2, Just 0])
  , ("Am",     [Nothing, Just 0, Just 2, Just 2, Just 1, Just 0])
  , ("A7",     [Nothing, Just 0, Just 2, Just 0, Just 2, Just 0])
  , ("Am7",    [Nothing, Just 0, Just 2, Just 0, Just 1, Just 0])
  , ("Amaj7",  [Nothing, Just 0, Just 2, Just 1, Just 2, Just 0])
  , ("Asus2",  [Nothing, Just 0, Just 2, Just 2, Just 0, Just 0])
  , ("Asus4",  [Nothing, Just 0, Just 2, Just 2, Just 3, Just 0])
  , ("B",      [Nothing, Just 2, Just 4, Just 4, Just 4, Just 2])
  , ("Bm",     [Nothing, Just 2, Just 4, Just 4, Just 3, Just 2])
  , ("B7",     [Nothing, Just 2, Just 1, Just 2, Just 0, Just 2])
  , ("Bm7",    [Nothing, Just 2, Just 4, Just 2, Just 3, Just 2])
  , ("Bb",     [Nothing, Just 1, Just 3, Just 3, Just 3, Just 1])
  , ("Bbm",    [Nothing, Just 1, Just 3, Just 3, Just 2, Just 1])
  ]

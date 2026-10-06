module SvgRenderer (renderChordSvg) where

import AST

-- Compact, elegant chord diagram with elongated frets (5 frets) matching standard guitar charts.
-- Dimensions: 125 x 175.
renderChordSvg :: String -> ChordDiagram -> String
renderChordSvg name diag =
  let width = 125 :: Int
      height = 175 :: Int
      topMargin = 30 :: Int
      leftMargin = 22 :: Int
      stringSpacing = 16 :: Int
      fretSpacing = 22 :: Int
      numStrings = 6 :: Int
      numFrets = 5 :: Int
      gridWidth = (numStrings - 1) * stringSpacing
      gridHeight = numFrets * fretSpacing
      baseFret = diagBaseFret diag

      nutOrFretHeader =
        if baseFret == 1
        then "<rect class=\"nut-line\" x=\"" ++ show leftMargin ++ "\" y=\"" ++ show topMargin
             ++ "\" width=\"" ++ show gridWidth ++ "\" height=\"3.5\" rx=\"1.5\" />"
        else "<text class=\"diag-fret\" x=\"" ++ show (leftMargin - 13) ++ "\" y=\"" ++ show (topMargin + 15)
             ++ "\" font-size=\"10.5\" text-anchor=\"middle\" font-family=\"var(--mono)\">" ++ show baseFret ++ "fr</text>"

      stringLines = concat
        [ "<line class=\"grid-line\" x1=\"" ++ show x ++ "\" y1=\"" ++ show topMargin
          ++ "\" x2=\"" ++ show x ++ "\" y2=\"" ++ show (topMargin + gridHeight) ++ "\" stroke-width=\"1\" />"
        | s <- [0 .. numStrings - 1]
        , let x = leftMargin + s * stringSpacing
        ]

      fretLines = concat
        [ "<line class=\"grid-line\" x1=\"" ++ show leftMargin ++ "\" y1=\"" ++ show y
          ++ "\" x2=\"" ++ show (leftMargin + gridWidth) ++ "\" y2=\"" ++ show y ++ "\" stroke-width=\"1\" />"
        | f <- [1 .. numFrets]
        , let y = topMargin + f * fretSpacing
        ]

      -- Mute (x) and open (o) markers above the nut
      topMarkers = concat
        [ let x = leftMargin + (s - 1) * stringSpacing
              y = topMargin - 8
          in case mFret of
               Nothing -> "<text class=\"diag-mute\" x=\"" ++ show x ++ "\" y=\"" ++ show y
                          ++ "\" font-size=\"11\" text-anchor=\"middle\" font-family=\"var(--mono)\">&#215;</text>"
               Just 0  -> "<circle class=\"diag-open\" cx=\"" ++ show x ++ "\" cy=\"" ++ show (y - 4)
                          ++ "\" r=\"3.5\" stroke-width=\"1.5\" fill=\"none\" />"
               _       -> ""
        | (s, mFret) <- zip [1..6] (diagFrets diag)
        ]

      -- Barre bar
      barreSvg = concat
        [ let relFret = bFret - baseFret + 1
              y = topMargin + (relFret - 1) * fretSpacing + (fretSpacing `div` 2)
              x1 = leftMargin + (sMin - 1) * stringSpacing
              x2 = leftMargin + (sMax - 1) * stringSpacing
          in "<rect class=\"diag-dot\" x=\"" ++ show x1 ++ "\" y=\"" ++ show (y - 6)
             ++ "\" width=\"" ++ show (x2 - x1) ++ "\" height=\"12\" rx=\"6\" />"
        | (bFret, sMin, sMax) <- diagBarres diag
        , bFret >= baseFret && bFret < baseFret + numFrets
        ]

      -- Fretted finger dots
      frettedDots = concat
        [ let relFret = fret - baseFret + 1
              cx = leftMargin + (s - 1) * stringSpacing
              cy = topMargin + (relFret - 1) * fretSpacing + (fretSpacing `div` 2)
              fingerText = case mFinger of
                             Just f  -> "<text class=\"diag-finger\" x=\"" ++ show cx ++ "\" y=\"" ++ show (cy + 3)
                                        ++ "\" font-size=\"9\" text-anchor=\"middle\" font-family=\"var(--mono)\">" ++ show f ++ "</text>"
                             Nothing -> ""
          in if relFret >= 1 && relFret <= numFrets
             then "<circle class=\"diag-dot\" cx=\"" ++ show cx ++ "\" cy=\"" ++ show cy ++ "\" r=\"6\" />" ++ fingerText
             else ""
        | (s, (mFret, mFinger)) <- zip [1..6] (zip (diagFrets diag) (diagFingers diag))
        , Just fret <- [mFret]
        , fret > 0
        ]

  in concat
    [ "<svg class=\"chord-diagram-svg\" viewBox=\"0 0 " ++ show width ++ " " ++ show height
      ++ "\" width=\"" ++ show width ++ "\" height=\"" ++ show height ++ "\">"
    , "<text class=\"diag-name\" x=\"" ++ show (width `div` 2) ++ "\" y=\"17\" font-size=\"13.5\""
      ++ " text-anchor=\"middle\" font-family=\"var(--sans)\">" ++ name ++ "</text>"
    , stringLines
    , fretLines
    , nutOrFretHeader
    , topMarkers
    , barreSvg
    , frettedDots
    , "</svg>"
    ]

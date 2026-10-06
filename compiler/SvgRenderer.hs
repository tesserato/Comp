module SvgRenderer (renderChordSvg) where

import AST

-- Compact, elegant chord diagram with elongated 5 frets and no label collisions.
-- Dimensions: 110 x 152.
renderChordSvg :: String -> ChordDiagram -> String
renderChordSvg name diag =
  let width = 110 :: Int
      height = 152 :: Int
      topMargin = 32 :: Int
      leftMargin = 20 :: Int
      stringSpacing = 14 :: Int
      fretSpacing = 19 :: Int
      numStrings = 6 :: Int
      numFrets = 5 :: Int
      gridWidth = (numStrings - 1) * stringSpacing
      gridHeight = numFrets * fretSpacing
      baseFret = diagBaseFret diag

      nutOrFretHeader =
        if baseFret == 1
        then "<rect class=\"nut-line\" x=\"" ++ show leftMargin ++ "\" y=\"" ++ show topMargin
             ++ "\" width=\"" ++ show gridWidth ++ "\" height=\"3\" rx=\"1.5\" />"
        else "<text class=\"diag-fret\" x=\"" ++ show (leftMargin - 11) ++ "\" y=\"" ++ show (topMargin + 13)
             ++ "\" font-size=\"9.5\" text-anchor=\"middle\" font-family=\"var(--mono)\">" ++ show baseFret ++ "fr</text>"

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

      -- Mute (x) and open (o) markers above the nut (y=22 for circle, y=25 for x)
      topMarkers = concat
        [ let x = leftMargin + (s - 1) * stringSpacing
          in case mFret of
               Nothing -> "<text class=\"diag-mute\" x=\"" ++ show x ++ "\" y=\"25\""
                          ++ " font-size=\"10\" text-anchor=\"middle\" font-family=\"var(--mono)\">&#215;</text>"
               Just 0  -> "<circle class=\"diag-open\" cx=\"" ++ show x ++ "\" cy=\"22\""
                          ++ " r=\"3\" stroke-width=\"1.2\" fill=\"none\" />"
               _       -> ""
        | (s, mFret) <- zip [1..6] (diagFrets diag)
        ]

      -- Barre bar
      barreSvg = concat
        [ let relFret = bFret - baseFret + 1
              y = topMargin + (relFret - 1) * fretSpacing + (fretSpacing `div` 2)
              x1 = leftMargin + (sMin - 1) * stringSpacing
              x2 = leftMargin + (sMax - 1) * stringSpacing
          in "<rect class=\"diag-dot\" x=\"" ++ show x1 ++ "\" y=\"" ++ show (y - 5)
             ++ "\" width=\"" ++ show (x2 - x1) ++ "\" height=\"11\" rx=\"5.5\" />"
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
                                        ++ "\" font-size=\"8\" text-anchor=\"middle\" font-family=\"var(--mono)\">" ++ show f ++ "</text>"
                             Nothing -> ""
          in if relFret >= 1 && relFret <= numFrets
             then "<circle class=\"diag-dot\" cx=\"" ++ show cx ++ "\" cy=\"" ++ show cy ++ "\" r=\"5.2\" />" ++ fingerText
             else ""
        | (s, (mFret, mFinger)) <- zip [1..6] (zip (diagFrets diag) (diagFingers diag))
        , Just fret <- [mFret]
        , fret > 0
        ]

  in concat
    [ "<svg class=\"chord-diagram-svg\" viewBox=\"0 0 " ++ show width ++ " " ++ show height
      ++ "\" width=\"" ++ show width ++ "\" height=\"" ++ show height ++ "\">"
    , "<text class=\"diag-name\" x=\"" ++ show (width `div` 2) ++ "\" y=\"13\" font-size=\"12\""
      ++ " text-anchor=\"middle\" font-family=\"var(--sans)\">" ++ name ++ "</text>"
    , stringLines
    , fretLines
    , nutOrFretHeader
    , topMarkers
    , barreSvg
    , frettedDots
    , "</svg>"
    ]

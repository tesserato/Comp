module SvgRenderer (renderChordSvg) where

import AST
import Data.Maybe (isJust)

-- Render chord SVG diagram (CifraClub style)
-- Dimensions: 120 x 145 px
renderChordSvg :: String -> ChordDiagram -> String
renderChordSvg name diag =
  let width = 120 :: Int
      height = 145 :: Int
      topMargin = 32 :: Int
      leftMargin = 22 :: Int
      stringSpacing = 15 :: Int
      fretSpacing = 18 :: Int
      numStrings = 6 :: Int
      numFrets = 4 :: Int
      gridWidth = (numStrings - 1) * stringSpacing
      gridHeight = numFrets * fretSpacing
      baseFret = diagBaseFret diag

      nutOrFretHeader =
        if baseFret == 1
        then "<rect x=\"" ++ show leftMargin ++ "\" y=\"" ++ show topMargin ++ "\" width=\"" ++ show gridWidth ++ "\" height=\"3\" fill=\"#f3f4f6\" rx=\"1\" />"
        else "<text x=\"" ++ show (leftMargin - 14) ++ "\" y=\"" ++ show (topMargin + 13) ++ "\" font-size=\"10\" font-weight=\"bold\" fill=\"#9ca3af\" text-anchor=\"middle\" font-family=\"monospace\">" ++ show baseFret ++ "fr</text>"

      -- Vertical string lines
      stringLines = concat
        [ "<line x1=\"" ++ show x ++ "\" y1=\"" ++ show topMargin ++ "\" x2=\"" ++ show x ++ "\" y2=\"" ++ show (topMargin + gridHeight) ++ "\" stroke=\"#4b5563\" stroke-width=\"1\" />"
        | s <- [0 .. numStrings - 1]
        , let x = leftMargin + s * stringSpacing
        ]

      -- Horizontal fret lines
      fretLines = concat
        [ "<line x1=\"" ++ show leftMargin ++ "\" y1=\"" ++ show y ++ "\" x2=\"" ++ show (leftMargin + gridWidth) ++ "\" y2=\"" ++ show y ++ "\" stroke=\"#4b5563\" stroke-width=\"1\" />"
        | f <- [1 .. numFrets]
        , let y = topMargin + f * fretSpacing
        ]

      -- Top markers: 'x' (mute) or 'o' (open)
      topMarkers = concat
        [ let x = leftMargin + (s - 1) * stringSpacing
              y = topMargin - 8
          in case mFret of
               Nothing -> "<text x=\"" ++ show x ++ "\" y=\"" ++ show y ++ "\" font-size=\"11\" font-weight=\"bold\" fill=\"#ef4444\" text-anchor=\"middle\" font-family=\"sans-serif\">&#215;</text>"
               Just 0  -> "<circle cx=\"" ++ show x ++ "\" cy=\"" ++ show (y - 3) ++ "\" r=\"3.5\" stroke=\"#9ca3af\" stroke-width=\"1.5\" fill=\"none\" />"
               _       -> ""
        | (s, mFret) <- zip [1..6] (diagFrets diag)
        ]

      -- Barre bar (if any)
      barreSvg = concat
        [ let relFret = bFret - baseFret + 1
              y = topMargin + (relFret - 1) * fretSpacing + (fretSpacing `div` 2)
              x1 = leftMargin + (sMin - 1) * stringSpacing
              x2 = leftMargin + (sMax - 1) * stringSpacing
          in "<rect x=\"" ++ show x1 ++ "\" y=\"" ++ show (y - 5) ++ "\" width=\"" ++ show (x2 - x1) ++ "\" height=\"10\" rx=\"5\" fill=\"#38bdf8\" />"
        | (bFret, sMin, sMax) <- diagBarres diag
        , bFret >= baseFret && bFret < baseFret + numFrets
        ]

      -- Fretted finger dots
      frettedDots = concat
        [ let relFret = fret - baseFret + 1
              cx = leftMargin + (s - 1) * stringSpacing
              cy = topMargin + (relFret - 1) * fretSpacing + (fretSpacing `div` 2)
              fingerText = case mFinger of
                             Just f  -> "<text x=\"" ++ show cx ++ "\" y=\"" ++ show (cy + 3) ++ "\" font-size=\"8.5\" font-weight=\"bold\" fill=\"#0b0f19\" text-anchor=\"middle\" font-family=\"sans-serif\">" ++ show f ++ "</text>"
                             Nothing -> ""
          in if relFret >= 1 && relFret <= numFrets
             then "<circle cx=\"" ++ show cx ++ "\" cy=\"" ++ show cy ++ "\" r=\"5.5\" fill=\"#38bdf8\" />" ++ fingerText
             else ""
        | (s, (mFret, mFinger)) <- zip [1..6] (zip (diagFrets diag) (diagFingers diag))
        , Just fret <- [mFret]
        , fret > 0
        ]

  in concat
    [ "<svg class=\"chord-diagram-svg\" viewBox=\"0 0 " ++ show width ++ " " ++ show height ++ "\" width=\"" ++ show width ++ "\" height=\"" ++ show height ++ "\" xmlns=\"http://www.w3.org/2000/svg\">"
    , "<text x=\"" ++ show (width `div` 2) ++ "\" y=\"18\" font-size=\"14\" font-weight=\"bold\" fill=\"#f3f4f6\" text-anchor=\"middle\" font-family=\"system-ui, sans-serif\">" ++ name ++ "</text>"
    , stringLines
    , fretLines
    , nutOrFretHeader
    , topMarkers
    , barreSvg
    , frettedDots
    , "</svg>"
    ]

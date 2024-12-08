module Common exposing (..)

import Dict exposing (..)
import Binary exposing (..)

import CommonModel exposing (..)



{- 
Stave Locations are defined per piano treble clef and given an associated numeric position:
G5  10      (hi-hat)
F5  9   ----(ride cymbal) -------------
E5  8       (high tom)
D5  7   ----(mid tom) -----------------
C5  6       (snare)
B4  5   -------------------------------
A4  4       (floor drum)
G4  3   -------------------------------
F4  2       (bass drum)
E4  1   -------------------------------
D4  0       (hi-hat foot)
-}

instrumentDict : Dict String Instrument
instrumentDict = Dict.fromList 
    [("Crash Cymbal", Instrument "A5" 9.0 CrossLedger False True False 0 Up)
    , ("Hi-Hat", Instrument "G5" 10.0 Cross False True False 10 Up)
    , ("Ride Cymbal", Instrument "F5" 11 CrossLedger False True False 20 Up)
    , ("High Tom", Instrument "E5" 12.0 Ovoid False True False 30 Up)
    , ("Mid Tom", Instrument "D5" 13 Ovoid False True False 40 Up)
    , ("Snare", Instrument "C5" 14.0 Ovoid True True True 50 Up)
    , ("Floor Tom", Instrument "A4" 16.0 Ovoid False True False 60 DownOrUp)
    , ("Bass Drum", Instrument "F4" 18.0 Ovoid False False False 70 DownOrUp)      
    , ("Hi-hat Foot", Instrument "D4" 20 Cross False False False 80 DownOrUp)
    , ("Rest", Instrument "" 15.5 Rest False False False 0 NoStalk)
    ]

subdivisions : List Subdivision
subdivisions =
  [Subdivision "4-16" "Four 16ths" 4
  , Subdivision "3-8" "Three 8ths" 3]
     


{-
  A and P blocks are needed in the initial setup
-}
aBlock : Block
aBlock = Block "A" "A.png" (Binary.fromIntegers [1,0,0,0]) "4-16" "\u{23FA} \u{2500} \u{2500} \u{2500}"

pBlock : Block
pBlock = Block "P" "P.png" (Binary.fromIntegers [0,0,0,0]) "4-16" "\u{2500} \u{2500} \u{2500} \u{2500}"

blockDict : Dict String Block
blockDict = Dict.fromList 
              [ ("A", aBlock)
              , ("B", Block "B" "B.png" (Binary.fromIntegers [0,1,0,0]) "4-16" "\u{2500} \u{23FA} \u{2500} \u{2500}")
              , ("C", Block "C" "C.png" (Binary.fromIntegers [0,0,1,0]) "4-16" "\u{2500} \u{2500} \u{23FA} \u{2500}")
              , ("D", Block "D" "D.png" (Binary.fromIntegers [0,0,0,1]) "4-16" "\u{2500} \u{2500} \u{2500} \u{23FA}")
              , ("E", Block "E" "E.png" (Binary.fromIntegers [1,1,0,0]) "4-16" "\u{23FA} \u{23FA} \u{2500} \u{2500}")
              , ("F", Block "F" "F.png" (Binary.fromIntegers [0,1,1,0]) "4-16" "\u{2500} \u{23FA} \u{23FA} \u{2500}")
              , ("G", Block "G" "G.png" (Binary.fromIntegers [0,0,1,1]) "4-16" "\u{2500} \u{2500} \u{23FA} \u{23FA}")
              , ("H", Block "H" "H.png" (Binary.fromIntegers [1,0,0,1]) "4-16" "\u{23FA} \u{2500} \u{2500} \u{23FA}")
              , ("I", Block "I" "I.png" (Binary.fromIntegers [1,0,1,0]) "4-16" "\u{23FA} \u{2500} \u{23FA} \u{2500}")
              , ("J", Block "J" "J.png" (Binary.fromIntegers [0,1,0,1]) "4-16" "\u{2500} \u{23FA} \u{2500} \u{23FA}")
              , ("K", Block "K" "K.png" (Binary.fromIntegers [1,1,1,0]) "4-16" "\u{23FA} \u{23FA} \u{23FA} \u{2500}")
              , ("L", Block "L" "L.png" (Binary.fromIntegers [0,1,1,1]) "4-16" "\u{2500} \u{23FA} \u{23FA} \u{23FA}")
              , ("M", Block "M" "M.png" (Binary.fromIntegers [1,0,1,1]) "4-16" "\u{23FA} \u{2500} \u{23FA} \u{23FA}")
              , ("N", Block "N" "N.png" (Binary.fromIntegers [1,1,0,1]) "4-16" "\u{23FA} \u{23FA} \u{2500} \u{23FA}")
              , ("O", Block "O" "O.png" (Binary.fromIntegers [1,1,1,1]) "4-16" "\u{23FA} \u{23FA} \u{23FA} \u{23FA}")
              , ("P", pBlock)
              , ("Q", Block "Q" "Q.png" (Binary.fromIntegers [1,0,0]) "3-8" "\u{23FA} \u{2500} \u{2500}")
              , ("R", Block "R" "R.png" (Binary.fromIntegers [0,1,0]) "3-8" "\u{2500} \u{23FA} \u{2500}")
              , ("S", Block "S" "S.png" (Binary.fromIntegers [0,0,1]) "3-8" "\u{2500} \u{2500} \u{23FA}")
              , ("T", Block "T" "T.png" (Binary.fromIntegers [1,1,0]) "3-8" "\u{23FA} \u{23FA} \u{2500}")
              , ("U", Block "U" "U.png" (Binary.fromIntegers [0,1,1]) "3-8" "\u{2500} \u{23FA} \u{23FA}")
              , ("V", Block "V" "V.png" (Binary.fromIntegers [1,0,1]) "3-8" "\u{23FA} \u{2500} \u{23FA}")
              , ("W", Block "W" "W.png" (Binary.fromIntegers [1,1,1]) "3-8" "\u{23FA} \u{23FA} \u{23FA}")
              , ("X", Block "X" "X.png" (Binary.fromIntegers [0,0,0]) "3-8" "\u{2500} \u{2500} \u{2500}")
              ]
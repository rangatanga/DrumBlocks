module Stave exposing (renderStaveBeat, renderStaveBars)

import Svg exposing (..)
import Svg.Attributes exposing (..)
import Binary exposing (..)
import Dict exposing (..)


import Html exposing (..)
import Html.Attributes exposing (..)

import CommonModel exposing (..)
import Common exposing (..)
import CommonEvents exposing (..)



type alias NoteSubBeat = 
  {subBeat : Int
  , instrumentName : String --this gives stave position and note shape
  , stalkDirection : StalkDirection
  , noteDuration : NoteDuration
  , isDotted : Bool
  , isRest : Bool
  , subdivision : String
  , stalkHeight : Float
  , nextSubBeat : Int
  , nextSubBeatNoteDuration : NoteDuration
  , prevSubBeat : Int
  , isGhostNote : Bool
  , isAccented : Bool
  }


staveLines : List Float
staveLines =
    [8, 10, 12, 14, 16]

staveShiftY : Float
staveShiftY = 23


stave : Float -> Int -> Int -> List (Svg Msg)
stave staveOffset barNo barOffset =
  let
    startX = String.fromInt (if barOffset == 0 then 5 else 108)
    endX = String.fromInt (if barOffset == 0 then 108 else 200)
  in
  ((staveLines)
        |> List.map
            (\n ->
                Svg.path
                    [ strokeWidth "0.3"
                    , stroke "black"
                    , d ("M " ++ startX ++ " " ++ String.fromFloat (n + 3.0 + (staveShiftY * staveOffset))
                         ++ " L " ++ endX ++ " " ++ String.fromFloat (n + 3.0 + (staveShiftY * staveOffset)))
                    ]
                    []
            )
  ) ++  if barOffset == 0 then
          [Svg.text_ [Svg.Attributes.x "2.5"
                     ,Svg.Attributes.y (String.fromFloat (15.5 + (staveShiftY * staveOffset)))
                     ,Svg.Attributes.class "stave-bar-number"
                     ] 
                     [Svg.text (String.fromInt barNo)]]
        else []

staveTimeSignature : Bar -> List (Svg Msg)
staveTimeSignature bar = 
  case bar.timeSignature of
      "4/4" ->  [Svg.image [xlinkHref "assets/images/Timesignature4-4.svg"
                        , Svg.Attributes.width "9"
                        , Svg.Attributes.height "11.5"
                        , Svg.Attributes.x "9.5"
                        , Svg.Attributes.y "9.6"] [] ]
      _ -> []
  
percussionClef : Float -> List(Svg Msg)
percussionClef staveOffset =
  [Svg.path
      [ strokeWidth "1.5"
      , stroke "black"
      , d ("M 8 " ++ String.fromFloat (12.8 + (staveShiftY * staveOffset)) ++ " L 8 " ++ String.fromFloat (17.2 + (staveShiftY * staveOffset)))
      ]
      []
  ,Svg.path
      [ strokeWidth "1.5"
      , stroke "black"
      , d ("M 10 " ++ String.fromFloat (12.8 + (staveShiftY * staveOffset)) ++ " L 10 " ++ String.fromFloat (17.2 + (staveShiftY * staveOffset)))
      ]
      []
  ]

singleBarLines : Float -> Int -> List(Svg Msg)
singleBarLines staveOffset barOffset =
  if barOffset == 0 then
    [Svg.path
        [ strokeWidth "0.2"
        , stroke "black"
        , d ("M " ++ String.fromInt 108 ++ " " ++ String.fromFloat (11 + (staveShiftY * staveOffset)) 
            ++ " L " ++ String.fromInt 108 ++ " " ++ String.fromFloat (19.0 + (staveShiftY * staveOffset))
            )
        ]
        []]
  else
    [Svg.path
        [ strokeWidth "0.2"
        , stroke "black"
        , d ("M " ++ String.fromInt 200 ++ " " ++ String.fromFloat (11 + (staveShiftY * staveOffset)) 
            ++ " L " ++ String.fromInt 200 ++ " " ++ String.fromFloat (19.0 + (staveShiftY * staveOffset))
            )
        ]
        []]
{-
Each beat in a bar is divided into 12 equal spaces because 12 is divisible by 3 and 4 meaning we can evenly space
both triplets and 16ths, e.g.

One             Trip            Let
X               X               X    
1   2   3   4   5   6   7   8   9   10    11    12
X           X           X           X
One         E           And         A

Iterate through all 12 spaces and all items in the arrangement, and draw a note if required.
-}
renderStaveBeat : Int -> Bar -> List(Svg Msg)
renderStaveBeat beat bar = 
  (stave 1 1 0) ++ (renderStaveBar 0 0 [beat] bar False)

renderStaveBars : BarDict -> List(Svg Msg)
renderStaveBars bars = 
  let
    divisions = (Dict.toList bars) |> List.map (\b -> (Tuple.second b).arrangement)
                                    |> List.concatMap (\a -> Dict.toList a) --(instrName, BeatBlockDict)
                                    |> List.map (\ib -> (Tuple.second ib))
                                    |> List.concatMap (\bb -> Dict.toList bb) --(Beat, Block)
                                    |> List.map (\bl -> if (Tuple.second bl).blockName /= "P"
                                                            && (Tuple.second bl).blockName /= "X" then
                                                          (Tuple.second bl).subdivision
                                                        else "")
                                           
    hasMixedDivisions = List.any (\x -> x == "3-8") divisions 
                        && List.any (\x -> x == "4-16") divisions 
  in
  (Dict.toList bars) |> List.concatMap (\b ->   let
                                                  barNo = (Tuple.first b)
                                                  bar = (Tuple.second b)
                                                  barOffset = modBy 2 (barNo-1)
                                                  staveOffset = toFloat ((barNo-1) // 2)
                                                in
                                                stave staveOffset barNo barOffset
                                                ++ (if barOffset == 0 then
                                                      percussionClef staveOffset
                                                    else [])
                                                ++ (singleBarLines staveOffset barOffset)
                                                ++ (if barNo == 1 then
                                                      staveTimeSignature bar
                                                     else [])
                                                ++ (renderStaveBar barOffset staveOffset (List.range 1 4) bar hasMixedDivisions)
                                        )

--(stave ++ percussionClef ++ (staveTimeSignature (Tuple.second bar)) ++ (singleBarLine 8) ++ 

renderStaveBar : Int -> Float -> List Int -> Bar -> Bool -> List(Svg Msg)
renderStaveBar barOffset staveOffset beats bar hasMixedDivisions = 
  --loop through each beat of the bar (this can be limited to a single beat for the Beat Options dialog)
  (beats) |> List.concatMap (\beat -> buildNoteSubBeats barOffset staveOffset beat (List.length beats) bar.arrangement (Dict.get beat bar.beatOptions) hasMixedDivisions)
              --|> Debug.toString


buildNoteSubBeats : Int -> Float -> Int -> Int -> InstrumentBlocksDict -> Maybe BeatOptions -> Bool -> List(Svg Msg)
buildNoteSubBeats barOffset staveOffset beat beatsCount instrumentBlocks beatOptions hasMixedDivisions = 
  let
      subBeats = [1, 4, 5, 7, 9, 10]
      --get alll instruments & blocks for the current beat
      beatBlocks = (Dict.toList instrumentBlocks) |> List.map (\ib -> Tuple.pair (Tuple.first ib) (Dict.get beat (Tuple.second ib)))
      noteSubBeats = (subBeats) |> List.concatMap  (\sb ->  getNoteSubBeats sb beatBlocks beatOptions)
                                |> updateTripletNoteSubBeats hasMixedDivisions
                                |> updateNoteSubBeats hasMixedDivisions
 
  in
  (noteSubBeats) |> List.concatMap (\nsb -> renderNote barOffset staveOffset beat beatsCount nsb noteSubBeats hasMixedDivisions)
  --(Debug.toString noteSubBeats) ++ " BEAT " ++ String.fromInt beat


getNoteSubBeats : Int -> List (String, Maybe Block) -> Maybe BeatOptions -> List NoteSubBeat
getNoteSubBeats subBeat beatBlocks beatOptions = 
  (beatBlocks) |> List.concatMap (\bb -> let
                                            instrumentName = (Tuple.first bb)
                                            stalkDirection = case Dict.get instrumentName instrumentDict of
                                                                Just i -> i.stalkDirection
                                                                _ -> Up
                                            isPlayed = case Tuple.second bb of
                                                          Just block -> isSubBeatMatch subBeat block
                                                          _ -> False
                                            subdivision = case Tuple.second bb of
                                                            Just block -> block.subdivision
                                                            _ -> "4-16"
                                            adjSubBeat =  if subdivision == "4-16" then --needed for the accent/ghost note/etc. bitmaps
                                                            (subBeat + 2) // 3
                                                          else
                                                            (subBeat + 3) // 4

                                            isGhostNote = case beatOptions of
                                                            Just beatOpts -> if subdivision == "4-16" then 
                                                                                  Binary.toDecimal  (Binary.and beatOpts.ghostNotes
                                                                                                                (Binary.fromDecimal (2 ^ (4-adjSubBeat)))
                                                                                                    ) /= 0
                                                                                  && (List.member subBeat [1, 4, 7, 10])
                                                                                else 
                                                                                  Binary.toDecimal  (Binary.and beatOpts.ghostNotes
                                                                                                                (Binary.fromDecimal (2 ^ (3-adjSubBeat)))
                                                                                                    ) /= 0
                                                                                  && (List.member subBeat [1, 5, 9])
                                                            _ -> False
                                            isAccented = case beatOptions of
                                                            Just beatOpts -> if subdivision == "4-16" then 
                                                                                  Binary.toDecimal  (Binary.and beatOpts.accents
                                                                                                                (Binary.fromDecimal (2 ^ (4-adjSubBeat)))
                                                                                                    ) /= 0
                                                                                else 
                                                                                  Binary.toDecimal  (Binary.and beatOpts.accents
                                                                                                                (Binary.fromDecimal (2 ^ (3-adjSubBeat)))
                                                                                                    ) /= 0
                                                            _ -> False
                                        in
                                  if isPlayed == True then 
                                    [NoteSubBeat subBeat instrumentName stalkDirection Crotchet False False subdivision 0 subBeat Crotchet subBeat False isAccented]
                                  else if isGhostNote && instrumentName == "Snare" then
                                    [NoteSubBeat subBeat instrumentName stalkDirection Crotchet False False subdivision 0 subBeat Crotchet subBeat isGhostNote False]
                                  else
                                    []
                    )

{-
For each note, update NoteDuration, isDotted, isRest, etc, - for each note we need to look forward (i.e. > subBeat) to the other
notes within the beat. 
-}
updateNoteSubBeats : Bool -> List NoteSubBeat -> List NoteSubBeat
updateNoteSubBeats hasMixedDivisions noteSubBeats = 
  let
    updateStalks = updateStalkHeight noteSubBeats
    firstSubBeatUp = case List.head (case List.minimum ((List.filter (\x -> x.stalkDirection == Up) updateStalks) |> List.map (\z -> z.subBeat)) of
                                      Just sb -> List.filter (\y -> y.subBeat == sb && y.stalkDirection == Up) updateStalks
                                      _ -> []
                                  ) of
                      Just nsb -> nsb.subBeat
                      _ -> 99
    firstSubBeatDownOrUp = case List.head (case List.minimum ((List.filter (\x -> x.stalkDirection == DownOrUp) updateStalks) |> List.map (\z -> z.subBeat)) of
                                      Just sb -> List.filter (\y -> y.subBeat == sb && y.stalkDirection == DownOrUp) updateStalks
                                      _ -> []
                                  ) of
                            Just nsb -> nsb.subBeat
                            _ -> 99
    noteSubBeatsWithRests = updateStalks
                            ++ (if hasMixedDivisions == False then
                                  if firstSubBeatUp > 1 && firstSubBeatDownOrUp > 1 then
                                    [NoteSubBeat 1 "Rest" Up Crotchet False True "4-16" 0 1 Crotchet 1 False False]
                                  else []
                                else
                                  (if firstSubBeatUp > 1 then
                                    [NoteSubBeat 1 "Rest" Up Crotchet False True "4-16" 0 firstSubBeatUp Crotchet 1 False False]
                                  else [])
                                  ++
                                  (if firstSubBeatDownOrUp > 1 then
                                    [NoteSubBeat 1 "Rest" DownOrUp Crotchet False True "4-16" 0 firstSubBeatDownOrUp Crotchet 1 False False]
                                  else [])
                               )
  in
  updateNoteDuration noteSubBeatsWithRests hasMixedDivisions
{-
  For triplet blocks, add rest notes, as necessary, to ensure we get the correct beams and triplet beams.
-}
addTripletRest : Int -> Int -> Int -> List NoteSubBeat -> Bool -> List NoteSubBeat
addTripletRest x y z noteSubBeats hasMixedDivisions =
  let
    nextSubBeat = if z == 1 then 5 else 9
    prevSubBeat = if z == 9 then 5 else 1
  in
  if hasMixedDivisions == False then
    if List.any (\a -> a.subBeat == x && a.subdivision == "3-8") noteSubBeats
      && List.any (\a -> a.subBeat == y && a.subdivision == "3-8") noteSubBeats
      && Basics.not (List.any (\a -> a.subBeat == z && a.subdivision == "3-8") noteSubBeats) then
      [NoteSubBeat z "Rest" Up Quaver False True "3-8" 0 nextSubBeat Crotchet prevSubBeat False False]
    else
      []
  else
    (if List.any (\a -> a.subBeat == x && a.subdivision == "3-8" && a.stalkDirection == Up) noteSubBeats
      && List.any (\a -> a.subBeat == y && a.subdivision == "3-8" && a.stalkDirection == Up) noteSubBeats
      && Basics.not (List.any (\a -> a.subBeat == z && a.subdivision == "3-8" && a.stalkDirection == Up) noteSubBeats) then
      [NoteSubBeat z "Rest" Up Quaver False True "3-8" 0 nextSubBeat Crotchet prevSubBeat False False]
    else
      []
    )
    ++
    (if List.any (\a -> a.subBeat == x && a.subdivision == "3-8" && a.stalkDirection == DownOrUp) noteSubBeats
      && List.any (\a -> a.subBeat == y && a.subdivision == "3-8" && a.stalkDirection == DownOrUp) noteSubBeats
      && Basics.not (List.any (\a -> a.subBeat == z && a.subdivision == "3-8" && a.stalkDirection == DownOrUp) noteSubBeats) then
      [NoteSubBeat z "Rest" DownOrUp Quaver False True "3-8" 0 nextSubBeat Crotchet prevSubBeat False False]
    else
      []
    )

updateTripletNoteSubBeats : Bool -> List NoteSubBeat -> List NoteSubBeat
updateTripletNoteSubBeats hasMixedDivisions noteSubBeats = 
  noteSubBeats
  ++ (addTripletRest 1 9 5 noteSubBeats hasMixedDivisions)
  ++ (addTripletRest 1 5 9 noteSubBeats hasMixedDivisions)
  ++ (addTripletRest 5 5 1 noteSubBeats hasMixedDivisions)
  ++ (addTripletRest 5 5 9 noteSubBeats hasMixedDivisions)
  ++ (addTripletRest 9 9 1 noteSubBeats hasMixedDivisions)
  ++ (addTripletRest 9 9 5 noteSubBeats hasMixedDivisions)
  
updateStalkHeight : List NoteSubBeat -> List NoteSubBeat
updateStalkHeight noteSubBeats =
  let
    stalkHeight = List.minimum ((noteSubBeats)  |> List.map (\nsb -> case Dict.get nsb.instrumentName instrumentDict of
                                                                      Just instrument -> instrument.stavePosition
                                                                      _ -> 99.0) )
                         
    justStalkHeight = (case stalkHeight of
                        Just sHeight -> sHeight
                        _ -> 20) - 6
  in
  (noteSubBeats) |> List.map (\nsb -> NoteSubBeat nsb.subBeat 
                                                  nsb.instrumentName 
                                                  nsb.stalkDirection 
                                                  nsb.noteDuration 
                                                  nsb.isDotted 
                                                  nsb.isRest 
                                                  nsb.subdivision 
                                                  justStalkHeight 
                                                  nsb.nextSubBeat 
                                                  nsb.nextSubBeatNoteDuration
                                                  nsb.prevSubBeat
                                                  nsb.isGhostNote
                                                  nsb.isAccented)

updateNoteDuration : List NoteSubBeat -> Bool -> List NoteSubBeat
updateNoteDuration noteSubBeats hasMixedDivisions = 
  let
    updNoteSubBeats = (noteSubBeats) |> List.map (\nsb -> Tuple.pair nsb (getNoteDuration nsb noteSubBeats hasMixedDivisions))
                                     |> List.map (\x -> NoteSubBeat (Tuple.first x).subBeat 
                                                                    (Tuple.first x).instrumentName 
                                                                    (Tuple.first x).stalkDirection 
                                                                    (Tuple.second x).noteDuration
                                                                    (Tuple.second x).isDotted
                                                                    (Tuple.first x).isRest 
                                                                    (Tuple.first x).subdivision 
                                                                    (Tuple.first x).stalkHeight
                                                                    (Tuple.second x).nextSubBeat
                                                                    (Tuple.first x).nextSubBeatNoteDuration
                                                                    (Tuple.second x).prevSubBeat
                                                                    (Tuple.first x).isGhostNote 
                                                                    (Tuple.first x).isAccented)
  in
  (updNoteSubBeats) |> List.map (\nsb ->  let
                                            nextNoteSubBeats = List.filter (\x -> x.subBeat == nsb.nextSubBeat
                                                                                  && (hasMixedDivisions == False || x.stalkDirection == nsb.stalkDirection)) updNoteSubBeats
                                            maxNoteDuration = List.foldl  (\n i -> if i == Crotchet || n.noteDuration == Crotchet then Crotchet
                                                                                   else if i == Quaver || n.noteDuration == Quaver then Quaver
                                                                                   else i
                                                                          ) SemiQuaver nextNoteSubBeats    
                                          in
                                          NoteSubBeat nsb.subBeat 
                                                      nsb.instrumentName 
                                                      nsb.stalkDirection 
                                                      nsb.noteDuration
                                                      nsb.isDotted
                                                      nsb.isRest 
                                                      nsb.subdivision 
                                                      nsb.stalkHeight
                                                      nsb.nextSubBeat
                                                      maxNoteDuration
                                                      nsb.prevSubBeat
                                                      nsb.isGhostNote
                                                      nsb.isAccented)

  
getNoteDuration : NoteSubBeat -> List NoteSubBeat -> Bool -> NoteDurationParam
getNoteDuration currNoteSubBeat allNoteSubBeats hasMixedDivisions =
  let
    nextSubBeat = List.filter (\nsb -> nsb.subBeat > currNoteSubBeat.subBeat
                                      && nsb.subdivision == currNoteSubBeat.subdivision
                                      && (hasMixedDivisions == False || nsb.stalkDirection == currNoteSubBeat.stalkDirection)) allNoteSubBeats
                   |> List.map (\nsb -> nsb.subBeat)
                   |> List.minimum
    prevSubBeat = case List.filter (\nsb -> nsb.subBeat < currNoteSubBeat.subBeat 
                                            && nsb.isRest == False
                                            && nsb.subdivision == currNoteSubBeat.subdivision
                                            && (hasMixedDivisions == False || nsb.stalkDirection == currNoteSubBeat.stalkDirection)) allNoteSubBeats
                        |> List.map (\nsb -> nsb.subBeat)
                        |> List.maximum of
                    Just pSubBeat -> pSubBeat
                    _ -> currNoteSubBeat.subBeat
    prevSubBeatInclRest = case List.filter (\nsb -> nsb.subBeat < currNoteSubBeat.subBeat
                                                    && (hasMixedDivisions == False || nsb.stalkDirection == currNoteSubBeat.stalkDirection)) allNoteSubBeats
                                |> List.map (\nsb -> nsb.subBeat)
                                |> List.maximum of
                            Just pSubBeat -> pSubBeat
                            _ -> currNoteSubBeat.subBeat
  in
  case nextSubBeat of
      Just nxtSubBeat -> if nxtSubBeat - currNoteSubBeat.subBeat == 3 then NoteDurationParam SemiQuaver False nxtSubBeat prevSubBeat
                         else if nxtSubBeat - currNoteSubBeat.subBeat == 6 then NoteDurationParam Quaver False nxtSubBeat prevSubBeat
                         else if nxtSubBeat - currNoteSubBeat.subBeat == 9 then NoteDurationParam Quaver True nxtSubBeat prevSubBeat
                         else if currNoteSubBeat.subdivision == "3-8" then NoteDurationParam Quaver False nxtSubBeat prevSubBeatInclRest
                         else NoteDurationParam Crotchet False nxtSubBeat prevSubBeat
      _ -> if currNoteSubBeat.subBeat == 1 then NoteDurationParam Crotchet False currNoteSubBeat.nextSubBeat prevSubBeat
           else if currNoteSubBeat.subBeat == 4 then NoteDurationParam Quaver True currNoteSubBeat.nextSubBeat prevSubBeat
           else if currNoteSubBeat.subBeat == 7 then NoteDurationParam Quaver False currNoteSubBeat.nextSubBeat prevSubBeat
           else if currNoteSubBeat.subdivision == "3-8" then NoteDurationParam Quaver False currNoteSubBeat.nextSubBeat prevSubBeat
           else NoteDurationParam SemiQuaver False currNoteSubBeat.nextSubBeat prevSubBeat

renderNote : Int -> Float -> Int -> Int -> NoteSubBeat -> List NoteSubBeat -> Bool -> List (Svg Msg)
renderNote barOffset staveOffset beat beatsCount noteSubBeat allNoteSubBeats hasMixedDivisions = 
  let
    instrument = Dict.get noteSubBeat.instrumentName instrumentDict
    noteCenterX = (toFloat (barOffset * 92)) + 22.0 + ((toFloat (((beat - 1) * (3 * beatsCount)) + (noteSubBeat.subBeat - 1))) * 1.8)
    nextNoteCenterX = (toFloat (barOffset * 92)) + 22.0 + ((toFloat (((beat - 1) * (3 * beatsCount)) + (noteSubBeat.nextSubBeat - 1))) * 1.8)
    prevNoteCenterX = (toFloat (barOffset * 92)) + 22.0 + ((toFloat (((beat - 1) * (3 * beatsCount)) + (noteSubBeat.prevSubBeat - 1))) * 1.8)
    crossNoteOffset = 1.0
    noteCenterY = case instrument of
                    Just instr -> instr.stavePosition + (staveShiftY * staveOffset)
                    _ -> 0
    noteShape = case instrument of
                    Just instr -> instr.noteShape
                    _ -> Ovoid 
    stalkDirection =  if hasMixedDivisions && noteSubBeat.stalkDirection == DownOrUp then Down 
                      else if noteSubBeat.stalkDirection == DownOrUp then Up
                      else noteSubBeat.stalkDirection
    stalkY = if stalkDirection == Up then noteSubBeat.stalkHeight else 24
    nextSubBeatIsRest = List.filter (\sb -> sb.subBeat == (noteSubBeat.subBeat + 4)
                                            && sb.subdivision == "3-8"
                                            && (hasMixedDivisions == False || sb.stalkDirection == noteSubBeat.stalkDirection)) allNoteSubBeats
                        |> List.all (\x -> x.isRest) 
    prevSubBeatIsRest = List.filter (\sb -> sb.subBeat == (noteSubBeat.subBeat - 4)
                                            && sb.subdivision == "3-8"
                                            && (hasMixedDivisions == False || sb.stalkDirection == noteSubBeat.stalkDirection)) allNoteSubBeats
                        |> List.all (\x -> x.isRest) 
    stalk = if noteShape == Rest then []
            else
              if stalkDirection == Up then
                (if noteShape == Cross || noteShape == CrossLedger then
                  [Svg.path
                    [ strokeWidth "0.3"
                    , stroke "black"
                    , d ("M " ++ (String.fromFloat (noteCenterX + 1.2)) ++ " " ++ (String.fromFloat (stalkY + (staveShiftY * staveOffset)))
                        ++ " L " ++ (String.fromFloat (noteCenterX + 1.2)) ++ " " ++ String.fromFloat (noteCenterY + 1.2 ))
                    ]
                    []]
                else
                  [Svg.path
                    [ strokeWidth "0.3"
                    , stroke "black"
                    , d ("M " ++ (String.fromFloat (noteCenterX + 1.2)) ++ " " ++ (String.fromFloat (stalkY + (staveShiftY * staveOffset)))
                    ++ " L " ++ (String.fromFloat (noteCenterX + 1.2)) ++ " " ++ (String.fromFloat (noteCenterY )))
                    ]
                    []]
                )
              else --stalkDirection == Down
                (if noteShape == Cross || noteShape == CrossLedger then
                  [Svg.path
                    [ strokeWidth "0.3"
                    , stroke "black"
                    , d ("M " ++ (String.fromFloat (noteCenterX - 1.0)) ++ " " ++ (String.fromFloat (stalkY + (staveShiftY * staveOffset)))
                        ++ " L " ++ (String.fromFloat (noteCenterX - 1.0)) ++ " " ++ String.fromFloat (noteCenterY - 1.2 ))
                    ]
                    []]
                else
                  [Svg.path
                    [ strokeWidth "0.3"
                    , stroke "black"
                    , d ("M " ++ (String.fromFloat (noteCenterX - 1.0)) ++ " " ++ (String.fromFloat (stalkY + (staveShiftY * staveOffset)))
                    ++ " L " ++ (String.fromFloat (noteCenterX - 1.0)) ++ " " ++ (String.fromFloat (noteCenterY )))
                    ]
                    []]
                )

    dot = if noteSubBeat.isDotted then
            if noteSubBeat.isRest then
              if hasMixedDivisions == False then
                [Svg.circle 
                  [cx (String.fromFloat (noteCenterX + (if noteSubBeat.isGhostNote then 2.9 else 2.4)))
                  , cy (String.fromFloat noteCenterY)
                  , r "0.4"
                  ] []]
              else
                if noteSubBeat.stalkDirection == Up then
                  [Svg.circle 
                    [cx (String.fromFloat (noteCenterX + (if noteSubBeat.isGhostNote then 2.9 else 2.4)))
                    , cy (String.fromFloat (noteCenterY - 2))
                    , r "0.3"
                    ] []]
                else
                  [Svg.circle 
                    [cx (String.fromFloat (noteCenterX + (if noteSubBeat.isGhostNote then 2.9 else 2.4)))
                    , cy (String.fromFloat (noteCenterY + 3))
                    , r "0.3"
                    ] []]
            else
              [Svg.circle 
                [cx (String.fromFloat (noteCenterX + (if noteSubBeat.isGhostNote then 2.9 else 2.4)))
                , cy (String.fromFloat noteCenterY)
                , r "0.4"
                ] []]
          else []  

    tripletBeam = if noteSubBeat.subBeat == 5 then --middle note of triplet 
                    if noteSubBeat.isRest && nextSubBeatIsRest then []
                    else
                      let
                        yAdjuster = if stalkDirection == Up then 1 else -1
                      in
                      (Svg.text_ [Svg.Attributes.x (String.fromFloat (noteCenterX ))
                                  ,Svg.Attributes.y (String.fromFloat (stalkY - (if stalkDirection == Up then 1.3 else -2.5) + (staveShiftY * staveOffset)))
                                  ,Svg.Attributes.class "stave-bar-number"
                                  ] 
                                  [Svg.text "3"]
                      )
                      ::
                      (if prevSubBeatIsRest || nextSubBeatIsRest || noteSubBeat.isRest then                                
                        [Svg.path 
                            [ strokeWidth "0.2"
                            , stroke "black"
                            , d ("M " ++ (String.fromFloat (prevNoteCenterX - 1.5)) ++ " " ++ (String.fromFloat (stalkY - (1.5 * yAdjuster) + (staveShiftY * staveOffset))) 
                              ++ " L " ++ (String.fromFloat (prevNoteCenterX - 1.5)) ++ " " ++ (String.fromFloat (stalkY - (2 * yAdjuster) + (staveShiftY * staveOffset))))
                            ] []
                        ,Svg.path 
                            [ strokeWidth "0.2"
                            , stroke "black"
                            , d ("M " ++ (String.fromFloat (prevNoteCenterX - 1.5)) ++ " " ++ (String.fromFloat (stalkY - (2 * yAdjuster) + (staveShiftY * staveOffset))) 
                              ++ " L " ++ (String.fromFloat (noteCenterX - 1.0)) ++ " " ++ (String.fromFloat (stalkY - (2 * yAdjuster) + (staveShiftY * staveOffset))))
                            ] []
                        ,Svg.path 
                            [ strokeWidth "0.2"
                            , stroke "black"
                            , d ("M " ++ (String.fromFloat (noteCenterX + 2.0)) ++ " " ++ (String.fromFloat (stalkY - (2 * yAdjuster) + (staveShiftY * staveOffset))) 
                              ++ " L " ++ (String.fromFloat (nextNoteCenterX + 2.0)) ++ " " ++ (String.fromFloat (stalkY - (2 * yAdjuster) + (staveShiftY * staveOffset))))
                            ] []
                        , Svg.path 
                            [ strokeWidth "0.2"
                            , stroke "black"
                            , d ("M " ++ (String.fromFloat (nextNoteCenterX + 2.0)) ++ " " ++ (String.fromFloat (stalkY - (1.5 * yAdjuster) + (staveShiftY * staveOffset))) 
                              ++ " L " ++ (String.fromFloat (nextNoteCenterX + 2.0)) ++ " " ++ (String.fromFloat (stalkY - (2 * yAdjuster) + (staveShiftY * staveOffset))))
                            ] []
                          ]
                        else []
                      )
                  else [] 

    topBeam = let
                xAdjuster = if stalkDirection == Up then 1 else -1
              in
              if noteSubBeat.subdivision == "4-16" then
                if noteSubBeat.subBeat == noteSubBeat.nextSubBeat then []
                else if noteSubBeat.isRest then []
                else 
                    let
                      beamStartX = if stalkDirection == Up then noteCenterX + 1.05 else noteCenterX - 1.1
                      beamEndX = if stalkDirection == Up then nextNoteCenterX + 1.35 else nextNoteCenterX - 0.9
                    in
                    [Svg.path 
                        [ strokeWidth "0.6"
                        , stroke "black"
                        , d ("M " ++ (String.fromFloat beamStartX) ++ " " ++ (String.fromFloat (stalkY + (staveShiftY * staveOffset))) 
                          ++ " L " ++ (String.fromFloat beamEndX) ++ " " ++ (String.fromFloat (stalkY + (staveShiftY * staveOffset))))
                        ] []]
              else --"3-8"
                if noteSubBeat.subBeat == 5 then
                  (if prevSubBeatIsRest then []
                  else if noteSubBeat.isRest && nextSubBeatIsRest then []
                  else                
                    let
                      beamStartX = if stalkDirection == Up then noteCenterX + 1.3 else noteCenterX - 0.85
                      beamEndX = if stalkDirection == Up then prevNoteCenterX + 1.05 else prevNoteCenterX - 1.15
                    in
                    [Svg.path 
                        [ strokeWidth "0.6"
                        , stroke "black"
                        , d ("M " ++ (String.fromFloat beamStartX) ++ " " ++ (String.fromFloat (stalkY + (staveShiftY * staveOffset))) 
                          ++ " L " ++ (String.fromFloat beamEndX) ++ " " ++ (String.fromFloat (stalkY + (staveShiftY * staveOffset))))
                        ] []]
                  )
                  ++
                  (if nextSubBeatIsRest then []
                  else if noteSubBeat.isRest && prevSubBeatIsRest then []
                  else                
                    let
                      beamStartX = if stalkDirection == Up then noteCenterX + 1.1 else noteCenterX - 1.0
                      beamEndX = if stalkDirection == Up then nextNoteCenterX + 1.35 else nextNoteCenterX - 0.85
                    in
                    [Svg.path 
                        [ strokeWidth "0.6"
                        , stroke "black"
                        , d ("M " ++ (String.fromFloat beamStartX) ++ " " ++ (String.fromFloat (stalkY + (staveShiftY * staveOffset))) 
                          ++ " L " ++ (String.fromFloat beamEndX) ++ " " ++ (String.fromFloat (stalkY + (staveShiftY * staveOffset))))
                        ] []]
                  )
                else
                  []

    
    semiQuaverBeam =  let
                        yAdjuster = if stalkDirection == Up then 1 else -1
                      in
                      if noteSubBeat.subdivision == "4-16" then
                        if noteSubBeat.noteDuration == SemiQuaver 
                          && noteSubBeat.isRest == False then 
                          if noteSubBeat.subBeat == noteSubBeat.nextSubBeat then --last (or only) note in the beat
                            if noteSubBeat.subBeat == 10 then --last subBeat position 
                              if noteSubBeat.prevSubBeat /= noteSubBeat.subBeat then --short semi quaver bar goes to the left
                                let
                                  beamStartX = if stalkDirection == Up then noteCenterX + 1.05 else noteCenterX - 1.0
                                  beamEndX = if stalkDirection == Up then noteCenterX - 0.8 else noteCenterX - 2.95
                                in
                                [Svg.path 
                                  [ strokeWidth "0.6"
                                  , stroke "black"
                                  , d ("M " ++ (String.fromFloat beamStartX) ++ " " ++ (String.fromFloat (stalkY + (staveShiftY * staveOffset) + (1.4 * yAdjuster))) 
                                    ++ " L " ++ (String.fromFloat beamEndX) ++ " " ++ (String.fromFloat (stalkY + (staveShiftY * staveOffset) + (1.4 * yAdjuster))))
                                  ] []
                                ]
                              else  --single semiquaver, no beam
                                if stalkDirection == Up then
                                  [Svg.image [xlinkHref "assets/images/semiquaver.svg"
                                              , Svg.Attributes.width "2"
                                              , Svg.Attributes.height "8"
                                              , Svg.Attributes.x (String.fromFloat (noteCenterX + 1.2))
                                              , Svg.Attributes.y (String.fromFloat (stalkY + (staveShiftY * staveOffset) - 1.7))] []]
                                else
                                  [Svg.image [xlinkHref "assets/images/semiquaver_up.svg"
                                              , Svg.Attributes.width "2.5"
                                              , Svg.Attributes.height "7.2"
                                              , Svg.Attributes.x (String.fromFloat (noteCenterX - 1.0))
                                              , Svg.Attributes.y (String.fromFloat (stalkY + (staveShiftY * staveOffset) - 6.7))] []]
                            else --short semi quaver bar goes to the right
                                let
                                  beamStartX = if stalkDirection == Up then noteCenterX + 1.05 else noteCenterX - 1.0
                                  beamEndX = if stalkDirection == Up then noteCenterX + 3.1 else noteCenterX + 1.95
                                in
                                [Svg.path 
                                  [ strokeWidth "0.6"
                                  , stroke "black"
                                  , d ("M " ++ (String.fromFloat beamStartX) ++ " " ++ (String.fromFloat (stalkY + (staveShiftY * staveOffset) + (1.4 * yAdjuster))) 
                                      ++ " L " ++ (String.fromFloat beamEndX) ++ " " ++ (String.fromFloat (stalkY + (staveShiftY * staveOffset) + (1.4 * yAdjuster))))
                                  ] []
                                ]
                          else if noteSubBeat.nextSubBeatNoteDuration == SemiQuaver then --full semi quaver bar goes to next note
                                  let
                                    beamStartX = if stalkDirection == Up then noteCenterX + 1.05 else noteCenterX - 1.0
                                    beamEndX = if stalkDirection == Up then nextNoteCenterX + 1.35 else nextNoteCenterX - 1.0
                                  in
                                  [Svg.path 
                                    [ strokeWidth "0.6"
                                    , stroke "black"
                                    , d ("M " ++ (String.fromFloat beamStartX) ++ " " ++ (String.fromFloat (stalkY + (staveShiftY * staveOffset) + (1.4 * yAdjuster))) 
                                        ++ " L " ++ (String.fromFloat beamEndX) ++ " " ++ (String.fromFloat (stalkY + (staveShiftY * staveOffset) + (1.4 * yAdjuster))))
                                    ] []
                                ]
                              else --short semi quaver bar goes to the right
                                  if noteSubBeat.subBeat /= noteSubBeat.nextSubBeat
                                    && noteSubBeat.subBeat /= noteSubBeat.prevSubBeat then
                                      [] --this handles the K block issue
                                  else
                                    let
                                      beamStartX = if stalkDirection == Up then noteCenterX + 1.05 else noteCenterX - 1.0
                                      beamEndX = if stalkDirection == Up then noteCenterX + 3.1 else noteCenterX + 0.95
                                    in
                                    [Svg.path 
                                      [ strokeWidth "0.6"
                                      , stroke "black"
                                      , d ("M " ++ (String.fromFloat beamStartX) ++ " " ++ (String.fromFloat (stalkY + (staveShiftY * staveOffset) + (1.4 * yAdjuster))) 
                                          ++ " L " ++ (String.fromFloat beamEndX) ++ " " ++ (String.fromFloat (stalkY + (staveShiftY * staveOffset) + (1.4 * yAdjuster))))
                                      ] []
                                    ]
                        else if noteSubBeat.noteDuration == Quaver 
                                && Basics.not noteSubBeat.isRest
                                && noteSubBeat.subBeat == noteSubBeat.prevSubBeat 
                                && noteSubBeat.subBeat == noteSubBeat.nextSubBeat then --single quaver, no beam
                                if stalkDirection == Up then
                                  [Svg.image [xlinkHref "assets/images/quaver.svg"
                                              , Svg.Attributes.width "5"
                                              , Svg.Attributes.height "7"
                                              , Svg.Attributes.x (String.fromFloat (noteCenterX - 0.25))
                                              , Svg.Attributes.y (String.fromFloat (stalkY + (staveShiftY * staveOffset) - 0.5))
                                              ] []]
                                else
                                  [Svg.image [xlinkHref "assets/images/quaver_up.svg"
                                              , Svg.Attributes.width "3"
                                              , Svg.Attributes.height "9"
                                              , Svg.Attributes.x (String.fromFloat (noteCenterX - 1.0))
                                              , Svg.Attributes.y (String.fromFloat (stalkY + (staveShiftY * staveOffset) - 7.0))
                                              ] []]
                            else []
                      else --"3-8", decide if we need to draw the quaver stalk
                        if (noteSubBeat.subBeat == 5 && prevSubBeatIsRest && nextSubBeatIsRest)
                            || (noteSubBeat.subBeat == 9 && prevSubBeatIsRest && (List.filter (\sb -> sb.subBeat == 1 && sb.subdivision == "3-8") allNoteSubBeats
                                                                                  |> List.all (\x -> x.isRest))) then
                          if stalkDirection == Up then
                            [Svg.image [xlinkHref "assets/images/quaver.svg"
                                        , Svg.Attributes.width "5"
                                        , Svg.Attributes.height "7"
                                        , Svg.Attributes.x (String.fromFloat (noteCenterX - 0.1))
                                        , Svg.Attributes.y (String.fromFloat (stalkY + (staveShiftY * staveOffset) - 0.45))
                                        ] []]
                          else
                            [Svg.image [xlinkHref "assets/images/quaver_up.svg"
                                        , Svg.Attributes.width "3"
                                        , Svg.Attributes.height "9"
                                        , Svg.Attributes.x (String.fromFloat (noteCenterX - 1.0))
                                        , Svg.Attributes.y (String.fromFloat (stalkY + (staveShiftY * staveOffset) - 7.0))
                                        ] []]
                        else []
    ghostNote = if noteSubBeat.isGhostNote then
                    [Svg.path 
                      [ strokeWidth "0.2"
                      , stroke "black"
                      , d ("M " ++ (String.fromFloat (noteCenterX - 1.5)) ++ " " ++ (String.fromFloat (noteCenterY - 1.5)) 
                                ++ "C "++ (String.fromFloat (noteCenterX - 2.0)) ++ " " ++ (String.fromFloat (noteCenterY - 1.0)) 
                                ++ " " ++ (String.fromFloat (noteCenterX - 2.0)) ++ " " ++ (String.fromFloat (noteCenterY + 1.0)) 
                                ++ " "++ (String.fromFloat (noteCenterX - 1.5)) ++ " " ++ (String.fromFloat (noteCenterY + 1.5)) 
                                )]
                      []
                    ,Svg.path 
                      [ strokeWidth "0.2"
                      , stroke "black"
                      , d ("M " ++ (String.fromFloat (noteCenterX + 1.7)) ++ " " ++ (String.fromFloat (noteCenterY - 1.5)) 
                                ++ " C "++ (String.fromFloat (noteCenterX + 2.2)) ++ " " ++ (String.fromFloat (noteCenterY - 1.0)) 
                                ++ " " ++ (String.fromFloat (noteCenterX + 2.2)) ++ " " ++ (String.fromFloat (noteCenterY + 1.0)) 
                                ++ " "++ (String.fromFloat (noteCenterX + 1.7)) ++ " " ++ (String.fromFloat (noteCenterY + 1.5)) 
                                )]
                      []
                    ]
                  else []
    accent = if noteSubBeat.isAccented then
                    [Svg.path 
                      [ strokeWidth "0.3"
                      , stroke "black"
                      , d ("M " ++ (String.fromFloat (noteCenterX - 1.05)) ++ " " ++ (String.fromFloat (noteSubBeat.stalkHeight + (staveShiftY * staveOffset) - 2.7)) 
                                ++ " L "++ (String.fromFloat (noteCenterX + 1.05)) ++ " " ++ (String.fromFloat (noteSubBeat.stalkHeight + (staveShiftY * staveOffset) - 1.7)) 
                                )]
                      []
                    ,Svg.path 
                      [ strokeWidth "0.3"
                      , stroke "black"
                      , d ("M " ++ (String.fromFloat (noteCenterX + 1.05)) ++ " " ++ (String.fromFloat (noteSubBeat.stalkHeight + (staveShiftY * staveOffset) - 1.7)) 
                                ++ " L "++ (String.fromFloat (noteCenterX - 1.05)) ++ " " ++ (String.fromFloat (noteSubBeat.stalkHeight + (staveShiftY * staveOffset) - 0.7)) 
                                )]
                      []
                    ]
                  else []
  in
  (case noteShape of
      Ovoid ->
          [Svg.ellipse 
            [cx (String.fromFloat noteCenterX)
              , cy (String.fromFloat noteCenterY)
              , rx "1.3"
              , ry "0.95"
              , transform ("rotate(-20, " ++ (String.fromFloat noteCenterX) ++ ", " ++ (String.fromFloat noteCenterY) ++ ")")
            ] []
          ]
      Cross ->
          [Svg.path
            [ strokeWidth "0.4"
              , stroke "black"
              , d ("M " ++ (String.fromFloat (noteCenterX - crossNoteOffset)) ++ " " ++ (String.fromFloat (noteCenterY - crossNoteOffset)) 
                  ++ " L " ++ (String.fromFloat (noteCenterX + crossNoteOffset)) ++ " " ++ (String.fromFloat (noteCenterY + crossNoteOffset)) )
            ] []
          ,Svg.path
            [ strokeWidth "0.4"
              , stroke "black"
              , d ("M " ++ (String.fromFloat (noteCenterX - crossNoteOffset)) ++ " " ++ (String.fromFloat (noteCenterY + crossNoteOffset)) 
                  ++ " L " ++ (String.fromFloat (noteCenterX + crossNoteOffset)) ++ " " ++ (String.fromFloat (noteCenterY - crossNoteOffset)) )
            ] []
          ]
      CrossLedger ->
          [Svg.path
            [ strokeWidth "0.4"
              , stroke "black"
              , d ("M " ++ (String.fromFloat (noteCenterX - crossNoteOffset)) ++ " " ++ (String.fromFloat (noteCenterY - crossNoteOffset)) 
                  ++ " L " ++ (String.fromFloat (noteCenterX + crossNoteOffset)) ++ " " ++ (String.fromFloat (noteCenterY + crossNoteOffset)) )
            ] []
          ,Svg.path
            [ strokeWidth "0.4"
              , stroke "black"
              , d ("M " ++ (String.fromFloat (noteCenterX - crossNoteOffset)) ++ " " ++ (String.fromFloat (noteCenterY + crossNoteOffset)) 
                  ++ " L " ++ (String.fromFloat (noteCenterX + crossNoteOffset)) ++ " " ++ (String.fromFloat (noteCenterY - crossNoteOffset)) )
            ] []
          ,Svg.path
            [ strokeWidth "0.3"
              , stroke "black"
              , d ("M " ++ (String.fromFloat (noteCenterX - 1.4)) ++ " " ++ (String.fromFloat (noteCenterY)) 
                  ++ " L " ++ (String.fromFloat (noteCenterX + 1.4)) ++ " " ++ (String.fromFloat (noteCenterY)) )
            ] []
          ]
      Triangle ->
          [Svg.circle [cx (String.fromFloat noteCenterX), cy (String.fromFloat noteCenterY), r "1.5"] []]
      Rest ->
          case noteSubBeat.noteDuration of
              Crotchet ->
                      if hasMixedDivisions == False then
                        [Svg.image [xlinkHref "assets/images/crotchet-rest.svg"
                                    , Svg.Attributes.width "5"
                                    , Svg.Attributes.height "7"
                                    , Svg.Attributes.x (String.fromFloat (noteCenterX - 2))
                                    , Svg.Attributes.y (String.fromFloat (noteCenterY - 4))
                                    ] [] ]
                      else
                        if noteSubBeat.stalkDirection == Up then
                          [Svg.image [xlinkHref "assets/images/crotchet-rest.svg"
                                      , Svg.Attributes.width "4"
                                      , Svg.Attributes.height "6"
                                      , Svg.Attributes.x (String.fromFloat (noteCenterX - 2))
                                      , Svg.Attributes.y (String.fromFloat (noteCenterY - 6))
                                      ] [] ]
                        else
                          [Svg.image [xlinkHref "assets/images/crotchet-rest.svg"
                                      , Svg.Attributes.width "4"
                                      , Svg.Attributes.height "6"
                                      , Svg.Attributes.x (String.fromFloat (noteCenterX - 2))
                                      , Svg.Attributes.y (String.fromFloat (noteCenterY - 1))
                                      ] [] ]
              Quaver -> 
                      if hasMixedDivisions == False then
                        if noteSubBeat.subdivision == "3-8" then
                          [Svg.image [xlinkHref "assets/images/quaver-rest.svg"
                                      , Svg.Attributes.width "3"
                                      , Svg.Attributes.height "5"
                                      , Svg.Attributes.x (String.fromFloat (noteCenterX - 1))
                                      , Svg.Attributes.y (String.fromFloat (noteCenterY - 4.0))
                                      ] [] ]
                        else
                          [Svg.image [xlinkHref "assets/images/quaver-rest.svg"
                                      , Svg.Attributes.width "4"
                                      , Svg.Attributes.height "6"
                                      , Svg.Attributes.x (String.fromFloat (noteCenterX - 3))
                                      , Svg.Attributes.y (String.fromFloat (noteCenterY - 3.5))
                                      ] [] ]
                      else
                        if noteSubBeat.stalkDirection == Up then
                          [Svg.image [xlinkHref "assets/images/quaver-rest.svg"
                                      , Svg.Attributes.width "3"
                                      , Svg.Attributes.height "5"
                                      , Svg.Attributes.x (String.fromFloat (noteCenterX - 1))
                                      , Svg.Attributes.y (String.fromFloat (noteCenterY - 5.2))
                                      ] [] ]
                        else
                          [Svg.image [xlinkHref "assets/images/quaver-rest.svg"
                                      , Svg.Attributes.width "3"
                                      , Svg.Attributes.height "5"
                                      , Svg.Attributes.x (String.fromFloat (noteCenterX - 1))
                                      , Svg.Attributes.y (String.fromFloat (noteCenterY + 1))
                                      ] [] ]
              SemiQuaver ->
                      if hasMixedDivisions == False then
                        [Svg.image [xlinkHref "assets/images/16th_rest.svg"
                                    , Svg.Attributes.width "4"
                                    , Svg.Attributes.height "6"
                                    , Svg.Attributes.x (String.fromFloat (noteCenterX - 3))
                                    , Svg.Attributes.y (String.fromFloat (noteCenterY - 2.5))
                                    ] [] ]
                      else
                        if noteSubBeat.stalkDirection == Up then
                          [Svg.image [xlinkHref "assets/images/16th_rest.svg"
                                      , Svg.Attributes.width "3"
                                      , Svg.Attributes.height "3.5"
                                      , Svg.Attributes.x (String.fromFloat (noteCenterX - 1.5))
                                      , Svg.Attributes.y (String.fromFloat (noteCenterY - 4.5))
                                      ] [] ]
                        else
                          [Svg.image [xlinkHref "assets/images/16th_rest.svg"
                                      , Svg.Attributes.width "3"
                                      , Svg.Attributes.height "3.5"
                                      , Svg.Attributes.x (String.fromFloat (noteCenterX - 1.5))
                                      , Svg.Attributes.y (String.fromFloat (noteCenterY + 1.5))
                                      ] [] ]
              _ -> []
          )                                    
      ++ stalk
      ++ dot
      ++ tripletBeam
      ++ topBeam
      ++ semiQuaverBeam
      ++ ghostNote
      ++ accent



{-
isSubBeatMatch returns True if the block has a note that corresponds with the sub beat
-}  
isSubBeatMatch :  Int -> Block -> Bool
isSubBeatMatch subBeat block =
    case block.subdivision of 
                  "4-16" -> if (subBeat == 1     && Binary.toDecimal (Binary.and block.notePlacement (Binary.fromIntegers [1,0,0,0])) > 0) 
                                || (subBeat == 4  && Binary.toDecimal (Binary.and block.notePlacement (Binary.fromIntegers [0,1,0,0])) > 0) 
                                || (subBeat == 7  && Binary.toDecimal (Binary.and block.notePlacement (Binary.fromIntegers [0,0,1,0])) > 0)
                                || (subBeat == 10 && Binary.toDecimal (Binary.and block.notePlacement (Binary.fromIntegers [0,0,0,1])) > 0) then True
                            else False
                  "3-8" -> if (subBeat == 1     && Binary.toDecimal (Binary.and block.notePlacement (Binary.fromIntegers [1,0,0])) > 0) 
                                || (subBeat == 5 && Binary.toDecimal (Binary.and block.notePlacement (Binary.fromIntegers [0,1,0])) > 0) 
                                || (subBeat == 9 && Binary.toDecimal (Binary.and block.notePlacement (Binary.fromIntegers [0,0,1])) > 0) then True
                            else False
                  _ -> False


getStavePosition : String -> Float
getStavePosition  instrumentName =
  let 
    instr = Dict.get instrumentName instrumentDict
  in
  case instr of
    Just instrument -> instrument.stavePosition
    Nothing -> 0

getNoteShape : String -> NoteShape
getNoteShape  instrumentName =
  let 
    instr = Dict.get instrumentName instrumentDict
  in
  case instr of
    Just instrument -> instrument.noteShape
    Nothing -> Ovoid

  
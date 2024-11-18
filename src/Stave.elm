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
staveShiftY = 22


stave : Float -> List (Svg Msg)
stave staveOffset =
    (staveLines)
        |> List.map
            (\n ->
                Svg.path
                    [ strokeWidth "0.3"
                    , stroke "black"
                    , d ("M 0 " ++ String.fromFloat (n + 3.0 + (staveShiftY * staveOffset))
                         ++ " L 195 " ++ String.fromFloat (n + 3.0 + (staveShiftY * staveOffset)))
                    ]
                    []
            )

staveTimeSignature : Bar -> List (Svg Msg)
staveTimeSignature bar = 
  case bar.timeSignature of
      "4/4" ->  [Svg.image [xlinkHref "assets/images/Timesignature4-4.svg"
                        , Svg.Attributes.width "9"
                        , Svg.Attributes.height "11.5"
                        , Svg.Attributes.x "4.5"
                        , Svg.Attributes.y "9.6"] [] ]
      _ -> []
  
percussionClef : Float -> List(Svg Msg)
percussionClef staveOffset =
  [Svg.path
      [ strokeWidth "1.5"
      , stroke "black"
      , d ("M 3 " ++ String.fromFloat (12.8 + (staveShiftY * staveOffset)) ++ " L 3 " ++ String.fromFloat (17.2 + (staveShiftY * staveOffset)))
      ]
      []
  ,Svg.path
      [ strokeWidth "1.5"
      , stroke "black"
      , d ("M 5 " ++ String.fromFloat (12.8 + (staveShiftY * staveOffset)) ++ " L 5 " ++ String.fromFloat (17.2 + (staveShiftY * staveOffset)))
      ]
      []
  ]

singleBarLines : Float -> List(Svg Msg)
singleBarLines staveOffset =
  [Svg.path
      [ strokeWidth "0.2"
      , stroke "black"
      , d ("M " ++ String.fromInt 103 ++ " " ++ String.fromFloat (11 + (staveShiftY * staveOffset)) 
           ++ " L " ++ String.fromInt 103 ++ " " ++ String.fromFloat (19.0 + (staveShiftY * staveOffset))
          )
      ]
      [] 
  ,Svg.path
      [ strokeWidth "0.2"
      , stroke "black"
      , d ("M " ++ String.fromInt 195 ++ " " ++ String.fromFloat (11 + (staveShiftY * staveOffset)) 
           ++ " L " ++ String.fromInt 195 ++ " " ++ String.fromFloat (19.0 + (staveShiftY * staveOffset))
          )
      ]
      []   ]
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
  (stave 1) ++ (renderStaveBar 0 0 [beat] bar)

renderStaveBars : BarDict -> List(Svg Msg)
renderStaveBars bars = 
  (Dict.toList bars) |> List.concatMap (\b ->   let
                                                  barNo = (Tuple.first b)
                                                  bar = (Tuple.second b)
                                                  barOffset = modBy 2 (barNo-1)
                                                  staveOffset = toFloat ((barNo-1) // 2)
                                                in
                                                (if barOffset == 0 then
                                                  stave staveOffset
                                                  ++ percussionClef staveOffset
                                                  ++ (singleBarLines staveOffset)
                                                 else [])
                                                ++ (if barNo == 1 then
                                                      staveTimeSignature bar
                                                     else [])
                                                ++ (renderStaveBar barOffset staveOffset (List.range 1 4) bar)
                                                
                                        )

--(stave ++ percussionClef ++ (staveTimeSignature (Tuple.second bar)) ++ (singleBarLine 8) ++ 

renderStaveBar : Int -> Float -> List Int -> Bar -> List(Svg Msg)
renderStaveBar barOffset staveOffset beats bar = 
  --loop through each beat of the bar (this can be limited to a single beat for the Beat Options dialog)
  (beats) |> List.concatMap (\beat -> buildNoteSubBeats barOffset staveOffset beat (List.length beats) bar.arrangement (Dict.get beat bar.beatOptions))
              --|> Debug.toString


buildNoteSubBeats : Int -> Float -> Int -> Int -> InstrumentBlocksDict -> Maybe BeatOptions -> List(Svg Msg)
buildNoteSubBeats barOffset staveOffset beat beatsCount instrumentBlocks beatOptions = 
  let
      subBeats = [1, 4, 5, 7, 9, 10]
      --get alll instruments & blocks for the current beat
      beatBlocks = (Dict.toList instrumentBlocks) |> List.map (\ib -> Tuple.pair (Tuple.first ib) (Dict.get beat (Tuple.second ib)))
      noteSubBeats = (subBeats) |> List.concatMap  (\sb ->  getNoteSubBeats sb beatBlocks beatOptions)
                                |> updateNoteSubBeats
 
  in
  (noteSubBeats) |> List.concatMap (\nsb -> renderNote barOffset staveOffset beat beatsCount nsb)
  --(Debug.toString noteSubBeats) ++ " BEAT " ++ String.fromInt beat


getNoteSubBeats : Int -> List (String, Maybe Block) -> Maybe BeatOptions -> List NoteSubBeat
getNoteSubBeats subBeat beatBlocks beatOptions = 
  (beatBlocks) |> List.concatMap (\bb -> let
                                            instrumentName = (Tuple.first bb)
                                            isPlayed = case Tuple.second bb of
                                                          Just block -> isSubBeatMatch subBeat block
                                                          _ -> False
                                            subDivision = case Tuple.second bb of
                                                            Just block -> block.subdivision
                                                            _ -> "4-16"
                                            adjSubBeat =  if subDivision == "4-16" then
                                                            (subBeat + 2) // 3
                                                          else
                                                            (subBeat + 3) // 4

                                            isGhostNote = case beatOptions of
                                                            Just beatOpts -> if subDivision == "4-16" then 
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
                                                            Just beatOpts -> if subDivision == "4-16" then 
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
                                    [NoteSubBeat subBeat instrumentName Crotchet False False subDivision 0 subBeat Crotchet subBeat False isAccented]
                                  else if isGhostNote && instrumentName == "Snare" then
                                    [NoteSubBeat subBeat instrumentName Crotchet False False subDivision 0 subBeat Crotchet subBeat isGhostNote False]
                                  else
                                    []
                    )

{-
For each note, update NoteDuration, isDotted, isRest, etc, - for each note we need to look forward (i.e. > subBeat) to the other
notes within the beat. 
-}
updateNoteSubBeats : List NoteSubBeat -> List NoteSubBeat
updateNoteSubBeats noteSubBeats = 
  let
    updateStalks = updateStalkHeight noteSubBeats
    noteSubBeatsWithRests = List.append (if List.any (\a -> a.subBeat == 1) updateStalks then 
                                            []
                                         else
                                            [NoteSubBeat 1 "Rest" Crotchet False True "4-16" 0 1 Crotchet 1 False False]) updateStalks

  in
  updateNoteDuration noteSubBeatsWithRests

updateStalkHeight : List NoteSubBeat -> List NoteSubBeat
updateStalkHeight noteSubBeats =
  let
    stalkHeight = List.minimum ((noteSubBeats)  |> List.map (\nsb -> nsb.instrumentName)
                                                |> List.map (\i -> Dict.get i instrumentDict)
                                                |> List.map (\i -> case i of
                                                                      Just instrument -> instrument.stavePosition
                                                                      _ -> 99.0) )
                         
    justStalkHeight = (case stalkHeight of
                        Just sHeight -> sHeight
                        _ -> 20) - 7
  in
  (noteSubBeats) |> List.map (\nsb -> NoteSubBeat nsb.subBeat 
                                                  nsb.instrumentName 
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

updateNoteDuration : List NoteSubBeat -> List NoteSubBeat
updateNoteDuration noteSubBeats = 
  let
    updNoteSubBeats = (noteSubBeats) |> List.map (\nsb -> Tuple.pair nsb (getNoteDuration nsb noteSubBeats))
                                     |> List.map (\x -> NoteSubBeat (Tuple.first x).subBeat 
                                                                    (Tuple.first x).instrumentName 
                                                                    (Tuple.second x).noteDuration
                                                                    (Tuple.second x).isDotted
                                                                    (Tuple.first x).isRest 
                                                                    (Tuple.first x).subdivision 
                                                                    (Tuple.first x).stalkHeight
                                                                    (Tuple.second x).nextSubBeat
                                                                    (Tuple.first x).nextSubBeatNoteDuration
                                                                    (Tuple.second x).prevSubBeat
                                                                    (Tuple.first x).isGhostNote 
                                                                    (Tuple.first x).isAccented )
  in
  (updNoteSubBeats) |> List.map (\nsb ->  let
                                            nextNoteSubBeats = List.filter (\x -> x.subBeat == nsb.nextSubBeat) updNoteSubBeats
                                            maxNoteDuration = List.foldl  (\n i -> if i == Crotchet || n.noteDuration == Crotchet then Crotchet
                                                                                   else if i == Quaver || n.noteDuration == Quaver then Quaver
                                                                                   else i
                                                                          ) SemiQuaver nextNoteSubBeats    
                                          in
                                          NoteSubBeat nsb.subBeat 
                                                      nsb.instrumentName 
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

  
getNoteDuration : NoteSubBeat -> List NoteSubBeat -> NoteDurationParam
getNoteDuration currNoteSubBeat allNoteSubBeats =
  let
    nextSubBeat = List.filter (\nsb -> nsb.subBeat > currNoteSubBeat.subBeat) allNoteSubBeats
                   |> List.map (\nsb -> nsb.subBeat)
                   |> List.minimum
    prevSubBeat = case List.filter (\nsb -> nsb.subBeat < currNoteSubBeat.subBeat && nsb.isRest == False) allNoteSubBeats
                        |> List.map (\nsb -> nsb.subBeat)
                        |> List.maximum of
                    Just pSubBeat -> pSubBeat
                    _ -> currNoteSubBeat.subBeat
  in
  case nextSubBeat of
      Just nxtSubBeat -> if nxtSubBeat - currNoteSubBeat.subBeat == 3 then NoteDurationParam SemiQuaver False nxtSubBeat prevSubBeat
                         else if nxtSubBeat - currNoteSubBeat.subBeat == 6 then NoteDurationParam Quaver False nxtSubBeat prevSubBeat
                         else if nxtSubBeat - currNoteSubBeat.subBeat == 9 then NoteDurationParam Quaver True nxtSubBeat prevSubBeat
                         else NoteDurationParam Crotchet False nxtSubBeat prevSubBeat
      _ -> if currNoteSubBeat.subBeat == 1 then NoteDurationParam Crotchet False currNoteSubBeat.nextSubBeat prevSubBeat
           else if currNoteSubBeat.subBeat == 4 then NoteDurationParam Quaver True currNoteSubBeat.nextSubBeat prevSubBeat
           else if currNoteSubBeat.subBeat == 7 then NoteDurationParam Quaver False currNoteSubBeat.nextSubBeat prevSubBeat
           else NoteDurationParam SemiQuaver False currNoteSubBeat.nextSubBeat prevSubBeat

renderNote : Int -> Float -> Int -> Int -> NoteSubBeat -> List (Svg Msg)
renderNote barOffset staveOffset beat beatsCount noteSubBeat = 
  let
    instrument = Dict.get noteSubBeat.instrumentName instrumentDict
    noteCenterX = (toFloat (barOffset * 92)) + 17.0 + ((toFloat (((beat - 1) * (3 * beatsCount)) + (noteSubBeat.subBeat - 1))) * 1.8)
    nextNoteCenterX = (toFloat (barOffset * 92)) + 17.0 + ((toFloat (((beat - 1) * (3 * beatsCount)) + (noteSubBeat.nextSubBeat - 1))) * 1.8)
    crossNoteOffset = 1.0
    noteCenterY = case instrument of
                    Just instr -> instr.stavePosition + (staveShiftY * staveOffset)
                    _ -> 0
    noteShape = case instrument of
                    Just instr -> instr.noteShape
                    _ -> Ovoid  
    stalk = if noteShape == Rest then []
            else
              if noteShape == Cross || noteShape == CrossLedger then
                [Svg.path
                  [ strokeWidth "0.3"
                  , stroke "black"
                  , d ("M " ++ (String.fromFloat (noteCenterX + 1.2)) ++ " " ++ (String.fromFloat (noteSubBeat.stalkHeight + (staveShiftY * staveOffset)))
                      ++ " L " ++ (String.fromFloat (noteCenterX + 1.2)) ++ " " ++ String.fromFloat (noteCenterY + 1.2 ))
                  ]
                  []]
              else
                [Svg.path
                  [ strokeWidth "0.3"
                  , stroke "black"
                  , d ("M " ++ (String.fromFloat (noteCenterX + 1.2)) ++ " " ++ (String.fromFloat (noteSubBeat.stalkHeight + (staveShiftY * staveOffset)))
                  ++ " L " ++ (String.fromFloat (noteCenterX + 1.2)) ++ " " ++ (String.fromFloat (noteCenterY )))
                  ]
                  []]
    dot = if noteSubBeat.isDotted then
            [Svg.circle 
              [cx (String.fromFloat (noteCenterX + (if noteSubBeat.isGhostNote then 2.9 else 2.4)))
               , cy (String.fromFloat noteCenterY)
               , r "0.4"
              ] []]
          else []  

    topBeam = if noteSubBeat.subBeat == noteSubBeat.nextSubBeat 
                 || noteSubBeat.isRest then []
              else [Svg.path 
                      [ strokeWidth "0.6"
                      , stroke "black"
                      , d ("M " ++ (String.fromFloat (noteCenterX + 1.05)) ++ " " ++ (String.fromFloat (noteSubBeat.stalkHeight + (staveShiftY * staveOffset))) 
                        ++ " L " ++ (String.fromFloat (nextNoteCenterX + 1.35)) ++ " " ++ (String.fromFloat (noteSubBeat.stalkHeight + (staveShiftY * staveOffset))))
                      ] []]
    
    semiQuaverBeam =  if noteSubBeat.subdivision == "4-16" then
                        if noteSubBeat.noteDuration == SemiQuaver 
                          && Basics.not noteSubBeat.isRest then 
                          if noteSubBeat.subBeat == noteSubBeat.nextSubBeat then --last (or only) note in the beat
                            if noteSubBeat.subBeat == 10 then --last subBeat position 
                              if noteSubBeat.prevSubBeat /= noteSubBeat.subBeat then --short semi quaver bar goes to the left
                                [Svg.path 
                                  [ strokeWidth "0.6"
                                  , stroke "black"
                                  , d ("M " ++ (String.fromFloat (noteCenterX + 1.05)) ++ " " ++ (String.fromFloat (noteSubBeat.stalkHeight + (staveShiftY * staveOffset) + 1.4)) 
                                    ++ " L " ++ (String.fromFloat (noteCenterX - 0.8)) ++ " " ++ (String.fromFloat (noteSubBeat.stalkHeight + (staveShiftY * staveOffset) + 1.4)))
                                  ] []
                                ]
                              else  --single semiquaver, no beam
                                [Svg.image [xlinkHref "assets/images/semiquaver.svg"
                                            , Svg.Attributes.width "5"
                                            , Svg.Attributes.height "7"
                                            , Svg.Attributes.x (String.fromFloat (noteCenterX - 0.25))
                                            , Svg.Attributes.y (String.fromFloat (noteSubBeat.stalkHeight + (staveShiftY * staveOffset) - 0.5))] []]
                            else --short semi quaver bar goes to the right
                                [Svg.path 
                                  [ strokeWidth "0.6"
                                  , stroke "black"
                                  , d ("M " ++ (String.fromFloat (noteCenterX + 1.05)) ++ " " ++ (String.fromFloat (noteSubBeat.stalkHeight + (staveShiftY * staveOffset) + 1.4)) 
                                      ++ " L " ++ (String.fromFloat (noteCenterX + 3.1)) ++ " " ++ (String.fromFloat (noteSubBeat.stalkHeight + (staveShiftY * staveOffset) + 1.4)))
                                  ] []
                                ]
                          else if noteSubBeat.nextSubBeatNoteDuration == SemiQuaver then --full semi quaver bar goes to next note
                                  [Svg.path 
                                    [ strokeWidth "0.6"
                                    , stroke "black"
                                    , d ("M " ++ (String.fromFloat (noteCenterX + 1.05)) ++ " " ++ (String.fromFloat (noteSubBeat.stalkHeight + (staveShiftY * staveOffset) + 1.4)) 
                                        ++ " L " ++ (String.fromFloat (nextNoteCenterX + 1.35)) ++ " " ++ (String.fromFloat (noteSubBeat.stalkHeight + (staveShiftY * staveOffset) + 1.4)))
                                    ] []
                                ]
                              else --short semi quaver bar goes to the right
                                  if noteSubBeat.subBeat /= noteSubBeat.nextSubBeat
                                    && noteSubBeat.subBeat /= noteSubBeat.prevSubBeat then
                                      [] --this handles the K block issue
                                  else
                                    [Svg.path 
                                      [ strokeWidth "0.6"
                                      , stroke "black"
                                      , d ("M " ++ (String.fromFloat (noteCenterX + 1.05)) ++ " " ++ (String.fromFloat (noteSubBeat.stalkHeight + (staveShiftY * staveOffset) + 1.4)) 
                                          ++ " L " ++ (String.fromFloat (noteCenterX + 3.1)) ++ " " ++ (String.fromFloat (noteSubBeat.stalkHeight + (staveShiftY * staveOffset) + 1.4)))
                                      ] []
                                    ]
                        else if noteSubBeat.noteDuration == Quaver 
                                && Basics.not noteSubBeat.isRest
                                && noteSubBeat.subBeat == noteSubBeat.prevSubBeat 
                                && noteSubBeat.subBeat == noteSubBeat.nextSubBeat then --single quaver, no beam
                                [Svg.image [xlinkHref "assets/images/quaver.svg"
                                            , Svg.Attributes.width "5"
                                            , Svg.Attributes.height "7"
                                            , Svg.Attributes.x (String.fromFloat (noteCenterX - 0.25))
                                            , Svg.Attributes.y (String.fromFloat (noteSubBeat.stalkHeight + (staveShiftY * staveOffset) - 0.5))
                                            ] []]
                            else []
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
                      , d ("M " ++ (String.fromFloat (noteCenterX - 1.05)) ++ " " ++ (String.fromFloat (noteSubBeat.stalkHeight + (staveShiftY * staveOffset) - 3.0)) 
                                ++ " L "++ (String.fromFloat (noteCenterX + 1.05)) ++ " " ++ (String.fromFloat (noteSubBeat.stalkHeight + (staveShiftY * staveOffset) - 2.0)) 
                                )]
                      []
                    ,Svg.path 
                      [ strokeWidth "0.3"
                      , stroke "black"
                      , d ("M " ++ (String.fromFloat (noteCenterX + 1.05)) ++ " " ++ (String.fromFloat (noteSubBeat.stalkHeight + (staveShiftY * staveOffset) - 2.0)) 
                                ++ " L "++ (String.fromFloat (noteCenterX - 1.05)) ++ " " ++ (String.fromFloat (noteSubBeat.stalkHeight + (staveShiftY * staveOffset) - 1.0)) 
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
            [ strokeWidth "0.5"
              , stroke "black"
              , d ("M " ++ (String.fromFloat (noteCenterX - 2)) ++ " " ++ (String.fromFloat (noteCenterY - 2)) 
                  ++ " L " ++ (String.fromFloat (noteCenterX + 2)) ++ " " ++ (String.fromFloat (noteCenterY + 2)) )
            ] []
          ,Svg.path
            [ strokeWidth "0.5"
              , stroke "black"
              , d ("M " ++ (String.fromFloat (noteCenterX - 2)) ++ " " ++ (String.fromFloat (noteCenterY + 2)) 
                  ++ " L " ++ (String.fromFloat (noteCenterX + 2)) ++ " " ++ (String.fromFloat (noteCenterY - 2)) )
            ] []
          ,Svg.path
            [ strokeWidth "0.3"
              , stroke "black"
              , d ("M " ++ (String.fromFloat (noteCenterX - 2.5)) ++ " " ++ (String.fromFloat (noteCenterY)) 
                  ++ " L " ++ (String.fromFloat (noteCenterX + 2.5)) ++ " " ++ (String.fromFloat (noteCenterY)) )
            ] []
          ]
      Triangle ->
          [Svg.circle [cx (String.fromFloat noteCenterX), cy (String.fromFloat noteCenterY), r "1.5"] []]
      Rest ->
          case noteSubBeat.noteDuration of
              Crotchet ->
                      [Svg.image [xlinkHref "assets/images/crotchet-rest.svg"
                                  , Svg.Attributes.width "4%"
                                  , Svg.Attributes.height "4%"
                                  , Svg.Attributes.x (String.fromFloat (noteCenterX - 4))
                                  , Svg.Attributes.y (String.fromFloat (noteCenterY - 3))
                                  ] [] ]
              Quaver ->
                      [Svg.image [xlinkHref "assets/images/quaver-rest.svg"
                                  , Svg.Attributes.width "3%"
                                  , Svg.Attributes.height "3%"
                                  , Svg.Attributes.x (String.fromFloat (noteCenterX - 3))
                                  , Svg.Attributes.y (String.fromFloat (noteCenterY - 2.5))
                                  ] [] ]
              SemiQuaver ->
                      [Svg.image [xlinkHref "assets/images/16th_rest.svg"
                                  , Svg.Attributes.width "1.8%"
                                  , Svg.Attributes.height "4.0%"
                                  , Svg.Attributes.x (String.fromFloat (noteCenterX - 3))
                                  , Svg.Attributes.y (String.fromFloat (noteCenterY - 2.4))
                                  ] [] ]
              _ -> []
          )                                    
      ++ stalk
      ++ dot
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

  
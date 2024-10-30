module Main exposing (main)

import Browser
import Browser.Navigation as Nav
import Html exposing (..)
import Html.Attributes exposing (..)
import Html.Attributes as HA
import Html.Events exposing (on)
import Url
--import Platform exposing (Router)
import Binary exposing (..)
import Svg exposing (..)
import Svg.Attributes exposing (..)
import Dict exposing (..)
import Json.Decode as Json


-- MAIN


main : Program () Model Msg
main =
  Browser.application
    { init = init
    , view = view
    , update = update
    , subscriptions = subscriptions
    , onUrlChange = UrlChanged
    , onUrlRequest = LinkClicked
    }



-- MODEL


type alias Model =
  { subdivisions : List Subdivision
  , instruments : List String
  , arrangement : List InstrumentBlocks
  --, subdivisionSelect : Select.Select Subdivision
  , debugText : String
  }

type alias Subdivision =
  { name : String
  , description : String 
  , subBeats : Int
  }

type NoteShape = 
  Triangle
  | Cross
  | CrossLedger
  | Ovoid

type alias Instrument = 
  { staveLocation : String
  , stavePosition : Float 
  , noteShape : NoteShape
  }


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

type alias Block = 
  { imageName : String
  , notePlacement : Bits
  , subdivision : String
  }

type alias InstrumentBlocks = 
  { instrumentName : String
  , blockNames : List String
  }

type alias NoteBlock =
  { blockBeat : Int
  ,stavePos : Float
  ,noteShape : NoteShape
  ,blockName : String
  }

staveLines : List Float
staveLines =
    [ 0, 3, 6, 9, 12]

staveShiftY : Float
staveShiftY = 20

instrumentDict : Dict String Instrument
instrumentDict = Dict.fromList 
    [("Hi-Hat", Instrument "G5" -1.5 Cross)
    , ("Ride Cymbal", Instrument "F5" 0 CrossLedger)
    , ("High Tom", Instrument "E5" 1.5 Ovoid)
    , ("Mid Tom", Instrument "D5" 3 Ovoid)
    , ("Snare", Instrument "C5" 4.5 Ovoid)
    , ("Floor Tom", Instrument "A4" 7.5 Ovoid)
    , ("Bass Drum", Instrument "F4" 10.5 Ovoid)      
    , ("Hi-hat Foot", Instrument "D4" 13 Cross)
    ]

blockDict : Dict String Block
blockDict = Dict.fromList 
              [ ("A", Block "A.png" (Binary.fromIntegers [1,0,0,0]) "4-16")
              , ("B", Block "B.png" (Binary.fromIntegers [0,1,0,0]) "4-16")
              , ("C", Block "B.png" (Binary.fromIntegers [0,0,1,0]) "4-16")
              , ("D", Block "B.png" (Binary.fromIntegers [0,0,0,1]) "4-16")
              , ("E", Block "B.png" (Binary.fromIntegers [1,1,0,0]) "4-16")
              , ("F", Block "B.png" (Binary.fromIntegers [0,1,1,0]) "4-16")
              , ("G", Block "B.png" (Binary.fromIntegers [0,0,1,1]) "4-16")
              , ("H", Block "B.png" (Binary.fromIntegers [1,0,0,1]) "4-16")
              , ("I", Block "B.png" (Binary.fromIntegers [1,0,1,0]) "4-16")
              , ("J", Block "B.png" (Binary.fromIntegers [0,1,0,1]) "4-16")
              , ("K", Block "B.png" (Binary.fromIntegers [1,1,1,0]) "4-16")
              , ("L", Block "B.png" (Binary.fromIntegers [0,1,1,1]) "4-16")
              , ("M", Block "B.png" (Binary.fromIntegers [1,0,1,1]) "4-16")
              , ("N", Block "B.png" (Binary.fromIntegers [1,1,0,1]) "4-16")
              , ("O", Block "B.png" (Binary.fromIntegers [1,1,1,1]) "4-16")
              , ("P", Block "B.png" (Binary.fromIntegers [0,0,0,0]) "4-16")
              , ("Q", Block "B.png" (Binary.fromIntegers [1,0,0]) "3-8")
              , ("R", Block "B.png" (Binary.fromIntegers [0,1,0]) "3-8")
              , ("S", Block "B.png" (Binary.fromIntegers [0,0,1]) "3-8")
              , ("T", Block "B.png" (Binary.fromIntegers [1,1,0]) "3-8")
              , ("U", Block "B.png" (Binary.fromIntegers [0,1,1]) "3-8")
              , ("V", Block "B.png" (Binary.fromIntegers [1,0,1]) "3-8")
              , ("W", Block "B.png" (Binary.fromIntegers [1,1,1]) "3-8")
              , ("X", Block "B.png" (Binary.fromIntegers [0,0,0]) "3-8")
              ]

-- INIT

init : () -> Url.Url -> Nav.Key -> ( Model, Cmd Msg )
init flags url key =
    let 
        model = initialModel
    in
    ( model, Cmd.none )


initialModel : Model
initialModel = 
    { subdivisions = [Subdivision "4-16" "Four 16ths" 4
                     ,Subdivision "3-8" "Three 8ths" 3]
    , instruments = ["Hi-Hat", "Snare", "Bass Drum"]
    , arrangement = [ InstrumentBlocks "Hi-Hat" ["W", "B", "C", "D"]
                    , InstrumentBlocks "Snare" ["P", "A", "P", "A"]
                    , InstrumentBlocks "Bass Drum" ["A", "P", "A", "P"]
                    ]     
    --, subdivisionSelect = Select.init "select-subdivision" |> Select.setItems [Subdivision "4-16" "Four 16ths" 4
    --                                                                          ,Subdivision "3-8" "Three 8ths" 3]
    , debugText = ""
    }   


-- UPDATE


type Msg
  = LinkClicked Browser.UrlRequest
  | UrlChanged Url.Url
  | BlockSelectedChange SelectIdValue
  --| SubdivisionSelectMsg (Select.Msg Subdivision)

update : Msg -> Model -> ( Model, Cmd Msg )
update msg model = 
    case msg of
        BlockSelectedChange param ->  let
                                        idList = String.split "~" param.id
                                        instrName = List.head idList
                                        blockIndex = List.head (List.reverse idList)
                                        arr = model.arrangement
                                      in
                                      case instrName of
                                          Just iName -> case blockIndex of
                                                          Just bIndex -> ({model | arrangement = (updateArrangement iName bIndex param.value arr)}, Cmd.none)
                                                          _ -> ({model | debugText = (Debug.toString value)}, Cmd.none)
                                          _ -> ({model | debugText = (Debug.toString value)}, Cmd.none)
        _ -> (model, Cmd.none)


updateArrangement : String -> String -> String -> List InstrumentBlocks -> List InstrumentBlocks
updateArrangement instrName blockIndex newVal currArrangement =
  case String.toInt blockIndex of
      Just bIndex -> (currArrangement) |> List.map (\a -> if a.instrumentName == instrName then 
                                                             InstrumentBlocks instrName (a.blockNames |> List.indexedMap (\i b -> if i == bIndex then newVal else b))
                                                          else a)
      _ -> currArrangement

-- SUBSCRIPTIONS


subscriptions : Model -> Sub Msg
subscriptions _ =
  Sub.none



-- VIEW


view : Model -> Browser.Document Msg
view model =
  { title = "Drum Blocks"
  , body =
      [
        Html.table
              [] 
              ([Html.tr 
                        [HA.class "instrumentTableHeaderRow"] 
                        [th [HA.class "instrumentTableHeaderCell"] 
                            [Html.text "Instrument"]
                        ,th [HA.class "instrumentTableHeaderCell"] 
                            [Html.text "Rhythm Blocks"]
                        ]]
               ++ (instrumentView model.arrangement)
               ++ [Html.tr 
                        [HA.class "instrumentTableRow"] 
                        [td [] [Html.text "+ Add Instrument"]
                        ,td [] []
                        ]
                  ])
      , div []
            [svg
                [ viewBox "0 0 200 100"
                , Svg.Attributes.class "stave"
                ]
                (stave ++ percussionClef ++ (renderBar model.arrangement))
            ]

      , Html.text model.debugText
      ]
  }


subdivisionDropdown : Model -> Html Msg
subdivisionDropdown model = 
     div
      [] 
      [
        (div 
          [] 
          [ Html.text "Subdivision:"
          , select [] (List.map subdivisionOption model.subdivisions)
          ]
        ) 
      ]


subdivisionOption : Subdivision -> Html Msg
subdivisionOption subdiv = 
    Html.option [] [Html.text subdiv.description]

instrumentView : List InstrumentBlocks -> List (Html Msg)
instrumentView instrumentBlocks = 
  (instrumentBlocks) |> List.map (\ib -> tr [Html.Attributes.class "instrumentTableRow"]
                                            [td [Html.Attributes.class "instrumentTableCell"]
                                                [Html.text ib.instrumentName]
                                            , td []
                                                 [instrumentRow ib]
                                            ])

instrumentRow : InstrumentBlocks -> Html Msg
instrumentRow instrumentBlock = 
  div 
    [] 
    (blockView instrumentBlock)

blockView : InstrumentBlocks -> List (Html Msg)
blockView instrblocks = 
  let
    instrName = instrblocks.instrumentName
    blocks = instrblocks.blockNames
  in
  (blocks) |> List.indexedMap (\i b -> blockButton instrName i b)

blockButton : String -> Int -> String -> Html Msg
blockButton instrName index blockName = 
  Html.select [onChange BlockSelectedChange
              , HA.id (instrName ++ "~" ++ String.fromInt index)
              , HA.class "instrumentBlockSelect"
              ]
              (getBlockOptions blockName)

getBlockOptions : String -> List (Html Msg)
getBlockOptions blockName = 
  (Dict.keys blockDict) |> List.map (\k -> (Html.option [if blockName == k then selected True else selected False] [Html.text k]))


type alias SelectIdValue = 
  {
    id : String
    ,value : String
  }

onChange : (SelectIdValue -> msg) -> Html.Attribute msg
onChange tagger =
  on "change" (Json.map tagger selectDecoder)

targetIdDecoder : Json.Decoder String
targetIdDecoder =
  Json.at ["target", "id"] Json.string

targetValueDecoder : Json.Decoder String
targetValueDecoder =
  Json.at ["target", "value"] Json.string


selectDecoder : Json.Decoder SelectIdValue
selectDecoder =
  Json.map2 SelectIdValue targetIdDecoder targetValueDecoder

stave : List (Svg Msg)
stave =
    (staveLines)
        |> List.map
            (\n ->
                Svg.path
                    [ strokeWidth "0.3"
                    , stroke "black"
                    , d ("M 0 " ++ String.fromFloat (n + staveShiftY) ++ " L 355 " ++ String.fromFloat (n + staveShiftY))
                    ]
                    []
            )


  
percussionClef : List(Svg Msg)
percussionClef =
  [Svg.path
      [ strokeWidth "1.8"
      , stroke "black"
      , d ("M 5 " ++ String.fromFloat (3 + staveShiftY) ++ " L 5 " ++ String.fromFloat (9 + staveShiftY))
      ]
      []
  ,Svg.path
      [ strokeWidth "1.8"
      , stroke "black"
      , d ("M 8 " ++ String.fromFloat (3 + staveShiftY) ++ " L 8 " ++ String.fromFloat (9 + staveShiftY))
      ]
      []
  ]
  
{-
Each beat in a bar is divided into 12 equal spaces because 12 is divisible by 3 and 4 meaning we can evenly space
both triplets and 16ths, e.g.

One             Trip            Let
O               O               O    
1   2   3   4   5   6   7   8   9   10    11    12
O           O           O           O
One         E           And         A

So we need to iterate through all 12 spaces and all items in the arrangement, and draw a note if required.
-}

renderBar : List InstrumentBlocks -> List(Svg Msg)
renderBar instrumentBlocks = 
  let
    beatCount = List.range 1 4
    subBeats = List.range 1 12
    noteBlocks = List.map createInstrumentBlocks instrumentBlocks |> List.concat
  in
  (beatCount) |> List.concatMap (\i -> List.map (\j -> (i,j)) subBeats)
              |> List.map (\i -> beatLoop (Tuple.first i) (Tuple.second i) noteBlocks)
              |> List.concat
              --|> Debug.toString


{-
Variable beat is looping from 1 to 4, within this subBeat is looping from 1 to 12.

noteBlock has value blockBeat in range 1 to 4, it also has the block name.

If beat == blockBeat && the block has a note on the subBeat then draw note
else do nothing
-}
beatLoop : Int -> Int -> List(NoteBlock) -> List(Svg Msg)
beatLoop beat subBeat noteBlocks  = 
  (noteBlocks) |> List.map (\n -> blockLoop beat subBeat n)
                      |> List.concat

blockLoop : Int -> Int -> NoteBlock -> List(Svg Msg)
blockLoop beat subBeat noteBlock = 
  (if (beat == noteBlock.blockBeat
     && isSubBeatMatch subBeat noteBlock) then
    renderNote beat subBeat noteBlock
  else
    []
  ) 


renderNote : Int -> Int -> NoteBlock -> List (Svg Msg)
renderNote beat subBeat noteBlock = 
  let
      noteCenterX = 20.0 + ((toFloat (((beat - 1) * 12) + (subBeat - 1))) * 3.6)
      noteCenterY = noteBlock.stavePos + staveShiftY
  in
  case noteBlock.noteShape of
      Ovoid ->
          [Svg.ellipse 
            [cx (String.fromFloat noteCenterX)
              , cy (String.fromFloat noteCenterY)
              , rx "1.6"
              , ry "1.4"
              , transform ("rotate(-15, " ++ (String.fromFloat noteCenterX) ++ ", " ++ (String.fromFloat noteCenterY) ++ ")")
            ] []]
      Cross ->
          [Svg.path
            [ strokeWidth "0.4"
              , stroke "black"
              , d ("M " ++ (String.fromFloat (noteCenterX - 1.5)) ++ " " ++ (String.fromFloat (noteCenterY - 1.5)) ++ " L " ++ (String.fromFloat (noteCenterX + 1.5)) ++ " " ++ (String.fromFloat (noteCenterY + 1.5)) )
            ] []
          ,Svg.path
            [ strokeWidth "0.4"
              , stroke "black"
              , d ("M " ++ (String.fromFloat (noteCenterX - 1.5)) ++ " " ++ (String.fromFloat (noteCenterY + 1.5)) ++ " L " ++ (String.fromFloat (noteCenterX + 1.5)) ++ " " ++ (String.fromFloat (noteCenterY - 1.5)) )
            ] []
          ]
      CrossLedger ->
          [Svg.path
            [ strokeWidth "0.5"
              , stroke "black"
              , d ("M " ++ (String.fromFloat (noteCenterX - 2)) ++ " " ++ (String.fromFloat (noteCenterY - 2)) ++ " L " ++ (String.fromFloat (noteCenterX + 2)) ++ " " ++ (String.fromFloat (noteCenterY + 2)) )
            ] []
          ,Svg.path
            [ strokeWidth "0.5"
              , stroke "black"
              , d ("M " ++ (String.fromFloat (noteCenterX - 2)) ++ " " ++ (String.fromFloat (noteCenterY + 2)) ++ " L " ++ (String.fromFloat (noteCenterX + 2)) ++ " " ++ (String.fromFloat (noteCenterY - 2)) )
            ] []
          ,Svg.path
            [ strokeWidth "0.3"
              , stroke "black"
              , d ("M " ++ (String.fromFloat (noteCenterX - 2.5)) ++ " " ++ (String.fromFloat (noteCenterY)) ++ " L " ++ (String.fromFloat (noteCenterX + 2.5)) ++ " " ++ (String.fromFloat (noteCenterY)) )
            ] []
          ]
      Triangle ->
          [Svg.circle [cx (String.fromFloat noteCenterX), cy (String.fromFloat noteCenterY), r "1.5"] []]


{-
isSubBeatMatch returns True if the block has a note that corresponds with the sub beat
-}  
isSubBeatMatch :  Int -> NoteBlock -> Bool
isSubBeatMatch subBeat noteBlock =
  let
    blockQuery = Dict.get noteBlock.blockName blockDict 
  in
  case blockQuery of
    Just block -> case block.subdivision of 
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
    Nothing -> False    

createInstrumentBlocks : InstrumentBlocks -> List(NoteBlock)
createInstrumentBlocks instrBlocks = 
  let
    sp = getStavePosition instrBlocks.instrumentName
    ns = getNoteShape instrBlocks.instrumentName
  in
  (instrBlocks.blockNames)  |> List.indexedMap (\i b ->  {blockBeat = i + 1
                                                          , stavePos = sp
                                                          , noteShape = ns
                                                          , blockName = b})

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

  
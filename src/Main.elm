port module Main exposing (main)

import Browser
import Browser.Navigation as Nav
import Browser.Events exposing (onKeyDown)
import Html exposing (..)
import Html.Attributes exposing (..)
import Html.Attributes as HA
import Html.Events exposing (on, onClick)
import Url
--import Platform exposing (Router)
import Binary exposing (..)
import Svg exposing (..)
import Svg.Attributes exposing (..)
import Dict exposing (..)
import Json.Decode as Json

import CommonEvents exposing (..)

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
  { arrangement : InstrumentBlocksDict
  , timeSignature : String
  , blockOptionsParams : BlockOptionsParams
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
  | Rest

type alias Instrument = 
  { staveLocation : String
  , stavePosition : Float 
  , noteShape : NoteShape
  , isGhostNoteable : Bool
  , isAccentable : Bool
  , sortOrder : Int
  }

type NoteDuration =
  Crotchet
  | Quaver
  | SemiQuaver
  | Minim
  | Breve


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
  , ghostNotes : GhostNotes
  , isAccented : Bool
  }

type alias NoteDurationParam = 
  {
    noteDuration : NoteDuration
    ,isDotted : Bool
    ,nextSubBeat : Int
    ,prevSubBeat : Int
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
  {blockName : String
  , imageName : String
  , notePlacement : Bits
  , subdivision : String
  }

type GhostNotes = 
  HasGhostNotes
  | NoGhostNotes

type alias InstrumentBlocksDict = Dict String BeatBlockOptionsDict

type alias BeatBlockOptionsDict = Dict Int BlockOptions

type alias BlockOptions = 
  { block : Block
  , ghostNotes : GhostNotes
  , accentPattern : Bits
  }

type alias BlockOptionsParams = 
  {beat : Int
  , subBeat : Int
  , instrumentName : String
  , blockName : String
  , ghostNotes : GhostNotes
  , accentPattern : Bits
  }

subdivisions : List Subdivision
subdivisions =
  [Subdivision "4-16" "Four 16ths" 4
  , Subdivision "3-8" "Three 8ths" 3]
     

staveLines : List Float
staveLines =
    [ 0, 3, 6, 9, 12]

staveShiftY : Float
staveShiftY = 20

instrumentDict : Dict String Instrument
instrumentDict = Dict.fromList 
    [("Hi-Hat", Instrument "G5" -1.5 Cross False True 10)
    , ("Ride Cymbal", Instrument "F5" 0 CrossLedger False True 20)
    , ("High Tom", Instrument "E5" 1.5 Ovoid True True 30)
    , ("Mid Tom", Instrument "D5" 3 Ovoid True True 40)
    , ("Snare", Instrument "C5" 4.5 Ovoid True True 50)
    , ("Floor Tom", Instrument "A4" 7.5 Ovoid True True 60)
    , ("Bass Drum", Instrument "F4" 10.5 Ovoid False False 70)      
    , ("Hi-hat Foot", Instrument "D4" 13 Cross False False 80)
    , ("Rest", Instrument "" 7 Rest False False 0)
    ]

{-
  A and P blocks are needed in the initial setup
-}
aBlock : Block
aBlock = Block "A" "A.png" (Binary.fromIntegers [1,0,0,0]) "4-16"

pBlock : Block
pBlock = Block "P" "P.png" (Binary.fromIntegers [0,0,0,0]) "4-16"

blockDict : Dict String Block
blockDict = Dict.fromList 
              [ ("A", aBlock)
              , ("B", Block "B" "B.png" (Binary.fromIntegers [0,1,0,0]) "4-16")
              , ("C", Block "C" "C.png" (Binary.fromIntegers [0,0,1,0]) "4-16")
              , ("D", Block "D" "D.png" (Binary.fromIntegers [0,0,0,1]) "4-16")
              , ("E", Block "E" "E.png" (Binary.fromIntegers [1,1,0,0]) "4-16")
              , ("F", Block "F" "F.png" (Binary.fromIntegers [0,1,1,0]) "4-16")
              , ("G", Block "G" "G.png" (Binary.fromIntegers [0,0,1,1]) "4-16")
              , ("H", Block "H" "H.png" (Binary.fromIntegers [1,0,0,1]) "4-16")
              , ("I", Block "I" "I.png" (Binary.fromIntegers [1,0,1,0]) "4-16")
              , ("J", Block "J" "J.png" (Binary.fromIntegers [0,1,0,1]) "4-16")
              , ("K", Block "K" "K.png" (Binary.fromIntegers [1,1,1,0]) "4-16")
              , ("L", Block "L" "L.png" (Binary.fromIntegers [0,1,1,1]) "4-16")
              , ("M", Block "M" "M.png" (Binary.fromIntegers [1,0,1,1]) "4-16")
              , ("N", Block "N" "N.png" (Binary.fromIntegers [1,1,0,1]) "4-16")
              , ("O", Block "O" "O.png" (Binary.fromIntegers [1,1,1,1]) "4-16")
              , ("P", pBlock)
              , ("Q", Block "Q" "Q.png" (Binary.fromIntegers [1,0,0]) "3-8")
              , ("R", Block "R" "R.png" (Binary.fromIntegers [0,1,0]) "3-8")
              , ("S", Block "S" "S.png" (Binary.fromIntegers [0,0,1]) "3-8")
              , ("T", Block "T" "T.png" (Binary.fromIntegers [1,1,0]) "3-8")
              , ("U", Block "U" "U.png" (Binary.fromIntegers [0,1,1]) "3-8")
              , ("V", Block "V" "V.png" (Binary.fromIntegers [1,0,1]) "3-8")
              , ("W", Block "W" "W.png" (Binary.fromIntegers [1,1,1]) "3-8")
              , ("X", Block "X" "X.png" (Binary.fromIntegers [0,0,0]) "3-8")
              ]

blockOptionsDialog : String -> List (Html msg) -> Html msg
blockOptionsDialog dialogId content =
    Html.node "dialog" [ HA.id dialogId ] content

port toggleDialog : String -> Cmd msg

defaultBlockOptions : BlockOptionsParams
defaultBlockOptions = 
  BlockOptionsParams -1 -1 "" "" NoGhostNotes Binary.empty

-- INIT

init : () -> Url.Url -> Nav.Key -> ( Model, Cmd Msg )
init flags url key =
  let 
      model = initialModel
  in
  ( model, Cmd.none )


initialModel : Model
initialModel = 
    { arrangement = Dict.fromList [("Hi-Hat", (Dict.fromList [(1, BlockOptions aBlock NoGhostNotes Binary.empty)
                                                                , (2, BlockOptions aBlock NoGhostNotes Binary.empty)
                                                                , (3, BlockOptions aBlock NoGhostNotes Binary.empty)
                                                                , (4, BlockOptions aBlock NoGhostNotes Binary.empty)])) 
                                  , ("Snare", (Dict.fromList [(1, BlockOptions pBlock HasGhostNotes Binary.empty)
                                                                            , (2, BlockOptions aBlock NoGhostNotes (Binary.fromIntegers [1,0,0,0]))
                                                                            , (3, BlockOptions pBlock NoGhostNotes Binary.empty)
                                                                            , (4, BlockOptions aBlock NoGhostNotes Binary.empty)]))
                                  , ("Bass Drum", (Dict.fromList [(1, BlockOptions aBlock NoGhostNotes Binary.empty)
                                                                                , (2, BlockOptions pBlock NoGhostNotes Binary.empty)
                                                                                , (3, BlockOptions aBlock NoGhostNotes Binary.empty)
                                                                                , (4, BlockOptions pBlock NoGhostNotes Binary.empty)]))
                    ]     
    , timeSignature = "4/4"
    , blockOptionsParams = defaultBlockOptions
    --, subdivisionSelect = Select.init "select-subdivision" |> Select.setItems [Subdivision "4-16" "Four 16ths" 4
    --                                                                          ,Subdivision "3-8" "Three 8ths" 3]
    , debugText = ""
    }   


-- UPDATE


type Msg
  = LinkClicked Browser.UrlRequest
  | UrlChanged Url.Url
  | BlockSelectedChange SelectIdValue
  | BlockOptionsDialogOpen BlockOptionsParams
  | BlockOptionsDialogSave
  | BlockOptionsDialogCancel
  | GhostNotesCheckBoxChanged CheckboxIdChecked
  | AccentCheckBoxChanged CheckboxIdChecked
  | KeyPressedMsg KeyEventMsg
  | KeyReleasedMsg KeyEventMsg

type KeyEventMsg
    = KeyEventControl
    | KeyEventAlt
    | KeyEventShift
    | KeyEventMeta
    | KeyEventLetter Char
    | KeyEventUnknown String
  --| SubdivisionSelectMsg (Select.Msg Subdivision)

update : Msg -> Model -> ( Model, Cmd Msg )
update msg model = 
    case msg of
        BlockSelectedChange param -> applyBlockSelectedChange model param
        BlockOptionsDialogOpen params -> ({model | blockOptionsParams = params}, toggleDialog "block-options-dialog")
        BlockOptionsDialogSave -> ({model | arrangement = updateArrangement model.blockOptionsParams.instrumentName
                                                                            model.blockOptionsParams.subBeat
                                                                            model.blockOptionsParams.blockName
                                                                            model.blockOptionsParams.ghostNotes
                                                                            model.blockOptionsParams.accentPattern
                                                                            model.arrangement
                                            , blockOptionsParams = defaultBlockOptions
                                            }, toggleDialog "block-options-dialog")
        BlockOptionsDialogCancel -> ({model | blockOptionsParams = defaultBlockOptions}, toggleDialog "block-options-dialog")
        GhostNotesCheckBoxChanged param -> let 
                                              opts = BlockOptionsParams model.blockOptionsParams.beat
                                                                        model.blockOptionsParams.subBeat
                                                                        model.blockOptionsParams.instrumentName
                                                                        model.blockOptionsParams.blockName
                                                                        (if param.checked then HasGhostNotes else NoGhostNotes)
                                                                        model.blockOptionsParams.accentPattern
                                           in
                                           ({model | blockOptionsParams = opts}, Cmd.none)
        KeyPressedMsg keyEventMsg -> case keyEventMsg of
                                        KeyEventUnknown key-> if key == "Escape" then 
                                                                ({model | blockOptionsParams = defaultBlockOptions}, toggleDialog "block-options-dialog")
                                                              else 
                                                                (model, Cmd.none)
                                        _ -> (model, Cmd.none)
        AccentCheckBoxChanged param ->  let 
                                              opts = BlockOptionsParams model.blockOptionsParams.beat
                                                                        model.blockOptionsParams.subBeat
                                                                        model.blockOptionsParams.instrumentName
                                                                        model.blockOptionsParams.blockName
                                                                        model.blockOptionsParams.ghostNotes
                                                                        (updateAccentPattern param model.blockOptionsParams.blockName model.blockOptionsParams.accentPattern)
                                        in
                                        ({model | blockOptionsParams = opts}, Cmd.none)
        _ -> ({model | debugText = Debug.toString msg}, Cmd.none)


applyBlockSelectedChange : Model -> SelectIdValue -> ( Model, Cmd Msg )
applyBlockSelectedChange model param = 
  let
    idList = String.split "~" param.id
    instrName = List.head idList
    blockIndex = case List.head (List.reverse idList) of
                  Just index -> index
                  _ -> ""
    arr = model.arrangement
  in
  case instrName of
      Just iName -> case Dict.get iName arr of
                      Just blockOptsDict ->
                          case String.toInt blockIndex of
                            Just bIndex -> ({model | arrangement = (updateArrangement iName bIndex param.value NoGhostNotes (Binary.fromDecimal 0) arr)}, Cmd.none)
                            _           -> ({model | debugText = (Debug.toString value)}, Cmd.none)
                      _ -> ({model | debugText = (Debug.toString value)}, Cmd.none)
      _ -> ({model | debugText = (Debug.toString value)}, Cmd.none)

updateArrangement : String -> Int -> String -> GhostNotes -> Bits -> InstrumentBlocksDict -> InstrumentBlocksDict
updateArrangement instrName blockIndex newBlockName newGhostNotes newAccentPattern currArrangement =
  let
    newBlock = Dict.get newBlockName blockDict
  in
  case newBlock of
      Just nBlock -> 
          case Dict.get instrName currArrangement of
              Just blockOptsDict -> 
                  case Dict.get blockIndex blockOptsDict of
                      Just blockOpts -> let
                                          newBlockOpts = BlockOptions nBlock newGhostNotes newAccentPattern
                                        in
                                        Dict.insert instrName (Dict.insert blockIndex newBlockOpts blockOptsDict) currArrangement
                      _ -> currArrangement 
              _ -> currArrangement
      _ -> currArrangement

updateAccentPattern : CheckboxIdChecked -> String -> Bits -> Bits
updateAccentPattern param blockName currAccentPattern =
  let
    subdivision = case Dict.get blockName blockDict of
                    Just block -> block.subdivision
                    _ -> "4-16"
  in
  case String.toInt (String.right 1 param.id) of
    Just subBeat -> let
                      bitmap =  if subdivision == "4-16" then
                                    if subBeat == 1 then Binary.fromIntegers [1,0,0,0]
                                    else if subBeat == 2 then Binary.fromIntegers [0,1,0,0]
                                    else if subBeat == 3 then Binary.fromIntegers [0,0,1,0]
                                    else Binary.fromIntegers [0,0,0,1]
                                else
                                    if subBeat == 1 then Binary.fromIntegers [1,0,0]
                                    else if subBeat == 2 then Binary.fromIntegers [0,1,0]
                                    else Binary.fromIntegers [0,0,1]
                    in
                    if param.checked then
                                      Binary.or currAccentPattern bitmap
                                    else
                                      Binary.and currAccentPattern (Binary.not bitmap)
    _ -> currAccentPattern

-- SUBSCRIPTIONS


subscriptions : Model -> Sub Msg
subscriptions model =
    Sub.batch
        [ Browser.Events.onKeyDown keyPressedDecoder
        , Browser.Events.onKeyUp keyReleasedDecoder
        ]

keyPressedDecoder : Json.Decoder Msg
keyPressedDecoder =
    Json.map (toKeyEventMsg >> KeyPressedMsg) (Json.field "key" Json.string)


keyReleasedDecoder : Json.Decoder Msg
keyReleasedDecoder =
    Json.map (toKeyEventMsg >> KeyReleasedMsg) (Json.field "key" Json.string)


toKeyEventMsg : String -> KeyEventMsg
toKeyEventMsg eventKeyString =
    case eventKeyString of
        "Control" ->
            KeyEventControl

        "Shift" ->
            KeyEventShift

        "Alt" ->
            KeyEventAlt

        "Meta" ->
            KeyEventMeta

        string_ ->
            case String.uncons string_ of
                Just ( char, "" ) ->
                    KeyEventLetter char

                _ ->
                    KeyEventUnknown eventKeyString
-- VIEW


view : Model -> Browser.Document Msg
view model =
  { title = "Drum Blocks"
  , body =
      [
        Html.table
              [] 
              (Html.tr 
                        [HA.class "instrumentTableHeaderRow"] 
                        [th [HA.class "instrumentTableHeaderCell"] 
                            [Html.text "Instrument"]
                        ,th [HA.class "instrumentTableHeaderCell"] 
                            [Html.text "Bar 1"]
                        ]
               :: (instrumentView model)
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
                (stave ++ percussionClef ++ (staveTimeSignature model) ++ singleBarLine ++ (renderBar model.arrangement))
                --(stave ++ percussionClef)
            ]

      , Html.text model.debugText
      --, Html.text (renderBar model.arrangement)
      ,blockOptionsDialog "block-options-dialog"
                (buildblockOptionsDialog model
                ++  [Html.div [HA.class "blockOptionsDialogButtons"] 
                              [button [ onClick BlockOptionsDialogSave, HA.class "blockOptionsDialogButton", HA.id "bb" ] [ Html.text "Save" ]
                              , button [ onClick BlockOptionsDialogCancel, HA.class "blockOptionsDialogButton" ] [ Html.text "Cancel" ]
                              ]
                    ]
                )
      ]
  }

buildblockOptionsDialog : Model -> List (Html Msg)
buildblockOptionsDialog model = 
  let
    instrumentName = model.blockOptionsParams.instrumentName
    ghostNotes = model.blockOptionsParams.ghostNotes
    isAccentable = case Dict.get instrumentName instrumentDict of
                      Just instr  -> instr.isAccentable
                      _           -> False
  in
  [Html.div  []
            (Html.img [HA.src ("assets/images/" ++ model.blockOptionsParams.blockName ++ ".png")] []
            :: (if instrumentName == "Snare" then 
                  [Html.div [HA.class "blockOptionsContainer"] 
                            [Html.text "Add Ghost Notes?"
                            , Html.input [HA.type_ "checkbox"
                                         , onCheckboxChanged GhostNotesCheckBoxChanged
                                         , checked (if ghostNotes == HasGhostNotes then True else False)
                                         , HA.id "ghostnotes_checkbox"][]
                            ]]
                else []
                )
            ++ (if isAccentable then 
                  [Html.div [HA.class "blockOptionsContainer"] 
                            [Html.text "Accent Pattern"
                            , Html.div [HA.id "accentPatternBox"]
                                       (renderAccentCheckboxes model)
                            ]
                  ]
                else []
                )
            )
  ]

renderAccentCheckboxes : Model -> List (Html Msg)
renderAccentCheckboxes model =
  let
    subBeatRange = case Dict.get model.blockOptionsParams.blockName blockDict of
                      Just block -> if block.subdivision == "4-16" then [{index = 1, bitmap = (Binary.fromIntegers [1,0,0,0])}
                                                                        , {index = 2, bitmap = (Binary.fromIntegers [0,1,0,0])}
                                                                        , {index = 3, bitmap = (Binary.fromIntegers [0,0,1,0])}
                                                                        , {index = 4, bitmap = (Binary.fromIntegers [0,0,0,1])}] 
                                                                   else [{index = 1, bitmap = (Binary.fromIntegers [1,0,0])}
                                                                        , {index = 2, bitmap = (Binary.fromIntegers [0,1,0])}
                                                                        , {index = 3, bitmap = (Binary.fromIntegers [0,0,1])}]
                      _ -> []
    notePlacement = case Dict.get model.blockOptionsParams.blockName blockDict of
                      Just block -> block.notePlacement
                      _ -> Binary.fromIntegers [0,0,0,0]
    accentPattern = model.blockOptionsParams.accentPattern

  in
  subBeatRange |> List.map (\i -> Html.input [HA.type_ "checkbox"
                                             , HA.id ("accent_checkbox_" ++ String.fromInt i.index)
                                             , HA.class "accentCheckbox"
                                             , onCheckboxChanged AccentCheckBoxChanged
                                             , checked (Binary.toDecimal (Binary.and i.bitmap accentPattern) /= 0)
                                             , HA.disabled (Binary.toDecimal (Binary.and i.bitmap notePlacement) == 0)][]
                           )

{-
subdivisionDropdown : Model -> Html Msg
subdivisionDropdown model = 
     div
      [] 
      [
        (div 
          [] 
          [ Html.text "Subdivision:"
          , select [] (List.map subdivisionOption subdivisions)
          ]
        ) 
      ]
-}

subdivisionOption : Subdivision -> Html Msg
subdivisionOption subdiv = 
    Html.option [] [Html.text subdiv.description]

instrumentView : Model -> List (Html Msg)
instrumentView model = 
  (Dict.toList model.arrangement) |> List.map (\item  ->  let
                                                            instrName = (Tuple.first item)
                                                            blockOpts = (Tuple.second item)
                                                            sortOrder = case Dict.get instrName instrumentDict of
                                                                          Just instr -> instr.sortOrder
                                                                          _ -> 100
                                                          in
                                                          {instrName = instrName, blockOpts = blockOpts, sortOrder = sortOrder})
                                  |> List.sortBy .sortOrder
                                  |> List.map (\a -> tr [Html.Attributes.class "instrumentTableRow"]
                                                              [td [Html.Attributes.class "instrumentTableCell"]
                                                                    [Html.text a.instrName]
                                                              , td []
                                                                   [instrumentRow model a.instrName a.blockOpts]
                                                              ])

instrumentRow : Model -> String -> BeatBlockOptionsDict -> Html Msg
instrumentRow model instrName beatBlockOptDict = 
  div 
    [] 
    (blockView model instrName beatBlockOptDict)

blockView : Model -> String -> BeatBlockOptionsDict -> List (Html Msg)
blockView model instrName beatBlockOptDict = 
  (Dict.toList beatBlockOptDict) |> List.concatMap (\ib -> (blockButton model 
                                                                        instrName 
                                                                        (Tuple.first ib) 
                                                                        (Tuple.second ib).block.blockName 
                                                                        (Tuple.second ib).ghostNotes
                                                                        (Tuple.second ib).accentPattern))

blockButton : Model -> String -> Int -> String -> GhostNotes -> Bits ->List (Html Msg)
blockButton model instrName index blockName ghostNotes accentPattern =
  let
    blockOptionsOpenParams =  if model.blockOptionsParams.beat == -1 then
                                BlockOptionsParams 1 index instrName blockName ghostNotes accentPattern
                              else
                                model.blockOptionsParams
  in
  [Html.select [onInputSelectChange BlockSelectedChange
              , HA.id (instrName ++ "~" ++ (String.fromInt index))
              , HA.class "instrumentBlockSelect"
              , HA.alt "Block Picker"
              , HA.title "Block Picker"
              ]
              (getBlockOptions blockName)]
  ++ [Html.button [HA.id ("blockOpt~" ++ instrName ++ "~" ++ String.fromInt index)
                 , HA.class "instrumentBlockOpts"
                 , HA.alt "Block Options"
                 , HA.title "Block Options"
                 , onClick (BlockOptionsDialogOpen blockOptionsOpenParams)
                 ] [Html.img [HA.src "assets/images/options.svg"
                              , HA.class "instrumentBlockOptsImg"] []]]

getBlockOptions : String -> List (Html Msg)
getBlockOptions blockName = 
  let
    quarterBlocks = (Dict.keys blockDict) |> List.filter (\k -> k <= "P")
    tripletBlocks = (Dict.keys blockDict) |> List.filter (\k -> k > "P")
  in
--  quarterBlocks |> List.map (\k -> (Html.option [if blockName == k then selected True else selected False] [Html.text k]))
  --Html.optgroup [HA.class "quarterBlocksOptGroup"] (quarterBlocks |> List.map (\k -> (Html.option [if blockName == k then selected True else selected False] [Html.text k])))
  --:: [Html.optgroup [HA.class "tripletBlocksOptGroup"] ((tripletBlocks) |> List.map (\k -> (Html.option [if blockName == k then selected True else selected False] [Html.text k])))]
  (quarterBlocks |> List.map (\k -> (Html.option [if blockName == k then selected True else selected False
                                                  , HA.class "blockSelect"] [Html.text k])))
  ++ (tripletBlocks |> List.map (\k -> (Html.option [if blockName == k then selected True else selected False] [Html.text k])))


stave : List (Svg Msg)
stave =
    (staveLines)
        |> List.map
            (\n ->
                Svg.path
                    [ strokeWidth "0.3"
                    , stroke "black"
                    , d ("M 0 " ++ String.fromFloat (n + staveShiftY) ++ " L 195 " ++ String.fromFloat (n + staveShiftY))
                    ]
                    []
            )

staveTimeSignature : Model -> List (Svg Msg)
staveTimeSignature model = 
  case model.timeSignature of
      "4/4" ->  [Svg.image [xlinkHref "assets/images/Timesignature4-4.svg"
                        , Svg.Attributes.width "18"
                        , Svg.Attributes.height "18"
                        , Svg.Attributes.x "3"
                        , Svg.Attributes.y "17.4"] [] ]
      _ -> []
  
percussionClef : List(Svg Msg)
percussionClef =
  [Svg.path
      [ strokeWidth "1.8"
      , stroke "black"
      , d ("M 3 " ++ String.fromFloat (3 + staveShiftY) ++ " L 3 " ++ String.fromFloat (9 + staveShiftY))
      ]
      []
  ,Svg.path
      [ strokeWidth "1.8"
      , stroke "black"
      , d ("M 6 " ++ String.fromFloat (3 + staveShiftY) ++ " L 6 " ++ String.fromFloat (9 + staveShiftY))
      ]
      []
  ]

singleBarLine : List(Svg Msg)
singleBarLine =
  [Svg.path
      [ strokeWidth "0.2"
      , stroke "black"
      , d ("M 195 " ++ String.fromFloat (staveShiftY) ++ " L 195 " ++ String.fromFloat (12 + staveShiftY))
      ]
      [] 
  ]
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

renderBar : InstrumentBlocksDict -> List(Svg Msg)
renderBar instrumentBlocks = 
  let
    beatCount = List.range 1 4
  in
  --loop through each beat of the bar
  (beatCount) |> List.concatMap (\beat -> buildNoteSubBeats beat instrumentBlocks)
              --|> Debug.toString


buildNoteSubBeats : Int -> InstrumentBlocksDict -> List(Svg Msg)
buildNoteSubBeats beat instrumentBlocks = 
  let
      subBeats = [1, 4, 5, 7, 9, 10]
      --get alll instruments & blocks for the current beat
      beatBlocks = (Dict.toList instrumentBlocks) |> List.map (\ib -> let
                                                                        instrName = Tuple.first ib
                                                                        blockOptions = Tuple.second ib
                                                                      in
                                                                      Tuple.pair instrName (Dict.get beat blockOptions))
      noteSubBeats = (subBeats) |> List.concatMap  (\sb -> getNoteSubBeats sb beatBlocks)
                     |> updateNoteSubBeats
 
  in
  (noteSubBeats) |> List.concatMap (\nsb -> renderNote beat nsb)
  --(Debug.toString noteSubBeats) ++ " BEAT " ++ String.fromInt beat


getNoteSubBeats : Int -> List (String, Maybe BlockOptions) -> List NoteSubBeat
getNoteSubBeats subBeat beatBlockOptions = 
  (beatBlockOptions) |> List.concatMap (\bb -> let
                                                    isPlayed = case Tuple.second bb of
                                                                  Just blockOption -> isSubBeatMatch subBeat blockOption.block
                                                                  _                -> False
                                                    subDivision = case Tuple.second bb of
                                                                    Just blockOption -> blockOption.block.subdivision
                                                                    _ -> "4-16"
                                                    ghostNotes = case Tuple.second bb of
                                                                    Just blockOption -> blockOption.ghostNotes
                                                                    _ -> NoGhostNotes
                                                    adjSubBeat =  if subDivision == "4-16" then
                                                                    (subBeat + 2) // 3
                                                                  else
                                                                    (subBeat + 3) // 4

                                                    isAccented = case Tuple.second bb of
                                                                    Just blockOption -> if subDivision == "4-16" then 
                                                                                          Binary.toDecimal  (Binary.and blockOption.accentPattern 
                                                                                                                        (Binary.fromDecimal (2 ^ (4-adjSubBeat)))
                                                                                                            ) /= 0
                                                                                        else 
                                                                                          Binary.toDecimal  (Binary.and blockOption.accentPattern 
                                                                                                                        (Binary.fromDecimal (2 ^ (3-adjSubBeat)))
                                                                                                            ) /= 0
                                                                    _ -> False
                                                in
                                          if isPlayed == True then 
                                            [NoteSubBeat subBeat (Tuple.first bb) Crotchet False False subDivision 0 subBeat Crotchet subBeat NoGhostNotes isAccented]
                                          else if ghostNotes == HasGhostNotes 
                                                  && ((subDivision == "4-16" && List.member subBeat [1,4,7,10])
                                                      || (subDivision == "3-8" && List.member subBeat [1,5,9])) then
                                            [NoteSubBeat subBeat (Tuple.first bb) Crotchet False False subDivision 0 subBeat Crotchet subBeat HasGhostNotes False]
                                          else
                                            []
                            )


processNoteSubBeats : Int -> List NoteSubBeat ->List(Svg Msg)
processNoteSubBeats beat noteSubBeats = 
  List.append ((noteSubBeats) |> List.concatMap (\nsb -> renderNote beat nsb)) 
              []


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
                                            [NoteSubBeat 1 "Rest" Crotchet False True "4-16" 0 1 Crotchet 1 NoGhostNotes False]) updateStalks

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
                        _ -> 20) - 8
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
                                                  nsb.ghostNotes
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
                                                                    (Tuple.first x).ghostNotes 
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
                                                      nsb.ghostNotes
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

renderNote : Int -> NoteSubBeat -> List (Svg Msg)
renderNote beat noteSubBeat = 
  let
    instrument = Dict.get noteSubBeat.instrumentName instrumentDict
    noteCenterX = 20.0 + ((toFloat (((beat - 1) * 12) + (noteSubBeat.subBeat - 1))) * 3.6)
    nextNoteCenterX = 20.0 + ((toFloat (((beat - 1) * 12) + (noteSubBeat.nextSubBeat - 1))) * 3.6)
    noteCenterY = case instrument of
                    Just instr -> instr.stavePosition + staveShiftY
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
                  , d ("M " ++ (String.fromFloat (noteCenterX + 1.65)) ++ " " ++ (String.fromFloat (noteSubBeat.stalkHeight + staveShiftY)) ++ " L " ++ (String.fromFloat (noteCenterX + 1.65)) ++ " " ++ String.fromFloat (noteCenterY + 1.2))
                  ]
                  []]
              else
                [Svg.path
                  [ strokeWidth "0.3"
                  , stroke "black"
                  , d ("M " ++ (String.fromFloat (noteCenterX + 1.65)) ++ " " ++ (String.fromFloat (noteSubBeat.stalkHeight + staveShiftY)) ++ " L " ++ (String.fromFloat (noteCenterX + 1.65)) ++ " " ++ String.fromFloat noteCenterY)
                  ]
                  []]
    dot = if noteSubBeat.isDotted then
            [Svg.circle 
              [cx (String.fromFloat (noteCenterX + 2.6))
               , cy (String.fromFloat noteCenterY)
               , r "0.4"
              ] []]
          else []  

    topBeam = if noteSubBeat.subBeat == noteSubBeat.nextSubBeat 
                 || noteSubBeat.isRest then []
              else [Svg.path 
                      [ strokeWidth "0.8"
                      , stroke "black"
                      , d ("M " ++ (String.fromFloat (noteCenterX + 1.5)) ++ " " ++ (String.fromFloat (noteSubBeat.stalkHeight + staveShiftY)) ++ " L " ++ (String.fromFloat (nextNoteCenterX + 1.8)) ++ " " ++ (String.fromFloat (noteSubBeat.stalkHeight + staveShiftY)))
                      ] []]
    
    semiQuaverBeam =  if noteSubBeat.subdivision == "4-16" then
                        if noteSubBeat.noteDuration == SemiQuaver 
                          && Basics.not noteSubBeat.isRest then 
                          if noteSubBeat.subBeat == noteSubBeat.nextSubBeat then --last (or only) note in the beat
                            if noteSubBeat.subBeat == 10 then --last subBeat position 
                              if noteSubBeat.prevSubBeat /= noteSubBeat.subBeat then --short semi quaver bar goes to the left
                                [Svg.path 
                                  [ strokeWidth "0.8"
                                  , stroke "black"
                                  , d ("M " ++ (String.fromFloat (noteCenterX + 1.5)) ++ " " ++ (String.fromFloat (noteSubBeat.stalkHeight + staveShiftY + 1.6)) ++ " L " ++ (String.fromFloat (noteCenterX - 0.4)) ++ " " ++ (String.fromFloat (noteSubBeat.stalkHeight + staveShiftY + 1.6)))
                                  ] []
                                ]
                              else  --single semiquaver, no beam
                                [Svg.image [xlinkHref "assets/images/semiquaver.svg"
                                            , Svg.Attributes.width "6"
                                            , Svg.Attributes.height "6"
                                            , Svg.Attributes.x (String.fromFloat (noteCenterX - 0.4))
                                            , Svg.Attributes.y (String.fromFloat (noteSubBeat.stalkHeight + staveShiftY - 0.6))] []]
                            else --short semi quaver bar goes to the right
                                [Svg.path 
                                  [ strokeWidth "0.8"
                                  , stroke "black"
                                  , d ("M " ++ (String.fromFloat (noteCenterX + 1.5)) ++ " " ++ (String.fromFloat (noteSubBeat.stalkHeight + staveShiftY + 1.6)) ++ " L " ++ (String.fromFloat (noteCenterX + 3.5)) ++ " " ++ (String.fromFloat (noteSubBeat.stalkHeight + staveShiftY + 1.6)))
                                  ] []
                                ]
                          else if noteSubBeat.nextSubBeatNoteDuration == SemiQuaver then --full semi quaver bar goes to next note
                                  [Svg.path 
                                    [ strokeWidth "0.8"
                                    , stroke "black"
                                    , d ("M " ++ (String.fromFloat (noteCenterX + 1.5)) ++ " " ++ (String.fromFloat (noteSubBeat.stalkHeight + staveShiftY + 1.6)) ++ " L " ++ (String.fromFloat (nextNoteCenterX + 1.8)) ++ " " ++ (String.fromFloat (noteSubBeat.stalkHeight + staveShiftY + 1.6)))
                                    ] []
                                ]
                              else --short semi quaver bar goes to the right
                                  if noteSubBeat.subBeat /= noteSubBeat.nextSubBeat
                                    && noteSubBeat.subBeat /= noteSubBeat.prevSubBeat then
                                      [] --this handles the K block issue
                                  else
                                    [Svg.path 
                                      [ strokeWidth "0.8"
                                      , stroke "black"
                                      , d ("M " ++ (String.fromFloat (noteCenterX + 1.5)) ++ " " ++ (String.fromFloat (noteSubBeat.stalkHeight + staveShiftY + 1.6)) ++ " L " ++ (String.fromFloat (noteCenterX + 3.5)) ++ " " ++ (String.fromFloat (noteSubBeat.stalkHeight + staveShiftY + 1.6)))
                                      ] []
                                    ]
                        else if noteSubBeat.noteDuration == Quaver 
                                && Basics.not noteSubBeat.isRest
                                && noteSubBeat.subBeat == noteSubBeat.prevSubBeat 
                                && noteSubBeat.subBeat == noteSubBeat.nextSubBeat then --single quaver, no beam
                                [Svg.image [xlinkHref "assets/images/quaver.svg"
                                            , Svg.Attributes.width "6"
                                            , Svg.Attributes.height "6"
                                            , Svg.Attributes.x (String.fromFloat (noteCenterX - 0.4))
                                            , Svg.Attributes.y (String.fromFloat (noteSubBeat.stalkHeight + staveShiftY - 0.6))] []]
                            else []
                      else []
    ghostNote = if noteSubBeat.ghostNotes == HasGhostNotes then
                    [Svg.path 
                      [ strokeWidth "0.2"
                      , stroke "black"
                      , d ("M " ++ (String.fromFloat (noteCenterX - 2.0)) ++ " " ++ (String.fromFloat (noteCenterY - 2.0)) 
                                ++ "C "++ (String.fromFloat (noteCenterX - 2.5)) ++ " " ++ (String.fromFloat (noteCenterY - 1.0)) 
                                ++ " " ++ (String.fromFloat (noteCenterX - 2.5)) ++ " " ++ (String.fromFloat (noteCenterY + 1.0)) 
                                ++ " "++ (String.fromFloat (noteCenterX - 2.0)) ++ " " ++ (String.fromFloat (noteCenterY + 2.0)) 
                                )]
                      []
                    ,Svg.path 
                      [ strokeWidth "0.2"
                      , stroke "black"
                      , d ("M " ++ (String.fromFloat (noteCenterX + 2.2)) ++ " " ++ (String.fromFloat (noteCenterY - 2.0)) 
                                ++ " C "++ (String.fromFloat (noteCenterX + 2.7)) ++ " " ++ (String.fromFloat (noteCenterY - 1.0)) 
                                ++ " " ++ (String.fromFloat (noteCenterX + 2.7)) ++ " " ++ (String.fromFloat (noteCenterY + 1.0)) 
                                ++ " "++ (String.fromFloat (noteCenterX + 2.2)) ++ " " ++ (String.fromFloat (noteCenterY + 2.0)) 
                                )]
                      []
                    ]
                  else []
    accent = if noteSubBeat.isAccented then
                    [Svg.path 
                      [ strokeWidth "0.3"
                      , stroke "black"
                      , d ("M " ++ (String.fromFloat (noteCenterX - 1.5)) ++ " " ++ (String.fromFloat (noteSubBeat.stalkHeight + staveShiftY - 3.0)) 
                                ++ " L "++ (String.fromFloat (noteCenterX + 1.5)) ++ " " ++ (String.fromFloat (noteSubBeat.stalkHeight + staveShiftY - 2.0)) 
                                )]
                      []
                    ,Svg.path 
                      [ strokeWidth "0.3"
                      , stroke "black"
                      , d ("M " ++ (String.fromFloat (noteCenterX + 1.5)) ++ " " ++ (String.fromFloat (noteSubBeat.stalkHeight + staveShiftY - 2.0)) 
                                ++ " L "++ (String.fromFloat (noteCenterX - 1.5)) ++ " " ++ (String.fromFloat (noteSubBeat.stalkHeight + staveShiftY - 1.0)) 
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
              , rx "1.85"
              , ry "1.3"
              , transform ("rotate(-20, " ++ (String.fromFloat noteCenterX) ++ ", " ++ (String.fromFloat noteCenterY) ++ ")")
            ] []
          ]
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
      Rest ->
          case noteSubBeat.noteDuration of
              Crotchet ->
                      [Svg.image [xlinkHref "assets/images/crotchet-rest.svg"
                                  , Svg.Attributes.width "7"
                                  , Svg.Attributes.height "7"
                                  , Svg.Attributes.x (String.fromFloat (noteCenterX - 4))
                                  , Svg.Attributes.y (String.fromFloat (noteCenterY - 4))] [] ]
              Quaver ->
                      [Svg.image [xlinkHref "assets/images/quaver-rest.svg"
                                  , Svg.Attributes.width "6"
                                  , Svg.Attributes.height "6"
                                  , Svg.Attributes.x (String.fromFloat (noteCenterX - 3))
                                  , Svg.Attributes.y (String.fromFloat (noteCenterY - 4))] [] ]
              SemiQuaver ->
                      [Svg.image [xlinkHref "assets/images/semiquaver-rest.svg"
                                  , Svg.Attributes.width "6"
                                  , Svg.Attributes.height "6"
                                  , Svg.Attributes.x (String.fromFloat (noteCenterX - 3))
                                  , Svg.Attributes.y (String.fromFloat (noteCenterY - 4))] [] ]
              _ -> []
          )                                    
      ++ stalk
      ++ dot
      ++ topBeam
      ++ semiQuaverBeam
      ++ ghostNote
      ++ accent


renderBeams : Int -> List NoteSubBeat -> List (Svg Msg)
renderBeams beat noteSubBeats = 
  []

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

  
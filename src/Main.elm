port module Main exposing (main)

import Browser
import Browser.Navigation as Nav
import Browser.Events exposing (onKeyDown)
import Html exposing (..)
import Html.Attributes exposing (..)
import Html.Attributes as HA
import Html.Events exposing (onClick)
import Url
import Binary exposing (..)
import Dict exposing (..)
import Svg exposing (..)
import Svg.Attributes exposing (class, viewBox)
import File exposing (..)
import File.Select as Select
import File.Download as Download
import Task exposing (..)
import Json.Encode as JsonE
import Json.Decode as JsonD


import CommonModel exposing (..)
import CommonEvents exposing (..)
import Common exposing (..)
import Stave exposing (..)
import File.Download as Download

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



beatOptionsDialog : String -> List (Html msg) -> Html msg
beatOptionsDialog dialogId content =
    Html.node "dialog" [ HA.id dialogId ] content

port toggleDialog : String -> Cmd msg



-- INIT



init : () -> Url.Url -> Nav.Key -> ( Model, Cmd Msg )
init flags url key =
  let 
      model = initialModel
  in
  ( model, Cmd.none )


initialModel : Model
initialModel = 
    {bars = Dict.fromList [(1, Bar (Dict.fromList [("Hi-Hat", (Dict.fromList [(1, aBlock), (2, aBlock), (3, aBlock), (4, aBlock)]))
                                                 , ("Snare", (Dict.fromList [(1, pBlock), (2, aBlock), (3, pBlock), (4, aBlock)]))
                                                 , ("Bass Drum", (Dict.fromList [(1, aBlock), (2, pBlock), (3, aBlock), (4, pBlock)]))
                                                 ])
                                  Dict.empty
                                  "4/4")
                          ,(2, Bar (Dict.fromList [("Hi-Hat", (Dict.fromList [(1, aBlock), (2, aBlock), (3, aBlock), (4, aBlock)]))
                                                 , ("Snare", (Dict.fromList [(1, aBlock), (2, aBlock), (3, aBlock), (4, aBlock)]))
                                                 , ("Bass Drum", (Dict.fromList [(1, aBlock), (2, aBlock), (3, aBlock), (4, pBlock)]))
                                                 ])
                                  Dict.empty
                                  "4/4")
                          ]
    , barOptionsParams = Nothing
    , beatOptionsParams = Nothing
    , debugText = ""
    }   


-- UPDATE



update : Msg -> Model -> ( Model, Cmd Msg )
update msg model = 
    case msg of
{-
        BlockSelectedChange param -> applyBlockSelectedChange model param
        AddInstrumentSelectedChange param -> addInstrument model param
        BeatOptionsDialogOpen params -> ({model | beatOptionsParams = Just params
                                                    ,  debugText = ""}, toggleDialog "beat-options-dialog")
        BeatOptionsDialogSave -> ({model | beatOptions = updateBeatOptions model
                                           , beatOptionsParams = Nothing
                                           --, debugText = (Debug.toString (updateBeatOptions model))
                                            }, toggleDialog "beat-options-dialog")
        BeatOptionsDialogCancel -> ({model | beatOptionsParams = Nothing}, toggleDialog "beat-options-dialog")
        KeyPressedMsg keyEventMsg -> case keyEventMsg of
                                        KeyEventUnknown key-> if key == "Escape" then 
                                                                ({model | beatOptionsParams = Nothing}, toggleDialog "beat-options-dialog")
                                                              else 
                                                                (model, Cmd.none)
                                        _ -> (model, Cmd.none)
        GhostCheckBoxChanged param ->  let 
                                              opts = case model.beatOptionsParams of
                                                  Just bOptParams -> Just (BeatOptionsParams bOptParams.beat
                                                                                      (BeatOptions (updateEmbellishmentPattern param bOptParams.beatOptions.ghostNotes)
                                                                                                   bOptParams.beatOptions.accents
                                                                                      )
                                                                          )
                                                  _ -> Nothing
                                        in
                                        ({model | beatOptionsParams = opts}, Cmd.none)
        AccentCheckBoxChanged param ->  let 
                                              opts = case model.beatOptionsParams of
                                                  Just bOptParams -> Just (BeatOptionsParams bOptParams.beat
                                                                                      (BeatOptions bOptParams.beatOptions.ghostNotes
                                                                                                   (updateEmbellishmentPattern param bOptParams.beatOptions.accents)
                                                                                      )
                                                                          )
                                                  _ -> Nothing
                                        in
                                        ({model | beatOptionsParams = opts}, Cmd.none)
        PatternSave -> ({model | debugText = ""}, Download.string "drum_pattern.json" "application/json" (getPatternJson model))
        PatternLoad -> ({model | debugText = ""}, Select.file ["application/json"] UploadSelected)
        UploadSelected file -> (model, Task.perform FileLoaded (File.toString file))
        FileLoaded param -> (updateModelFromFile model param, Cmd.none)
-}
        _ -> ({model | debugText = ""}, Cmd.none)

{-
updateModelFromFile : Model -> String -> Model
updateModelFromFile model param = 
  let
    beatOptionsDict = case JsonD.decodeString (JsonD.field "beatOptions" (JsonD.list beatOptionDecoder)) param of 
                        Ok val -> let
                                      x = (val) |> List.map (\bo -> (bo.beat, (BeatOptions (Binary.fromDecimal bo.ghostNotes)(Binary.fromDecimal bo.accents))))
                                  in
                                  Dict.fromList x
                        Err val -> model.beatOptions
    newArrangement = case JsonD.decodeString (JsonD.field "arrangement" (JsonD.list arrangementDecoder)) param of 
                        Ok val -> let
                                      x = (val) |> List.map (\arr -> (arr.instrumentName, (buildBeatBlockDict arr.blocks)))
                                  in
                                  Dict.fromList x
                        Err val -> model.arrangement
  in
  {model | beatOptions = beatOptionsDict
           , arrangement = newArrangement
           , debugText = Debug.toString beatOptionsDict}


buildBeatBlockDict : List BeatBlockJson -> BeatBlockDict
buildBeatBlockDict beatBlocks = 
  List.foldl (\bb -> let
                      block = case Dict.get bb.blockName blockDict of
                                Just blok -> blok
                                _ -> pBlock
                     in 
                     Dict.insert bb.beat block) Dict.empty beatBlocks

getPatternJson : Model -> String
getPatternJson model = 
  let
    beatOpts = (Dict.toList model.beatOptions) |> List.map (\bo ->  BeatOptionJson  (Tuple.first bo)
                                                                                    (Binary.toDecimal ((Tuple.second bo).ghostNotes))
                                                                                    (Binary.toDecimal ((Tuple.second bo).accents))
                                                                  )
    arrangement = (Dict.toList model.arrangement) |> List.map (\arr ->  ArrangementJson (Tuple.first arr)
                                                                                        (Dict.keys (Tuple.second arr) |> List.map (\b -> BeatBlockJson b (case Dict.get b (Tuple.second arr) of
                                                                                                                                                          Just block -> block.blockName
                                                                                                                                                          _ -> "P")))
                                                                  )
  in
  JsonE.encode 1 (JsonE.object ([("arrangement", JsonE.list (\arr -> JsonE.object [("instrument", JsonE.string arr.instrumentName)
                                                                                  , ("blocks", JsonE.list (\b -> JsonE.object [("beat", JsonE.int b.beat)
                                                                                                                              ,("blockName", JsonE.string b.blockName)]
                                                                                                          ) arr.blocks)]) arrangement)
                               ,("beatOptions", JsonE.list (\bo -> JsonE.object [("beat", JsonE.int bo.beat)
                                                                                , ("ghostNotes", JsonE.int bo.ghostNotes)
                                                                                , ("accents", JsonE.int bo.accents)]) beatOpts)])
                  )
-}

applyBlockSelectedChange : Model -> SelectIdValue -> ( Model, Cmd Msg )
applyBlockSelectedChange model param = 
  let
    idList = String.split "~" param.id
    barNo = case List.head idList of
              Just str -> case String.toInt str of
                            Just int -> int
                            _ -> -1
              _ -> -1
    instrName = List.head (List.drop 1 idList)
    blockIndex = case List.head (List.reverse idList) of
                  Just index -> index
                  _ -> ""
    bar = Dict.get barNo model.bars
  in
  case bar of 
    Just currBar ->
        case instrName of
            Just iName -> case Dict.get iName currBar.arrangement of
                            Just blockOptsDict ->
                                case String.toInt blockIndex of
                                  Just bIndex -> ({model | bars = (Dict.insert barNo (updateBarArrangement iName bIndex param.value currBar) model.bars)}, Cmd.none)
                                  _           -> ({model | debugText = (Debug.toString value)}, Cmd.none)
                            _ -> ({model | debugText = (Debug.toString value)}, Cmd.none)
            _ -> ({model | debugText = (Debug.toString value)}, Cmd.none)
    _ -> ({model | debugText = (Debug.toString value)}, Cmd.none)


updateBarArrangement : String -> Int -> String -> Bar -> Bar
updateBarArrangement instrName blockIndex newBlockName currBar =
  let
    newBlock = Dict.get newBlockName blockDict
    currArrangement = currBar.arrangement
  in
  case newBlock of
      Just nBlock -> 
          case Dict.get instrName currArrangement of
              Just beatBlockDict -> Bar (Dict.insert instrName (Dict.insert blockIndex nBlock beatBlockDict) currArrangement)
                                        currBar.beatOptions
                                        currBar.timeSignature
              _ -> currBar
      _ -> currBar
{-
addInstrument : Model -> SelectIdValue -> ( Model, Cmd Msg )
addInstrument model param = 
  case Dict.get param.value instrumentDict of
      Just instr -> ({model |arrangement = Dict.insert param.value (Dict.fromList [(1, pBlock), (2, pBlock), (3, pBlock), (4, pBlock)]) model.arrangement}, Cmd.none)
      _ -> ({model | debugText = (Debug.toString value)}, Cmd.none)

updateBeatOptions : Model -> BeatOptionsDict
updateBeatOptions model = 
  case model.beatOptionsParams of
    Just bOptParams -> Dict.insert bOptParams.beat bOptParams.beatOptions model.beatOptions
    _ ->  model.beatOptions

updateEmbellishmentPattern : CheckboxIdChecked -> Bits -> Bits
updateEmbellishmentPattern param currPattern =
  case String.toInt (String.right 1 param.id) of
    Just subBeat -> let
                      bitmap =  if subBeat == 1 then Binary.fromIntegers [1,0,0,0]
                                else if subBeat == 2 then Binary.fromIntegers [0,1,0,0]
                                else if subBeat == 3 then Binary.fromIntegers [0,0,1,0]
                                else Binary.fromIntegers [0,0,0,1]
                    in
                    if param.checked then
                      Binary.or currPattern bitmap
                    else
                      Binary.and currPattern (Binary.not bitmap)
    _ -> currPattern

-}

-- SUBSCRIPTIONS


subscriptions : Model -> Sub Msg
subscriptions model =
    Sub.batch
        [ Browser.Events.onKeyDown keyPressedDecoder
        , Browser.Events.onKeyUp keyReleasedDecoder
        ]
                    
-- VIEW


view : Model -> Browser.Document Msg
view model =
  { title = "Drum Blocks"
  , body =
      [header [][]
       , section [HA.class "flex-container"]
           [div [HA.id "sidebar_left"
                , HA.style "order" "1"]
                [button [ onClick PatternSave
                        , HA.class "patternButton"
                        ]
                        [ Html.text "Save Pattern" ]
                , button [ onClick PatternLoad
                         , HA.class "patternButton" 
                         ] 
                         [ Html.text "Load Pattern" ]
                ]
            ,div [HA.id "main"]
                ((Html.table
                    [] 
                    (Html.tr  [HA.class "instrumentTableHeaderRow"] 
                              [th [HA.class "instrumentTableHeaderCell"] 
                                  [Html.text "Instrument"]
                              ,td [HA.rowspan ((List.length (getIncludedInstrumentNames model.bars))+1)]
                                  [displayBars model.bars]
                              ]
                    :: (displayInstruments model.bars)
                    ++ [Html.tr 
                              [HA.class "instrumentTableRow"] 
                              [td [HA.class "instrumentTableCell"] 
                                  [Html.select  [onInputSelectChange AddInstrumentSelectedChange
                                                , HA.alt "Add New Instrument"
                                                , HA.title "Add New Instrument"
                                                , HA.class "addInstrumentTableCell"
                                                ]
                                                (Html.option [selected True ] [Html.text "Add Instrument"]
                                                :: (getAvailableInstruments model))
                                  ]
                              ]
                        ])
                 ) :: (renderStaveBars model)
                  ++[ Html.text model.debugText
                      ,beatOptionsDialog "beat-options-dialog"
                            (buildBeatOptionsDialog model
                            ++  [Html.div [HA.class "subBeatOptionsDialogButtons"] 
                                          [button [ onClick BeatOptionsDialogSave, HA.class "subBeatOptionsDialogButton", HA.id "bb" ] [ Html.text "Save" ]
                                          , button [ onClick BeatOptionsDialogCancel, HA.class "subBeatOptionsDialogButton" ] [ Html.text "Cancel" ]
                                          ]
                                ]
                            )
                  ]
                )
           ]
      ]
  }

getAvailableInstruments : Model -> List (Html Msg)
getAvailableInstruments model =
  List.map (\i -> Html.option [] [Html.text i]) <| List.filter (\i -> List.member i (getIncludedInstrumentNames model.bars) == False  
                                                                                     && i /= "Rest") (Dict.keys instrumentDict) 

getIncludedInstrumentNames : BarDict -> List String
getIncludedInstrumentNames bars = 
  case (Dict.get 1 bars) of
    Just bar -> Dict.keys bar.arrangement
    _ -> []    

displayBars : BarDict -> Html Msg
displayBars bars = 
  div [HA.id "bars", HA.class "flex-container"]
      ((Dict.toList bars)
          |> List.map (\bar -> table  []
                                      ((tr []
                                          [th [HA.class "instrumentTableHeaderCell"
                                              , colspan 4] 
                                              [div [HA.class "flex-container", HA.id "bar-header"] 
                                                  [div[HA.id "bar-text"][Html.text ("Bar " ++ (String.fromInt (Tuple.first bar)))]
                                                  ,div [HA.id "bar-buttons"] 
                                                        [button [ onClick BeatOptionsDialogCancel, HA.class "barButton" ] 
                                                                [ Html.img [HA.src "assets/images/settings.svg", HA.class "barButtonImg"] []]
                                                        , button [ onClick BarAdd, HA.class "barButton" ] 
                                                                [ Html.img [HA.src "assets/images/add.svg", HA.class "barButtonImg"] []]
                                                        ]
                                                  ]
                                              ]
                                          ] 
                                       ) :: (instrumentView (Tuple.first bar) (Tuple.second bar))
                                         ++ ((List.range 1 4) |> 
                                              List.map (\beat -> td [HA.class "optionsTableCell"] 
                                                                    [Html.button [HA.id ("beatOpts~"  ++ (String.fromInt (Tuple.first bar)) ++ "~" ++ (String.fromInt beat))
                                                                                , HA.alt "Beat Options"
                                                                                , HA.title "Beat Options"
                                                                                , onClick (BeatOptionsDialogOpen (BeatOptionsParams beat (case Dict.get beat (Tuple.second bar).beatOptions of
                                                                                                                                            Just beatOpts -> beatOpts
                                                                                                                                            _ -> BeatOptions Binary.empty Binary.empty)))
                                                                                ] [Html.img [HA.src "assets/images/options-horizontal.svg"
                                                                                              , HA.class "instrumentBlockOptsImg"] []]
                                                                    ]))                                     )
                      )
      )


instrumentView : Int -> Bar -> List (Html Msg)
instrumentView barNo bar = 
  (Dict.toList bar.arrangement) |> List.map (\item  ->  let
                                                            instrName = (Tuple.first item)
                                                            sortOrder = case Dict.get instrName instrumentDict of
                                                                          Just instr -> instr.sortOrder
                                                                          _ -> 100
                                                          in
                                                          {instrName = instrName, sortOrder = sortOrder})
                                  |> List.sortBy .sortOrder
                                  |> List.map (\a -> tr [Html.Attributes.class "instrumentTableRow"]
                                                               (instrumentRow barNo a.instrName bar.arrangement)
                                                              )

instrumentRow : Int -> String -> InstrumentBlocksDict -> List (Html Msg)
instrumentRow barNo instrName instrBlock = 
  case Dict.get instrName instrBlock of
      Just beatBlockDict -> 
          (Dict.toList beatBlockDict) |> List.map (\ib -> (blockButton barNo 
                                                                       instrName 
                                                                       (Tuple.first ib) 
                                                                       (Tuple.second ib).blockName))
      _ -> []


blockButton : Int -> String -> Int -> String ->Html Msg
blockButton barNo instrName index blockName =
  td [] [Html.select [onInputSelectChange BlockSelectedChange
              , HA.id ((String.fromInt barNo) ++ "~" ++ instrName ++ "~" ++ (String.fromInt index))
              , HA.class "instrumentBlockSelect"
              , HA.alt "Block Picker"
              , HA.title "Block Picker"
              ]
              (getBlockOptions blockName)
        ]
  

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


buildBeatOptionsDialog : Model -> List (Html Msg)
buildBeatOptionsDialog model =
  []
{-
  let
    beat = case model.beatOptionsParams of
            Just params -> params.beat
            _ -> 0
    isGhostable = (Dict.keys model.arrangement) |> List.foldl (\i isGhable -> case Dict.get i instrumentDict of
                                                                                    Just instr -> isGhable || instr.isGhostable
                                                                                    _ -> isGhable) False 
    isAccentable = (Dict.keys model.arrangement) |> List.foldl (\i isAccable -> case Dict.get i instrumentDict of
                                                                                    Just instr -> isAccable || instr.isAccentable
                                                                                    _ -> isAccable) False 
  in
  [div []
            [svg
                [ Svg.Attributes.width "100"
                , Svg.Attributes.height "40"
                , Svg.Attributes.class "stave"
                ]
                []--(stave ++ (renderBar [beat] model.arrangement model.beatOptions))
            ]
  ,div  []
        ((if isGhostable then 
            [Html.div [HA.class "blockOptionsContainer"] 
                      [Html.text "Ghost Notes"
                      , Html.div [HA.class "embellishPatternBox"]
                                  (renderGhostCheckboxes model beat)
                      ]
            ]
          else []
          )
      ++ (if isAccentable then 
            [Html.div [HA.class "blockOptionsContainer"] 
                      [Html.text "Accents"
                      , Html.div [HA.class "embellishPatternBox"]
                                  (renderAccentCheckboxes model beat)
                      ]
            ]
          else []
          ))
  ]
-}

{-
displayAccentCheckboxes : Model -> Int -> List (Html Msg)
displayAccentCheckboxes model beat =
  let
    subBeatRange = if "4-16" == "4-16" then [{index = 1, bitmap = (Binary.fromIntegers [1,0,0,0])}
                                                                        , {index = 2, bitmap = (Binary.fromIntegers [0,1,0,0])}
                                                                        , {index = 3, bitmap = (Binary.fromIntegers [0,0,1,0])}
                                                                        , {index = 4, bitmap = (Binary.fromIntegers [0,0,0,1])}] 
                                                                   else [{index = 1, bitmap = (Binary.fromIntegers [1,0,0])}
                                                                        , {index = 2, bitmap = (Binary.fromIntegers [0,1,0])}
                                                                        , {index = 3, bitmap = (Binary.fromIntegers [0,0,1])}]
    accentableSubBeats = (Dict.toList model.arrangement)  |> List.map (\i -> Tuple.pair (Dict.get (Tuple.first i) instrumentDict) (Dict.get beat (Tuple.second i)) )
                                                          |> List.map (\ib -> case (Tuple.first ib) of
                                                                                Just instrument ->  if instrument.isAccentable then
                                                                                                      case (Tuple.second ib) of 
                                                                                                          Just block -> block.notePlacement
                                                                                                          _ -> (Binary.fromIntegers [0,0,0,0])
                                                                                                    else 
                                                                                                      (Binary.fromIntegers [0,0,0,0])
                                                                                _ -> (Binary.fromIntegers [0,0,0,0]))
                                                          |> List.foldl (Binary.or) (Binary.fromIntegers [0,0,0,0])
    accentPattern = case model.beatOptionsParams of
                      Just beatOpts -> beatOpts.beatOptions.accents
                      _ -> Binary.fromIntegers [0,0,0,0]

  in
  (subBeatRange |> List.map (\i -> Html.input [HA.type_ "checkbox"
                                             , HA.id ("accent_checkbox_" ++ String.fromInt i.index)
                                             , HA.class "embellishCheckbox"
                                             , onCheckboxChanged AccentCheckBoxChanged
                                             , checked (Binary.toDecimal (Binary.and i.bitmap accentPattern) /= 0)
                                             , HA.disabled (Binary.toDecimal (Binary.and i.bitmap accentableSubBeats) == 0)][]
                           )
  )--  ++ [Html.text (Debug.toString accentableSubBeats)]


displayGhostCheckboxes : Model -> Int -> List (Html Msg)
displayGhostCheckboxes model beat =
  let
    subBeatRange = if "4-16" == "4-16" then [{index = 1, bitmap = (Binary.fromIntegers [1,0,0,0])}
                                                                        , {index = 2, bitmap = (Binary.fromIntegers [0,1,0,0])}
                                                                        , {index = 3, bitmap = (Binary.fromIntegers [0,0,1,0])}
                                                                        , {index = 4, bitmap = (Binary.fromIntegers [0,0,0,1])}] 
                                                                   else [{index = 1, bitmap = (Binary.fromIntegers [1,0,0])}
                                                                        , {index = 2, bitmap = (Binary.fromIntegers [0,1,0])}
                                                                        , {index = 3, bitmap = (Binary.fromIntegers [0,0,1])}]
    ghostableSubBeats = (Dict.toList model.arrangement)   |> List.map (\i -> Tuple.pair (Dict.get (Tuple.first i) instrumentDict) (Dict.get beat (Tuple.second i)) )
                                                          |> List.map (\ib -> case (Tuple.first ib) of
                                                                                Just instrument ->  if instrument.isGhostable then
                                                                                                      case (Tuple.second ib) of 
                                                                                                          Just block -> block.notePlacement
                                                                                                          _ -> (Binary.fromIntegers [0,0,0,0])
                                                                                                    else 
                                                                                                      (Binary.fromIntegers [0,0,0,0])
                                                                                _ -> (Binary.fromIntegers [0,0,0,0]))
                                                          |> List.foldl (Binary.xor) (Binary.fromIntegers [1,1,1,1])
    ghostPattern = case model.beatOptionsParams of
                      Just beatOpts -> beatOpts.beatOptions.ghostNotes
                      _ -> Binary.fromIntegers [0,0,0,0]

  in
  (subBeatRange |> List.map (\i -> Html.input [HA.type_ "checkbox"
                                             , HA.id ("ghost_checkbox_" ++ String.fromInt i.index)
                                             , HA.class "embellishCheckbox"
                                             , onCheckboxChanged GhostCheckBoxChanged
                                             , checked (Binary.toDecimal (Binary.and i.bitmap ghostPattern) /= 0)
                                             , HA.disabled (Binary.toDecimal (Binary.and i.bitmap ghostableSubBeats) == 0)][]
                           )
  )--  ++ [Html.text (Debug.toString accentableSubBeats)]

-}



subdivisionOption : Subdivision -> Html Msg
subdivisionOption subdiv = 
    Html.option [] [Html.text subdiv.description]

displayInstruments : BarDict -> List (Html Msg)
displayInstruments bars = 
  (getIncludedInstrumentNames bars) |> List.map (\instrName  ->   let
                                                                    sortOrder = case Dict.get instrName instrumentDict of
                                                                          Just instr -> instr.sortOrder
                                                                          _ -> 100
                                                                  in
                                                                  {instrName = instrName, sortOrder = sortOrder})
                                  |> List.sortBy .sortOrder
                                  |> List.map (\a -> tr [Html.Attributes.class "instrumentTableRow"]
                                                        [td [Html.Attributes.class "instrumentTableCell"] [Html.text a.instrName]
                                                              ])

renderStaveBars : Model -> List (Html Msg)
renderStaveBars model = 
  (Dict.toList model.bars) 
    |> List.map (\bar -> 
                   div []
                        [svg
                            [ viewBox "0 0 200 20"
                            , Svg.Attributes.class "stave"
                            ]
                            (stave ++ percussionClef ++ (staveTimeSignature (Tuple.second bar)) ++ singleBarLine ++ (renderStaveBar (List.range 1 4) (Tuple.second bar))
                            )
                            --(stave ++ percussionClef)
                        ])

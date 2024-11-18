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



optionsDialog : String -> List (Html msg) -> Html msg
optionsDialog dialogId content =
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
        BlockSelectedChange param -> applyBlockSelectedChange model param
        AddInstrumentSelectedChange param -> addInstrument model param
        BeatOptionsDialogOpen params -> ({model | beatOptionsParams = Just params
                                                  --, debugText = ""
                                          }, toggleDialog "beat-options-dialog")
        BeatOptionsDialogSave -> ({model | bars = updateBeatOptions model
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
                                              Just bOptParams -> Just (BeatOptionsParams 
                                                                                  bOptParams.barNo
                                                                                  bOptParams.beat
                                                                                  (BeatOptions (updateEmbellishmentPattern param bOptParams.beatOptions.ghostNotes)
                                                                                                bOptParams.beatOptions.accents
                                                                                  )
                                                                      )
                                              _ -> Nothing
                                        in
                                        ({model | beatOptionsParams = opts}, Cmd.none)
        AccentCheckBoxChanged param ->  let 
                                          opts = case model.beatOptionsParams of
                                              Just bOptParams -> Just (BeatOptionsParams 
                                                                                  bOptParams.barNo
                                                                                  bOptParams.beat
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
        BarAdd barNo -> (addBar model barNo, Cmd.none)
        InstrumentDelete instrName -> (deleteInstrument model instrName, Cmd.none)
        _ -> ({model | debugText = ""}, Cmd.none)

updateModelFromFile : Model -> String -> Model
updateModelFromFile model param = 
  let
    barDict = case JsonD.decodeString (JsonD.field "bars" (JsonD.list barDecoder)) param of 
                Ok barVal -> (barVal) |> List.map (\bval -> (bval.barNo,(Bar ((bval.arrangement) |> List.map (\arr -> (arr.instrumentName, (buildBeatBlockDict arr.blocks)))
                                                                                                 |>Dict.fromList)
                                                                             ((bval.beatOptions) |> List.map (\bo -> (bo.beat, (BeatOptions (Binary.fromDecimal bo.ghostNotes)(Binary.fromDecimal bo.accents))))
                                                                                                 |> Dict.fromList)
                                                                             "4/4"
                                                                        )
                                                            )
                                                  )
                                      |> Dict.fromList 
                Err val -> model.bars


  in
  {model | bars = barDict
           , debugText = Debug.toString ""}



addBar : Model -> Int -> Model
addBar model barNo = 
  let
    shiftedBars = (Dict.keys model.bars)   |> List.filter (\i -> i > barNo)
                                           |> List.reverse
                                           |> List.foldl (\key dic -> case Dict.get key model.bars of
                                                                        Just bar -> Dict.insert (key + 1) bar dic
                                                                        _ -> dic) model.bars
    newBar = case Dict.get barNo model.bars of
                Just bar -> bar
                _ -> Bar (Dict.fromList [("Hi-Hat", (Dict.fromList [(1, aBlock), (2, aBlock), (3, aBlock), (4, aBlock)]))
                                                 , ("Snare", (Dict.fromList [(1, pBlock), (2, aBlock), (3, pBlock), (4, aBlock)]))
                                                 , ("Bass Drum", (Dict.fromList [(1, aBlock), (2, pBlock), (3, aBlock), (4, pBlock)]))
                                                 ])
                                  Dict.empty
                                  "4/4"
  in
  {model | bars = (Dict.insert (barNo + 1) newBar shiftedBars)}

deleteInstrument : Model -> String -> Model
deleteInstrument model instrName =
  let
    newBars = (Dict.toList model.bars) |> List.foldl (\x a -> let
                                                                oldBar = (Tuple.second x)
                                                                newBar = Bar (Dict.remove instrName oldBar.arrangement)
                                                                             oldBar.beatOptions
                                                                             oldBar.timeSignature
                                                              in
                                                              Dict.insert (Tuple.first x) newBar a) Dict.empty 
  in
  {model | bars = newBars}


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
    bars = (Dict.toList model.bars) |> List.map (\bar ->  let 
                                                            beatOpts = (Dict.toList (Tuple.second bar).beatOptions) |> List.map (\bo ->  BeatOptionJson  (Tuple.first bo)
                                                                                                                                            (Binary.toDecimal ((Tuple.second bo).ghostNotes))
                                                                                                                                            (Binary.toDecimal ((Tuple.second bo).accents)))
                                                            arrangement = (Dict.toList (Tuple.second bar).arrangement) |> List.map (\arr ->  ArrangementJson (Tuple.first arr)
                                                                                                                                                (Dict.keys (Tuple.second arr) |> List.map (\b -> BeatBlockJson b (case Dict.get b (Tuple.second arr) of
                                                                                                                                                                                                                  Just block -> block.blockName
                                                                                                                                                                                                                  _ -> "P"))))
                                                          in                                                                                      
                                                          BarJson (Tuple.first bar)
                                                                  arrangement
                                                                  beatOpts
                                                )                                                                                                                                                                                                                  
  in
  JsonE.encode 1 (JsonE.object [("bars", JsonE.list (\bar -> JsonE.object [("barNo", JsonE.int bar.barNo)
                                                                          ,("arrangement", JsonE.list (\arr -> JsonE.object [("instrument", JsonE.string arr.instrumentName)
                                                                                                                                  , ("blocks", JsonE.list (\b -> JsonE.object [("beat", JsonE.int b.beat)
                                                                                                                                                                              ,("blockName", JsonE.string b.blockName)]
                                                                                                                                                          ) arr.blocks)]) bar.arrangement)
                                                                          ,("beatOptions", JsonE.list (\bo -> JsonE.object [("beat", JsonE.int bo.beat)
                                                                                                                                , ("ghostNotes", JsonE.int bo.ghostNotes)
                                                                                                                                , ("accents", JsonE.int bo.accents)]) bar.beatOptions)
                                                                          ]) bars
                                )]                                                                                                                                
                  )

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
addInstrument : Model -> SelectIdValue -> ( Model, Cmd Msg )
addInstrument model param = 
  case Dict.get param.value instrumentDict of
      Just instr -> let
                      newBars = (Dict.toList model.bars) |> List.map (\bar -> ((Tuple.first bar), Bar (Dict.insert param.value (Dict.fromList [(1, pBlock), (2, pBlock), (3, pBlock), (4, pBlock)]) ((Tuple.second bar).arrangement))
                                                                                                      (Tuple.second bar).beatOptions
                                                                                                      (Tuple.second bar).timeSignature))
                                                         |> Dict.fromList 
                    in
                    ({model |bars = newBars}, Cmd.none)
      _ -> ({model | debugText = (Debug.toString value)}, Cmd.none)

updateBeatOptions : Model -> BarDict
updateBeatOptions model = 
  case model.beatOptionsParams of
    Just bOptParams ->  let 
                          bar = Dict.get bOptParams.barNo  model.bars 
                        in
                        case bar of
                            Just b -> Dict.insert bOptParams.barNo (Bar b.arrangement
                                                                       (Dict.insert bOptParams.beat bOptParams.beatOptions b.beatOptions) 
                                                                       b.timeSignature)
                                                                       model.bars
                            _ -> model.bars
    _ ->  model.bars


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
       , section [HA.class "flex-container-row"]
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
                 [(div [HA.class "flex-container-row"]
                       [div
                          [HA.id "instruments"]
                          [div
                              [] 
                              (div [HA.class "group-title"] [Html.text "Instrument"]
                              :: (displayInstruments model.bars)
                              ++ [Html.select  [onInputSelectChange AddInstrumentSelectedChange
                                                          , HA.alt "Add New Instrument"
                                                          , HA.title "Add New Instrument"
                                                          , HA.id "add-instrument-select"
                                                          ]
                                                          (Html.option [selected True ] [Html.text "Add Instrument"]
                                                          :: (getAvailableInstruments model))]
                              )
                          ]
                       ,div [HA.id "bars"] [displayBars model.bars]
                      ]
                  )
                 ,renderStave model
                 ,Html.text model.debugText
                 ,optionsDialog "beat-options-dialog"
                        (buildBeatOptionsDialog model
                        ++  [Html.div [HA.class "subBeatOptionsDialogButtons"] 
                                      [button [ onClick BeatOptionsDialogSave, HA.class "subBeatOptionsDialogButton", HA.id "bb" ] [ Html.text "Save" ]
                                      , button [ onClick BeatOptionsDialogCancel, HA.class "subBeatOptionsDialogButton" ] [ Html.text "Cancel" ]
                                      ]
                            ]
                        )
                  ,optionsDialog "bar-options-dialog"
                        (buildBarOptionsDialog model
                        ++  [Html.div [HA.class "subBeatOptionsDialogButtons"] 
                                      [button [ onClick BarOptionsDialogSave, HA.class "subBeatOptionsDialogButton", HA.id "bb" ] [ Html.text "Save" ]
                                      , button [ onClick BarOptionsDialogCancel, HA.class "subBeatOptionsDialogButton" ] [ Html.text "Cancel" ]
                                      ]
                            ]
                        )
                  ]
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
  div [HA.id "bars-flex", HA.class "flex-container-row"]
      ((Dict.toList bars)
          |> List.map (\bar -> table  [HA.class "bar-table"]
                                      ((tr []
                                          [th [HA.class "instrumentTableHeaderCell"
                                              , colspan 4] 
                                              [div [HA.class "flex-container-row", HA.id "bar-header"] 
                                                  [div[HA.id "bar-text"][Html.text ("Bar " ++ (String.fromInt (Tuple.first bar)))]
                                                  ,div [HA.id "bar-buttons"] 
                                                        [button [ onClick BeatOptionsDialogCancel, HA.class "barButton" ] 
                                                                [ Html.img [HA.src "assets/images/settings.svg"
                                                                            , HA.class "barButtonImg"
                                                                            , HA.alt "Bar Settings"
                                                                            , HA.title "Bar Settings"] []]
                                                        , button [ onClick (BarAdd (Tuple.first bar)), HA.class "barButton" ] 
                                                                [ Html.img [HA.src "assets/images/add.svg"
                                                                            , HA.class "barButtonImg"
                                                                            , HA.alt "Add Bar (after this one)"
                                                                            , HA.title "Add Bar (after this one)"] []]
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
                                                                                , onClick (BeatOptionsDialogOpen (BeatOptionsParams (Tuple.first bar)
                                                                                                                                    beat 
                                                                                                                                    (case Dict.get beat (Tuple.second bar).beatOptions of
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
  case model.beatOptionsParams of
      Just params ->
        case Dict.get params.barNo model.bars of
            Just bar ->
                      let
                        isGhostable = (Dict.keys bar.arrangement) |> List.foldl (\i isGhable -> case Dict.get i instrumentDict of
                                                                                                        Just instr -> isGhable || instr.isGhostable
                                                                                                        _ -> isGhable) False 
                        isAccentable = (Dict.keys bar.arrangement) |> List.foldl (\i isAccable -> case Dict.get i instrumentDict of
                                                                                                        Just instr -> isAccable || instr.isAccentable
                                                                                                        _ -> isAccable) False 
                      in
                      [div []
                                [svg
                                    [ Svg.Attributes.width "100"
                                    , Svg.Attributes.height "40"
                                    , Svg.Attributes.class "stave"
                                    ]
                                    (renderStaveBeat params.beat bar)
                                ]
                      ,div  []
                            ((if isGhostable then 
                                [Html.div [HA.class "blockOptionsContainer"] 
                                          [Html.text "Ghost Notes"
                                          , Html.div [HA.class "embellishPatternBox"]
                                                    (displayGhostCheckboxes bar params)
                                          ]
                                ]
                              else []
                              )
                          ++ (if isAccentable then 
                                [Html.div [HA.class "blockOptionsContainer"] 
                                          [Html.text "Accents"
                                          , Html.div [HA.class "embellishPatternBox"]
                                                    (displayAccentCheckboxes bar params)
                                          ]
                                ]
                              else []
                              ))
                      ]
            _ -> []              
      _ -> []              


buildBarOptionsDialog : Model -> List (Html Msg)
buildBarOptionsDialog model =
  []


displayAccentCheckboxes : Bar -> BeatOptionsParams -> List (Html Msg)
displayAccentCheckboxes bar params =
  let
    subBeatRange = if "4-16" == "4-16" then [{index = 1, bitmap = (Binary.fromIntegers [1,0,0,0])}
                                                                        , {index = 2, bitmap = (Binary.fromIntegers [0,1,0,0])}
                                                                        , {index = 3, bitmap = (Binary.fromIntegers [0,0,1,0])}
                                                                        , {index = 4, bitmap = (Binary.fromIntegers [0,0,0,1])}] 
                                                                   else [{index = 1, bitmap = (Binary.fromIntegers [1,0,0])}
                                                                        , {index = 2, bitmap = (Binary.fromIntegers [0,1,0])}
                                                                        , {index = 3, bitmap = (Binary.fromIntegers [0,0,1])}]
    accentableSubBeats = (Dict.toList bar.arrangement)  |> List.map (\i -> Tuple.pair (Dict.get (Tuple.first i) instrumentDict) (Dict.get params.beat (Tuple.second i)) )
                                                          |> List.map (\ib -> case (Tuple.first ib) of
                                                                                Just instrument ->  if instrument.isAccentable then
                                                                                                      case (Tuple.second ib) of 
                                                                                                          Just block -> block.notePlacement
                                                                                                          _ -> (Binary.fromIntegers [0,0,0,0])
                                                                                                    else 
                                                                                                      (Binary.fromIntegers [0,0,0,0])
                                                                                _ -> (Binary.fromIntegers [0,0,0,0]))
                                                          |> List.foldl (Binary.or) (Binary.fromIntegers [0,0,0,0])
    accentPattern = params.beatOptions.accents

  in
  (subBeatRange |> List.map (\i -> Html.input [HA.type_ "checkbox"
                                             , HA.id ("accent_checkbox_" ++ String.fromInt i.index)
                                             , HA.class "embellishCheckbox"
                                             , onCheckboxChanged AccentCheckBoxChanged
                                             , checked (Binary.toDecimal (Binary.and i.bitmap accentPattern) /= 0)
                                             , HA.disabled (Binary.toDecimal (Binary.and i.bitmap accentableSubBeats) == 0)][]
                           )
  )--  ++ [Html.text (Debug.toString accentableSubBeats)]


displayGhostCheckboxes : Bar -> BeatOptionsParams -> List (Html Msg)
displayGhostCheckboxes bar params =
  let
    subBeatRange = if "4-16" == "4-16" then [{index = 1, bitmap = (Binary.fromIntegers [1,0,0,0])}
                                                                        , {index = 2, bitmap = (Binary.fromIntegers [0,1,0,0])}
                                                                        , {index = 3, bitmap = (Binary.fromIntegers [0,0,1,0])}
                                                                        , {index = 4, bitmap = (Binary.fromIntegers [0,0,0,1])}] 
                                                                   else [{index = 1, bitmap = (Binary.fromIntegers [1,0,0])}
                                                                        , {index = 2, bitmap = (Binary.fromIntegers [0,1,0])}
                                                                        , {index = 3, bitmap = (Binary.fromIntegers [0,0,1])}]
    ghostableSubBeats = (Dict.toList bar.arrangement)   |> List.map (\i -> Tuple.pair (Dict.get (Tuple.first i) instrumentDict) (Dict.get params.beat (Tuple.second i)) )
                                                          |> List.map (\ib -> case (Tuple.first ib) of
                                                                                Just instrument ->  if instrument.isGhostable then
                                                                                                      case (Tuple.second ib) of 
                                                                                                          Just block -> block.notePlacement
                                                                                                          _ -> (Binary.fromIntegers [0,0,0,0])
                                                                                                    else 
                                                                                                      (Binary.fromIntegers [0,0,0,0])
                                                                                _ -> (Binary.fromIntegers [0,0,0,0]))
                                                          |> List.foldl (Binary.xor) (Binary.fromIntegers [1,1,1,1])
    ghostPattern = params.beatOptions.ghostNotes

  in
  (subBeatRange |> List.map (\i -> Html.input [HA.type_ "checkbox"
                                             , HA.id ("ghost_checkbox_" ++ String.fromInt i.index)
                                             , HA.class "embellishCheckbox"
                                             , onCheckboxChanged GhostCheckBoxChanged
                                             , checked (Binary.toDecimal (Binary.and i.bitmap ghostPattern) /= 0)
                                             , HA.disabled (Binary.toDecimal (Binary.and i.bitmap ghostableSubBeats) == 0)][]
                           )
  )--  ++ [Html.text (Debug.toString accentableSubBeats)]





subdivisionOption : Subdivision -> Html Msg
subdivisionOption subdiv = 
    Html.option [] [Html.text subdiv.description]

displayInstruments : BarDict -> List (Html Msg)
displayInstruments bars = 
  let
    instrs = getIncludedInstrumentNames bars
  in
  (instrs)  |> List.map (\instrName  ->   let
                                              sortOrder = case Dict.get instrName instrumentDict of
                                                    Just instr -> instr.sortOrder
                                                    _ -> 100
                                            in
                                            {instrName = instrName, sortOrder = sortOrder})
            |> List.sortBy .sortOrder
            |> List.map (\a -> div [HA.class "sub-group-title1"] [div [HA.class "flex-container-row"] 
                                                                      ((div [HA.style "flex-grow" "1"] [Html.text a.instrName])
                                                                      :: if List.length instrs > 1 then
                                                                           [button [ onClick (InstrumentDelete a.instrName)
                                                                                     , HA.class "barButton" ] 
                                                                                   [ Html.img [HA.src "assets/images/remove.svg"
                                                                                              , HA.class "barButtonImg"
                                                                                              , HA.alt "Delete Instrument"
                                                                                              , HA.title "Delete Instrument"] []]]
                                                                          else [] 
                                                                      )
                                                                  ]
                        )

renderStave : Model -> Html Msg
renderStave model = 
      div [HA.id "stave-view"]
          [div [HA.width 600
               , HA.height (100 + (100 * ((Dict.size model.bars) - 1)))]
               [svg
                  [ Svg.Attributes.id "stave"
                  , Svg.Attributes.viewBox "0 0 200 100"
                  ]
                  (renderStaveBars model.bars)
               ]              
          ]

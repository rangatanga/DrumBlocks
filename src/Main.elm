module Main exposing (main)

import Browser
import Browser.Navigation as Nav
import Html exposing (..)
import Html.Attributes exposing (..)
import Url
import Element exposing (Element, el, text, row, alignRight, fill, width, rgb255, spacing, centerY, padding, rgb, Color)
import Element.Background as Background
import Element.Border as Border
import Element.Font as Font
import Element.Input as Input
import Platform exposing (Router)
import Binary exposing (..)
import Element.Input
import Svg exposing (..)
import Svg.Attributes exposing (..)
import Select exposing (..)
import Dict exposing (..)
import Array exposing (Array)
import Widget exposing (..)
import Widget.Material as Material
--import Html.Events exposing (..)


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
  , subdivisionSelect : Select.Select Subdivision
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

staveLines : List Int
staveLines =
    [ 3, 6, 9, 12, 15]

instrumentDict : Dict String Instrument
instrumentDict = Dict.fromList 
    [("Hi-Hat", Instrument "G5" 1.5 Cross)
    , ("Ride Cymbal", Instrument "F5" 3 CrossLedger)
    , ("High Tom", Instrument "E5" 4.5 Ovoid)
    , ("Mid Tom", Instrument "D5" 6 Ovoid)
    , ("Snare", Instrument "C5" 7.5 Ovoid)
    , ("Floor Tom", Instrument "A4" 10.5 Ovoid)
    , ("Bass Drum", Instrument "F4" 13.5 Ovoid)      
    , ("Hi-hat Foot", Instrument "D4" 16 Cross)
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
    , subdivisionSelect = Select.init "select-subdivision" |> Select.setItems [Subdivision "4-16" "Four 16ths" 4
                                                                              ,Subdivision "3-8" "Three 8ths" 3]
    }   


-- UPDATE


type Msg
  = LinkClicked Browser.UrlRequest
  | UrlChanged Url.Url
  | BlockClickMsg 
  | SubdivisionSelectMsg (Select.Msg Subdivision)
  | ChangedSelected Int
  | InputChanged String


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model = 
    case msg of
        SubdivisionSelectMsg subMsg ->
            Select.update SubdivisionSelectMsg subMsg model.subdivisionSelect
                |> Tuple.mapFirst (\select -> { model | subdivisionSelect = select })
        _ -> (model, Cmd.none)



-- SUBSCRIPTIONS


subscriptions : Model -> Sub Msg
subscriptions _ =
  Sub.none



-- VIEW


view : Model -> Browser.Document Msg
view model =
  { title = "Drum Blocks"
  , body =
      [ Element.layout [] <| 
          (Element.column 
            [Element.width Element.fill
            ]
            (Element.row []
              [
              Element.el 
                [Element.width (Element.px 180)
                ,Font.size 22] 
                (Element.text "Subdivision:")
              , subdivisionDropdown model
              ]
           :: (instrumentView model.arrangement)
           ++ [Element.el 
                [Element.width (Element.px 180)
                , Element.height (Element.px 40)
                , Font.size 22
                --, Element.spacing 15
                , Element.padding 5
                ] 
                (Element.text "+ Add Instrument")]
           ++ [Element.el 
                [Element.height (Element.px 50) 
                ]
                Element.none
              ]
           ++ [Element.el 
                [Element.alignLeft
                , Element.alignTop
                , Element.height (Element.px 350) 
                , Element.width (Element.px 2000) 
                , Element.padding 5
                ] 
                (Element.html (svg
                                  [ Svg.Attributes.width "100%"
                                  , Svg.Attributes.height "100%"
                                  , viewBox "0 0 210 105"
                                  ]
                                  (stave ++ percussionClef ++ (renderBar model.arrangement)))
                                  --(stave ++ percussionClef))
                ) --Element.html
              ] --Element.el 
            ++ [(Input.text [] {onChange = InputChanged
                            , text = "StrAing"
                            , placeholder = Nothing-- Just (Input.placeholder [] (Element.text ""))
                            , label = (Input.labelHidden "Label ")
                           })
              ]
            )
          ) --Element.column
      ]
  }


subdivisionDropdown : Model -> Element Msg
subdivisionDropdown model = 
    Select.view
        |> Select.toElement []
            { select = model.subdivisionSelect
            , onChange = SubdivisionSelectMsg
            , itemToString = \c -> c.description
            , label = Element.Input.labelHidden ""
            , placeholder = Just (Element.Input.placeholder [] (Element.text "Type to search"))
            }
    {- Element.el  [Font.size 22] 
        (Element.row [] 
            [ Element.el [Element.width (Element.px 150)] (Element.text "Subdivision:")
            , Element.el [Font.size 22] (Element.html (select [] (List.map subdivisionOption model.subdivisions)))
            ]
        ) -}


subdivisionOption : Subdivision -> Html Msg
subdivisionOption subdiv = 
    Html.option [] [Html.text subdiv.description]

instrumentView : List InstrumentBlocks -> List (Element Msg)
instrumentView instrumentBlocks = List.map instrumentRow instrumentBlocks

instrumentRow : InstrumentBlocks -> Element Msg
instrumentRow instrumentBlock = Element.row 
                                  [Element.height (Element.px 40)
                                  , Element.spacing 5
                                  ] ([Element.el 
                                      [Element.width (Element.px 180)
                                      , Font.size 22
                                      ] (Element.text instrumentBlock.instrumentName)] ++ blockView instrumentBlock.blockNames)

blockView : List String -> List (Element Msg)
blockView blocks = List.map blockButton blocks

blockButton : String -> Element Msg
blockButton blockName = 
  Element.Input.button 
                  [ Background.color (Element.rgb255 238 238 238)
                  , Element.focused [Background.color (Element.rgb255 238 238 238)]
                  , Element.width (Element.px 80)
                  , Element.height (Element.px 25)
                  , Border.solid
                  , Border.color (rgb 0 0 0)
                  , Border.width 2
                  , Border.shadow {offset = (12.0,12.0), size = 5, blur = 5, color = (rgb 10 10 10)}
                  , Border.rounded 5
                  , Font.center
                  , Font.size 22
                  ]
                  { onPress = Just BlockClickMsg
                  , label = Element.text blockName
                  }


{-
myElement : String -> Element msg
myElement txt =
    el
        [ Background.color (rgb255 140 0 245)
        , Font.color (rgb255 255 255 255)
        , Border.rounded 3
        , padding 30
        ]
        (Element.text txt)
-}

stave : List (Svg Msg)
stave =
    (staveLines)
        |> List.map String.fromInt
        |> List.map
            (\n ->
                Svg.path
                    [ strokeWidth "0.3"
                    , stroke "black"
                    , d ("M 5 " ++ n ++ " L 395 " ++ n)
                    ]
                    []
            )

  
percussionClef : List(Svg Msg)
percussionClef =
  [Svg.path
      [ strokeWidth "1.8"
      , stroke "black"
      , d ("M 10 6 L 10 12")
      ]
      []
  ,Svg.path
      [ strokeWidth "1.8"
      , stroke "black"
      , d ("M 13 6 L 13 12")
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
              |> List.map (\i -> {beat = Tuple.first i
                                 , subBeat = Tuple.second i
                                 , noteBlocks = noteBlocks})
              |> List.map beatLoop
              |> List.concat
              --|> Debug.toString


type alias BeatLoopParams =
 { beat : Int
  , subBeat : Int
  , noteBlocks : List(NoteBlock)
 }
{-
Variable beat is looping from 1 to 4, within this subBeat is looping from 1 to 12.

noteBlock has value blockBeat in range 1 to 4, it also has the block name.

If beat == blockBeat && the block has a note on the subBeat then draw note
else do nothing
-}
beatLoop : BeatLoopParams -> List(Svg Msg)
beatLoop params  = 
  (params.noteBlocks) |> List.map (\n -> {beat = params.beat
                                         , subBeat = params.subBeat
                                         , noteBlock = n})
                      |> List.map blockLoop
                      |> List.concat
                      --|> Debug.toString (Binary.toDecimal (Binary.and (Binary.fromIntegers [1,1,0]) (Binary.fromIntegers [1,0,0])))

type alias BlockLoopParams =
 { beat : Int
  , subBeat : Int
  , noteBlock : NoteBlock
 }

blockLoop :BlockLoopParams -> List(Svg Msg)
blockLoop params = 
  (if (params.beat == params.noteBlock.blockBeat
     && isSubBeatMatch params.subBeat params.noteBlock) then
    renderNote params.beat params.subBeat params.noteBlock
  else
    []
  ) 


renderNote : Int -> Int -> NoteBlock -> List (Svg Msg)
renderNote beat subBeat noteBlock = 
  let
      noteCenterX = 20.0 + ((toFloat (((beat - 1) * 12) + (subBeat - 1))) * 4.0)
      noteCenterY = noteBlock.stavePos
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

  
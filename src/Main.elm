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
import Platform exposing (Router)
import Binary exposing (..)
import Element.Input
import Svg exposing (..)
import Svg.Attributes exposing (..)
import Select exposing (..)
import Dict exposing (..)
import Array exposing (Array)
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
  , subdivisionSelect : Select Subdivision
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
  , notePlacement : List Int
  , subdivision : String
  }

type alias InstrumentBlocks = 
  { instrumentName : String
  , blockNames : List String
  }

 
staveLines : List Int
staveLines =
    [ 3, 6, 9, 12, 15]

instrumentDict : Dict String Instrument
instrumentDict = Dict.fromList 
    [("Hi-Hat", Instrument "G5" 2 Cross)
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
              [ ("A", Block "A.png" [1,0,0,0] "4-16")
              , ("B", Block "B.png" [0,1,0,0] "4-16")
              , ("C", Block "B.png" [0,0,1,0] "4-16")
              , ("D", Block "B.png" [0,0,0,1] "4-16")
              , ("E", Block "B.png" [1,1,0,0] "4-16")
              , ("F", Block "B.png" [0,1,1,0] "4-16")
              , ("G", Block "B.png" [0,0,1,1] "4-16")
              , ("H", Block "B.png" [1,0,0,1] "4-16")
              , ("I", Block "B.png" [1,0,1,0] "4-16")
              , ("J", Block "B.png" [0,1,0,1] "4-16")
              , ("K", Block "B.png" [1,1,1,0] "4-16")
              , ("L", Block "B.png" [0,1,1,1] "4-16")
              , ("M", Block "B.png" [1,0,1,1] "4-16")
              , ("N", Block "B.png" [1,1,0,1] "4-16")
              , ("O", Block "B.png" [1,1,1,1] "4-16")
              , ("P", Block "B.png" [0,0,0,0] "4-16")
              , ("Q", Block "B.png" [1,0,0] "3-8")
              , ("R", Block "B.png" [0,1,0] "3-8")
              , ("S", Block "B.png" [0,0,1] "3-8")
              , ("T", Block "B.png" [1,1,0] "3-8")
              , ("U", Block "B.png" [0,1,1] "3-8")
              , ("V", Block "B.png" [1,0,1] "3-8")
              , ("W", Block "B.png" [1,1,1] "3-8")
              , ("X", Block "B.png" [0,0,0] "3-8")
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
    , arrangement = [ InstrumentBlocks "Hi-Hat" ["A", "A", "A", "A"]
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
                , Element.width (Element.px 1000) 
                , Element.padding 5
                ] 
                (Element.html (svg
                                  [ Svg.Attributes.width "100%"
                                  , Svg.Attributes.height "100%"
                                  , viewBox "0 0 110 105"
                                  ]
                                  (stave ++ percussionClef ++ (renderNotes model.arrangement)))
                ) --Element.html
              ] --Element.el 
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
blockButton block = Element.Input.button 
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
                                    , label = Element.text block
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
                    , d ("M 5 " ++ n ++ " L 95 " ++ n)
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
  
renderNotes : List InstrumentBlocks -> List(Svg Msg)
renderNotes instrumentBlocks = 
  (instrumentBlocks) |> List.map createInstrumentBlocks |> List.concat


createInstrumentBlocks : InstrumentBlocks -> List(Svg Msg)
createInstrumentBlocks instrBlocks = 
  let
    sp = getStavePosition instrBlocks.instrumentName
    ns = getNoteShape instrBlocks.instrumentName
  in
  (instrBlocks.blockNames)  |> List.indexedMap (\i b ->  {index = i
                                                          , stavePos = sp
                                                          , noteShape = ns
                                                          , blockName = b})
                           |> List.map renderNote
                           |> List.concat

type alias NoteBlock =
  {
    index : Int
    ,stavePos : Float
    ,noteShape : NoteShape
    ,blockName : String
  }

renderNote : NoteBlock -> List (Svg Msg)
renderNote noteBlock = 
  let
      stavePos = noteBlock.stavePos
      noteIndex = noteBlock.index

  in
  case noteBlock.noteShape of
      Ovoid ->
          [Svg.ellipse [cx (String.fromFloat (20 + ((toFloat noteIndex) * 12.0))), cy (String.fromFloat stavePos), rx "1.6", ry "1.4", transform ("rotate(-15, 20, " ++ (String.fromFloat stavePos) ++ ")")] []]
      Cross ->
          [Svg.path
            [ strokeWidth "0.5"
              , stroke "black"
              , d ("M 18 " ++ (String.fromFloat (stavePos - 1)) ++ " L 22 " ++ (String.fromFloat (stavePos + 1)))
            ] []
          ,Svg.path
            [ strokeWidth "0.5"
              , stroke "black"
              , d ("M 22 " ++ (String.fromFloat (stavePos - 1)) ++ " L 18 " ++ (String.fromFloat (stavePos + 1)))
            ] []
          ]
      CrossLedger ->
          [Svg.circle [cx "20", cy (String.fromFloat stavePos), r "1.5"] []]
      Triangle ->
          [Svg.circle [cx "20", cy (String.fromFloat stavePos), r "1.5"] []]


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

  
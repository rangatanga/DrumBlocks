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
  , instruments : List Instrument
  , availableInstruments : List Instrument
  , blockDict : Dict String Block
  , arrangement : List InstrumentBlocks
  , subdivisionSelect : Select Subdivision
  }

type alias Subdivision =
  { name : String
  , description : String 
  , subBeats : Int
  }

type alias Instrument = 
  { name : String
  , staveLocation : String
  , stavePosition : Int 
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
    , instruments = [Instrument "Hi-Hat" "G5" 10
                    , Instrument "Snare" "C5" 6
                    , Instrument "Bass Drum" "F4" 2
                    ]
    , availableInstruments = [Instrument "Ride Cymbal" "F5" 9
                             , Instrument "High Tom" "E5" 8
                             , Instrument "Mid Tom" "D5" 7
                             , Instrument "Floor Tom" "A4" 4
                             , Instrument "Hi-hat Foot" "D4" 0
                             ]
    , blockDict = Dict.fromList 
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
                                  (stave ++ percussionClef))
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

staveLines : List Int
staveLines =
    [ 0, 3, 6, 9, 12]
  
percussionClef : List(Svg Msg)
percussionClef =
  [Svg.path
      [ strokeWidth "1.8"
      , stroke "black"
      , d ("M 10 3 L 10 9")
      ]
      []
  ,Svg.path
      [ strokeWidth "1.8"
      , stroke "black"
      , d ("M 13 3 L 13 9")
      ]
      []
  ]
  
--renderNotes : List (InstrumentBlocks) -> List(Svg Msg)
--renderNotes instrBlocks = List.map \ib -> 


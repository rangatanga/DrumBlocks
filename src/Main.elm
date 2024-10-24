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
  , blocks : List Block
  , arrangement : List InstrumentBlocks
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
  { name : String
  , imageName : String
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
    , blocks = [Block "A" "A.png" (Binary.fromIntegers [1,0,0,0]) "4-16"
               , Block "B" "B.png" (Binary.fromIntegers [0,1,0,0]) "4-16"
               , Block "C" "B.png" (Binary.fromIntegers [0,0,1,0]) "4-16"
               , Block "D" "B.png" (Binary.fromIntegers [0,0,0,1]) "4-16"
               , Block "E" "B.png" (Binary.fromIntegers [1,1,0,0]) "4-16"
               , Block "F" "B.png" (Binary.fromIntegers [0,1,1,0]) "4-16"
               , Block "G" "B.png" (Binary.fromIntegers [0,0,1,1]) "4-16"
               , Block "H" "B.png" (Binary.fromIntegers [1,0,0,1]) "4-16"
               , Block "I" "B.png" (Binary.fromIntegers [1,0,1,0]) "4-16"
               , Block "J" "B.png" (Binary.fromIntegers [0,1,0,1]) "4-16"
               , Block "K" "B.png" (Binary.fromIntegers [1,1,1,0]) "4-16"
               , Block "L" "B.png" (Binary.fromIntegers [0,1,1,1]) "4-16"
               , Block "M" "B.png" (Binary.fromIntegers [1,0,1,1]) "4-16"
               , Block "N" "B.png" (Binary.fromIntegers [1,1,0,1]) "4-16"
               , Block "O" "B.png" (Binary.fromIntegers [1,1,1,1]) "4-16"
               , Block "P" "B.png" (Binary.fromIntegers [0,0,0,0]) "4-16"
               , Block "Q" "B.png" (Binary.fromIntegers [1,0,0]) "3-8"
               , Block "R" "B.png" (Binary.fromIntegers [0,1,0]) "3-8"
               , Block "S" "B.png" (Binary.fromIntegers [0,0,1]) "3-8"
               , Block "T" "B.png" (Binary.fromIntegers [1,1,0]) "3-8"
               , Block "U" "B.png" (Binary.fromIntegers [0,1,1]) "3-8"
               , Block "V" "B.png" (Binary.fromIntegers [1,0,1]) "3-8"
               , Block "W" "B.png" (Binary.fromIntegers [1,1,1]) "3-8"
               , Block "X" "B.png" (Binary.fromIntegers [0,0,0]) "3-8"
               ]
    , arrangement = [ InstrumentBlocks "Hi-Hat" ["A", "A", "A", "A"]
                    , InstrumentBlocks "Snare" ["P", "A", "P", "A"]
                    , InstrumentBlocks "Bass Drum" ["A", "P", "A", "P"]
                    ]               
    }   


-- UPDATE


type Msg
  = LinkClicked Browser.UrlRequest
  | UrlChanged Url.Url
  | BlockClickMsg 


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model = 
    (model, Cmd.none)


-- SUBSCRIPTIONS


subscriptions : Model -> Sub Msg
subscriptions _ =
  Sub.none



-- VIEW


view : Model -> Browser.Document Msg
view model =
  { title = "Drum Blocks"
  , body =
      [ Element.layout [] <| Element.column 
                              [] 
                              ([Element.row 
                                [] 
                                [subdivisionDropdown model] 
                               ]++(instrumentView model.arrangement))
      ,svg
            [ Svg.Attributes.width "100%"
            , Svg.Attributes.height "100%"
            , viewBox "0 0 100 95"
            ]
            (stave)
      ]
  }


subdivisionDropdown : Model -> Element Msg
subdivisionDropdown model = 
    el  [Font.size 18] 
        (Element.row [] 
            [ Element.el [Element.width (Element.px 150)] (Element.text "Subdivision:")
            , Element.html (select [] (List.map subdivisionOption model.subdivisions))
            ]
        )


subdivisionOption : Subdivision -> Html Msg
subdivisionOption subdiv = 
    Html.option [] [Html.text subdiv.description]

instrumentView : List InstrumentBlocks -> List (Element Msg)
instrumentView instrumentBlocks = List.map instrumentRow instrumentBlocks

instrumentRow : InstrumentBlocks -> Element Msg
instrumentRow instrumentBlock = Element.row [Element.height (Element.px 40)] ([Element.el [Element.width (Element.px 150)] (Element.text instrumentBlock.instrumentName)] ++ blockView instrumentBlock.blockNames)

blockView : List String -> List (Element Msg)
blockView blocks = List.map blockButton blocks

blockButton : String -> Element Msg
blockButton block = Element.Input.button 
                                    [ Background.color (Element.rgb255 238 238 238)
                                    , Element.focused [Background.color (Element.rgb255 238 238 238)]
                                    , Element.width (Element.px 60)
                                    , Border.solid
                                    , Border.color (rgb 0 0 0)
                                    , Border.width 2
                                    , Border.shadow {offset = (12.0,12.0), size = 5, blur = 5, color = (rgb 100 100 100)}
                                    , Border.rounded 5
                                    , Font.center
                                    , Font.size 20
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
    (clefLines)
        |> List.map String.fromInt
        |> List.map
            (\n ->
                Svg.path
                    [ strokeWidth "0.2"
                    , stroke "black"
                    , d ("M 5 " ++ n ++ " L 95 " ++ n)
                    ]
                    []
            )

clefLines : List Int
clefLines =
    [ 0, 4, 8, 12, 16 ]
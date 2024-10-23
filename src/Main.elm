module Main exposing (main)

import Browser
import Browser.Navigation as Nav
import Html exposing (..)
import Html.Attributes exposing (..)
import Url
import Element exposing (Element, el, text, row, alignRight, fill, width, rgb255, spacing, centerY, padding)
import Element.Background as Background
import Element.Border as Border
import Element.Font as Font
import Platform exposing (Router)


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
  { subdivisions : Subdivisions
  , instruments : Instruments
  , availableInstruments : Instruments
  }

type alias Subdivisions = List Subdivision

type alias Subdivision =
  { name : String
  , description : String 
  , subBeats : Int
  }

type alias Instruments = List Instrument

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
    }   


-- UPDATE


type Msg
  = LinkClicked Browser.UrlRequest
  | UrlChanged Url.Url


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
      [  Element.layout [] <| Element.row 
                                [] 
                                [subdivisionDropdown model]
      ]
  }

subdivisionDropdown : Model -> Element Msg
subdivisionDropdown model = 
    el  [Font.size 18] 
        (Element.row [] 
            [ Element.text "Subdivision: "
            , Element.html (select [] (List.map subdivisionOption model.subdivisions))
            ]
        )


subdivisionOption : Subdivision -> Html Msg
subdivisionOption subdiv = 
    Html.option [] [Html.text subdiv.description]

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
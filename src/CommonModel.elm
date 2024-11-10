module CommonModel exposing (..)

import Browser
import Url
import Binary exposing (..)
import Dict exposing (..)
import File exposing (..)


type Msg
  = LinkClicked Browser.UrlRequest
  | UrlChanged Url.Url
  | BlockSelectedChange SelectIdValue
  | AddInstrumentSelectedChange SelectIdValue
  | BeatOptionsDialogOpen BeatOptionsParams
  | BeatOptionsDialogSave
  | BeatOptionsDialogCancel
  | GhostCheckBoxChanged CheckboxIdChecked
  | AccentCheckBoxChanged CheckboxIdChecked
  | KeyPressedMsg KeyEventMsg
  | KeyReleasedMsg KeyEventMsg
  | PatternSave
  | PatternLoad
  | FileSelected File
  | FileLoaded String
type KeyEventMsg
    = KeyEventControl
    | KeyEventAlt
    | KeyEventShift
    | KeyEventMeta
    | KeyEventLetter Char
    | KeyEventUnknown String
  --| SubdivisionSelectMsg (Select.Msg Subdivision)

type alias BeatOptions = 
  { ghostNotes : Bits
  , accents : Bits
  }

type alias BeatOptionsParams = 
  {beat : Int
  , beatOptions : BeatOptions
  }

type alias SelectIdValue = 
  {
    id : String
    ,value : String
  }

type alias CheckboxIdChecked = 
  {
    id : String
    ,checked : Bool
  }
   

type NoteDuration =
  Crotchet
  | Quaver
  | SemiQuaver
  | Minim
  | Breve


type alias NoteDurationParam = 
  {
    noteDuration : NoteDuration
    ,isDotted : Bool
    ,nextSubBeat : Int
    ,prevSubBeat : Int
  }



type alias Model =
  { arrangement : InstrumentBlocksDict
  , beatOptions : BeatOptionsDict
  , timeSignature : String
  , beatOptionsParams : Maybe BeatOptionsParams
  , debugText : String
  }


type alias Subdivision =
  { name : String
  , description : String 
  , subBeats : Int
  }


type alias Block = 
  {blockName : String
  , imageName : String
  , notePlacement : Bits
  , subdivision : String
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
  , isGhostable : Bool
  , isAccentable : Bool
  , sortOrder : Int
  }


type alias InstrumentBlocksDict = Dict String BeatBlockDict

type alias BeatBlockDict = Dict Int Block

type alias BeatOptionsDict = Dict Int BeatOptions


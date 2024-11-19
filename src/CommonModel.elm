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
  | BarOptionsDialogOpen BarOptionsParams
  | BarOptionsDialogSave
  | BarOptionsDialogCancel
  | GhostCheckBoxChanged CheckboxIdChecked
  | AccentCheckBoxChanged CheckboxIdChecked
  | KeyPressedMsg KeyEventMsg
  | KeyReleasedMsg KeyEventMsg
  | PatternSave
  | PatternLoad
  | UploadSelected File
  | FileLoaded String
  | BarAdd Int
  | BarDelete Int
  | InstrumentDelete String
  | BarsScroll ScrollParams
  | NoOp
  --| Focus (Result Browser.DomError ())

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
  {barNo : Int
  , beat : Int
  , beatOptions : BeatOptions
  }

type alias BarOptionsParams = 
  {barNo : Int
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

type alias ScrollParams =
  {  
    id : String
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
  { bars : BarDict
  , beatOptionsParams : Maybe BeatOptionsParams
  , barOptionsParams : Maybe BarOptionsParams
  , debugText : String
  }

type alias BarDict = Dict Int Bar

type alias Bar = 
  {
    arrangement : InstrumentBlocksDict
  , beatOptions : BeatOptionsDict
  , timeSignature : String
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

type alias BarJson = 
  {barNo : Int
  , arrangement : List ArrangementJson
  , beatOptions : List BeatOptionJson
  }

type alias ArrangementJson = 
  {instrumentName : String
  , blocks : List BeatBlockJson
  }

type alias BeatOptionJson = 
  {beat : Int
  , ghostNotes : Int
  , accents : Int
  }

type alias BeatBlockJson = 
  {beat : Int
  , blockName : String
  }

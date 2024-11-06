module CommonEvents exposing (..)

import Json.Decode as Json
import Html.Events exposing (on)
import Html exposing (..)


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
   

onInputSelectChange : (SelectIdValue -> msg) -> Html.Attribute msg
onInputSelectChange tagger =
  on "change" (Json.map tagger selectDecoder)

onCheckboxChanged : (CheckboxIdChecked -> msg) -> Html.Attribute msg 
onCheckboxChanged tagger =
  on "change" (Json.map tagger checkboxDecoder)


selectDecoder : Json.Decoder SelectIdValue
selectDecoder =
  Json.map2 SelectIdValue targetIdDecoder targetValueDecoder

checkboxDecoder : Json.Decoder CheckboxIdChecked
checkboxDecoder =
  Json.map2 CheckboxIdChecked targetIdDecoder targetCheckedDecoder

targetIdDecoder : Json.Decoder String
targetIdDecoder =
  Json.at ["target", "id"] Json.string

targetValueDecoder : Json.Decoder String
targetValueDecoder =
  Json.at ["target", "value"] Json.string

targetCheckedDecoder : Json.Decoder Bool
targetCheckedDecoder =
  Json.at ["target", "checked"] Json.bool


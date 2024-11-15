module CommonEvents exposing (..)

import Json.Decode as Json
import Html.Events exposing (on)
import Html exposing (..)
import Binary exposing (..)
import File exposing (File)

import CommonModel exposing (..)



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



keyPressedDecoder : Json.Decoder Msg
keyPressedDecoder =
    Json.map (toKeyEventMsg >> KeyPressedMsg) (Json.field "key" Json.string)


keyReleasedDecoder : Json.Decoder Msg
keyReleasedDecoder =
    Json.map (toKeyEventMsg >> KeyReleasedMsg) (Json.field "key" Json.string)

filesDecoder : Json.Decoder (List File)
filesDecoder =
  Json.at ["target","files"] (Json.list File.decoder)


barDecoder : Json.Decoder BarJson
barDecoder =
  Json.map3 BarJson
    (Json.field "barNo" Json.int)
    (Json.field "arrangement" (Json.list arrangementDecoder))
    (Json.field "beatOptions" (Json.list beatOptionDecoder))


beatOptionDecoder : Json.Decoder BeatOptionJson
beatOptionDecoder =
  Json.map3 BeatOptionJson
    (Json.field "beat" Json.int)
    (Json.field "ghostNotes" Json.int)
    (Json.field "accents" Json.int)

arrangementDecoder : Json.Decoder ArrangementJson
arrangementDecoder =
  Json.map2 ArrangementJson
    (Json.field "instrument" Json.string)
    (Json.field "blocks" (Json.list beatBlockDecoder))

beatBlockDecoder : Json.Decoder BeatBlockJson
beatBlockDecoder =
  Json.map2 BeatBlockJson
    (Json.field "beat" Json.int)
    (Json.field "blockName" Json.string)

toKeyEventMsg : String -> KeyEventMsg
toKeyEventMsg eventKeyString =
    case eventKeyString of
        "Control" ->
            KeyEventControl

        "Shift" ->
            KeyEventShift

        "Alt" ->
            KeyEventAlt

        "Meta" ->
            KeyEventMeta

        string_ ->
            case String.uncons string_ of
                Just ( char, "" ) ->
                    KeyEventLetter char

                _ ->
                    KeyEventUnknown eventKeyString

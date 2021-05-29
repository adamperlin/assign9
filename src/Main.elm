module Main exposing (..)

import Browser
import Html exposing (Html, Attribute, div, input, text, button)
import Html.Attributes exposing (..)
import Html.Events exposing (onInput)
import Html.Events exposing (onClick)
import Html.Events exposing (onSubmit)
import Html.Events exposing (onClick)
import Parse exposing (parse, Expr, exprToString)
import Parser

main : Program () Model Msg
main =
  Browser.sandbox { init = init, update = update, view = view }

type Msg = Parse

type alias Model = {content : String}
init : Model
init = {content = ""}

update : Msg -> Model -> Model
update msg model = case msg of
    Parse -> let res = parse (model.content) in
        case res of
            Ok expr -> {model | content = exprToString expr}
            Err err -> {model | content = Parser.deadEndsToString err}

view : Model -> Html Msg
view model =
  div []
    [ input [placeholder "input giya code here", value model.content] []
    ,  button [onClick Parse] [text "parse"]
    , div [] [text (model.content)]]
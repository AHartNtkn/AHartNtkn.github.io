module Main exposing (..)

import List exposing (..)
import Maybe exposing (withDefault)
import Tuple exposing (first, second)
import Html exposing (..)
-- import Html.Attributes exposing (..)
import Browser exposing (..)
import Svg exposing (..)
import Svg.Attributes exposing (..)
import Svg.Events exposing (..)

nodeSize = 100

type GraphNode
  = Nothing
  | Cup Bool
  | Cap Bool
  | Dup Bool
  | Del Bool
  | Lam Bool
  | Left Bool
  | Right Bool
  | Hor Bool
  | Virt Bool

type alias GraphField
  = List (List GraphNode)

fieldSize : GraphField -> (Int, Int)
fieldSize l = (withDefault 1 (List.maximum (List.map length l)), length l)

type alias Model
  = {picture : GraphField}

initialModel = {picture = [ [ Del True , Del True , Del True ] , [ Virt True , Nothing , Virt True ] , [ Nothing , Virt True ] ] }
-- initialModel = Picture [ ]

addNodeX : Int -> GraphNode -> List GraphNode -> List GraphNode
addNodeX i n l =
  case l of
    [] -> repeat i Nothing ++ [n]
    (x :: xs) -> if i > 0 then x :: addNodeX (i-1) n xs else n :: xs

addNode : Int -> Int -> GraphNode -> GraphField -> GraphField
addNode i j n l = 
  case l of
    [] -> repeat j [] ++ [addNodeX i n []]
    (y :: ys) -> if j > 0 then y :: addNode i (j-1) n ys else addNodeX i n y :: ys

type Msg
  = Add GraphNode Int Int

update : Msg -> Model -> Model
update msg model =
  case msg of
    Add n i j -> { picture = addNode i j n model.picture }

nodePosition : Int -> Int -> (Int, Int)
nodePosition i j =
  if modBy 2 j == 0
  then ((nodeSize * i) + (nodeSize // 2), (2 * nodeSize * j // 3) + (nodeSize // 2))
  else ((nodeSize * i) + (nodeSize), (2 * nodeSize * j // 3) + (nodeSize // 2))
  

drawNode : Int -> Int -> GraphNode -> List (Svg Msg)
drawNode i j node = 
  case node of
    Nothing -> 
      []
    Del _ -> [
      circle
        [ cx (String.fromInt (first (nodePosition i j)))
        , cy (String.fromInt (second (nodePosition i j)))
        , r (String.fromInt (nodeSize // 2))
        , color "red"
        , fill "currentColor"
        , onClick (Add (Virt True) i j)
        ]
        [  ] ]
    Virt _ -> [
      circle
        [ cx (String.fromInt (first (nodePosition i j)))
        , cy (String.fromInt (second (nodePosition i j)))
        , r (String.fromInt (nodeSize // 2))
        , color "green"
        , fill "currentColor"
        , onClick (Add (Del True) i j)
        ]
        [] ]
    _ -> [
      circle
        [ cx (String.fromInt (first (nodePosition i j)))
        , cy (String.fromInt (second (nodePosition i j)))
        , r (String.fromInt (nodeSize // 2))
        , color "red"
        , fill "currentColor"
        ]
        [] ]

drawGraph : Int -> Int -> GraphField -> List (Svg Msg)
drawGraph i j pic = 
      case pic of
        [] ->
          [ circle
            [ cx (String.fromInt nodeSize)
            , cy (String.fromInt nodeSize)
            , r (String.fromInt (nodeSize // 2))
            , color "blue"
            , fill "currentColor"
            ]
            [] ]
        ([] :: []) -> []
        ([] :: l) -> drawGraph 0 (j+1) l
        ((l :: lp) :: ll) -> drawNode i j l ++ drawGraph (i+1) j (lp :: ll)

view : Model -> Html Msg
view model =
  div []
  [svg [ width (String.fromInt (first (fieldSize model.picture) * nodeSize + nodeSize // 2))
      , height (String.fromInt (second (fieldSize model.picture) * nodeSize))
      , viewBox ("0 0 " ++ 
                 String.fromInt (first (fieldSize model.picture) * nodeSize + nodeSize // 2) ++
                 " " ++ 
                 String.fromInt (second (fieldSize model.picture) * nodeSize)) ]
    (drawGraph 0 0 model.picture), 
    div [] [
      Html.text (
        String.fromInt (first (fieldSize model.picture)) ++ 
        "|" ++ 
        (String.fromInt (second (fieldSize model.picture) // 2))
        )]
    ]

-- main : Program () Model Msg
main =
  Browser.sandbox
    { init = initialModel 
    , view = view
    , update = update
    }
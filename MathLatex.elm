module MathLatex exposing (..)

import GraphicSVG
import GraphicSVG.App
import Html exposing (Html)

myShapes model =
  [
    GraphicSVG.square 1000 |> GraphicSVG.filled GraphicSVG.black
   ,
    arrow 0 0 |> GraphicSVG.scale 0.5
    
  ]

arrow x y =
  GraphicSVG.group[
    GraphicSVG.triangle 9 |> GraphicSVG.filled GraphicSVG.white |> GraphicSVG.move (7, 0)
    ,
    GraphicSVG.square 8 |> GraphicSVG.filled GraphicSVG.white |> GraphicSVG.move (-1, 0) 
  ] |> GraphicSVG.rotate pi |> GraphicSVG.move (x, y)

home x y =
  GraphicSVG.group[
    GraphicSVG.polygon [(5, 0), (-5, 0), (0, 4)] |> GraphicSVG.filled GraphicSVG.red |> GraphicSVG.move (0, 0.5)
    ,
    GraphicSVG.square 8 |> GraphicSVG.filled GraphicSVG.red |> GraphicSVG.move (0, -3) 
    ,
    GraphicSVG.rect 2 4 |> GraphicSVG.filled GraphicSVG.white |> GraphicSVG.move (0, -5) 
  ] |> GraphicSVG.move (x, y)

heart col = 
  GraphicSVG.group[
  GraphicSVG.curve (0, -15) [
      GraphicSVG.Pull (10, 0) (10, 0),
      GraphicSVG.Pull (5, 11) (0, 0),
      GraphicSVG.Pull (-5, 11) (-10, 0),
      GraphicSVG.Pull (0, -15) (0, -15)] 
        |> GraphicSVG.filled GraphicSVG.red |> GraphicSVG.addOutline (GraphicSVG.solid 1) GraphicSVG.red
  ] |> GraphicSVG.addOutline (GraphicSVG.solid 1) GraphicSVG.white

c = [1, 0, 3, 1, -1]
p = (formList 4)

formList x = 
    if x >= 0 then [x] ++ formList (x - 1)
    else []

cleanList x = 
  case x of 
      a :: rest -> if a /= 0 then [a] ++ cleanList rest else cleanList rest
      otherwise -> []

getSign x bool = 
  if x < 0  && bool == False then " - " else if x > 0 && bool == False then " + " else ""

valueString i = 
  if i == 1 then "" else String.fromInt (i)

printer x y mode coefs power fo terms addSign = 
  case coefs of 
    a :: rest ->
      case power of 
          b :: rest1 ->
            if mode == "center" then
                if a /= 0 && b /= 0 then
                    [
                      GraphicSVG.text (getSign a addSign) 
                        |> GraphicSVG.centered 
                        |> GraphicSVG.customFont "Comic Sans MS"
                        |> GraphicSVG.size fo  
                        |> GraphicSVG.filled GraphicSVG.black 
                        |> GraphicSVG.move (x - ((terms) / 2) * fo - fo , y)
                      ,
                      GraphicSVG.text (valueString(abs(a)) ++ "x") 
                        |> GraphicSVG.centered 
                        |> GraphicSVG.customFont "Comic Sans MS"
                        |> GraphicSVG.size fo  
                        |> GraphicSVG.filled GraphicSVG.black 
                        |> GraphicSVG.move (x - (terms / 2) * fo , y)
                      ,
                      GraphicSVG.text (if b > 1 then String.fromInt(b) else "") 
                        |> GraphicSVG.centered 
                        |> GraphicSVG.customFont "Comic Sans MS"
                        |> GraphicSVG.size (fo / 2) 
                        |> GraphicSVG.filled GraphicSVG.black 
                        |> GraphicSVG.move (x - ((terms) / 2) * fo + fo * (2 / 3), y + fo * 0.5)
                    ]
                    ++
                    printer (x + fo * 2) y "center" rest rest1 fo terms False
                    
                 else if b == 0 then
                    [
                      GraphicSVG.text (getSign a addSign) 
                        |> GraphicSVG.centered 
                        |> GraphicSVG.customFont "Comic Sans MS"
                        |> GraphicSVG.size fo  
                        |> GraphicSVG.filled GraphicSVG.black 
                        |> GraphicSVG.move (x - ((terms) / 2) * fo - fo , y)
                      ,
                      GraphicSVG.text (if a == 0 then "" else String.fromInt(abs a)) 
                        |> GraphicSVG.centered 
                        |> GraphicSVG.customFont "Comic Sans MS"
                        |> GraphicSVG.size fo  
                        |> GraphicSVG.filled GraphicSVG.black 
                        |> GraphicSVG.move (x - (terms / 2) * fo , y)
                    ]
                    ++
                    printer (x + fo * 2) y "center" rest rest1 fo terms False
                 else
                    printer x y "center" rest rest1 fo terms addSign 
            
       
           else printer (x + fo * 2) y "center" rest rest1 fo terms False
            
          otherwise -> [GraphicSVG.circle 0 |> GraphicSVG.filled GraphicSVG.white |> GraphicSVG.move (1000, 1000)]        
        
    otherwise -> [GraphicSVG.circle 0 |> GraphicSVG.filled GraphicSVG.white |> GraphicSVG.move (1000, 1000)]

derivative coefs ints = 
  case coefs of 
      a :: rest -> 
        case ints of 
          b :: rest1 -> [a * b] ++ derivative rest rest1
          otherwise -> []
   
            
      otherwise -> []

derivativePows ints = 
  case ints of 
    a :: rest -> 
      if a - 1 >= 0 then [a - 1] ++ derivativePows rest
      else [0] ++ derivativePows rest
    otherwise -> []

type Msg = Tick Float GraphicSVG.App.GetKeyState

type alias Model = { time : Float }

update msg model = case msg of
                     Tick t _ -> { time = t }

init = { time = 0 }

main = GraphicSVG.App.gameApp Tick { model = init, view = view, update = update, title = "Game Slot" }

view model = GraphicSVG.collage 192 128 (myShapes model)




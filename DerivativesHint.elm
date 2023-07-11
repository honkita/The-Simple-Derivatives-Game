module Elm.DerivativesHint exposing (..)
import Html exposing (Html)
import GraphicSVG
import GraphicSVG.App
import Collage
import Color
import Time 

--inve : Color -> Bool -> Color
inve colour invert blind = 
    if invert && blind then
        case colour of 
            Red -> NeonPink
            Charcoal -> White
            White -> Charcoal
            Green -> Purple
            Blue -> Blue
            Purple -> Purple
            Orange -> Orange
            NeonPink -> NeonPink
            
     else if invert then
        case colour of 
            Red -> Red
            Charcoal -> White
            White -> Charcoal
            Green -> Green
            Blue -> Blue
            Purple -> Purple
            Orange -> Orange
            NeonPink -> NeonPink
     
     else if blind then
        case colour of 
            Red -> NeonPink
            Charcoal -> Charcoal
            White -> White
            Green -> Purple
            Blue -> Blue
            Purple -> Purple
            Orange -> Charcoal
            NeonPink -> NeonPink
      
      else 
          case colour of 
            Orange -> Charcoal
            otherwise -> colour
colours c = 
  case c of 
      Red -> (GraphicSVG.rgb 200 0 0)
      Charcoal -> (GraphicSVG.rgb 28 28 28)
      White -> (GraphicSVG.rgb 245 245 245)
      Green -> GraphicSVG.green
      Blue -> GraphicSVG.blue
      Purple -> GraphicSVG.purple
      Orange -> (GraphicSVG.rgb 221 127 29)
      NeonPink -> (GraphicSVG.rgb 209 49 127)
      

type ChangeColour = 
    Charcoal
    | White
    | Green
    | Red 
    | Blue 
    | Purple 
    | Orange
    | NeonPink

calculator coefs expo start = 
  case coefs of 
    a :: rest -> 
      if expo >= 0 then toFloat a * start ^ expo + calculator rest (expo - 1) start
      else toFloat a * start ^ expo  
    otherwise -> 0

grid model = 
  GraphicSVG.group[
      GraphicSVG.rect 1.5 90
        |> GraphicSVG.filled (colours (inve Charcoal model.dark model.colourBlind))
        |> GraphicSVG.move (offsetx, offsety)
      ,
       GraphicSVG.rect 90 1.5 
        |> GraphicSVG.filled (colours (inve Charcoal model.dark model.colourBlind))
        |> GraphicSVG.move (offsetx, offsety)
     
  ]

box model = 
  GraphicSVG.group[
       GraphicSVG.square 90
        |> GraphicSVG.outlined (GraphicSVG.solid 3) (colours (inve Charcoal model.dark model.colourBlind))
        |> GraphicSVG.move (offsetx, offsety)
  ]
  
grapherN coefs expo start model c =
    GraphicSVG.group[
      printcircle coefs expo start model c
      
    ]

  

l = 15

offsetx = -45
offsety = 0
printcircle coefs expo start model c= 
    GraphicSVG.openPolygon (points coefs expo start)
        |> GraphicSVG.outlined (GraphicSVG.solid 3) (colours (inve c model.dark model.colourBlind))


points coefs expo start =
  if start * l < 45 then
    let p = (calculator coefs expo start) in
        if p * l <= 45 && p * l >= -45 then
          [(start * l + offsetx, p * l + offsety)] ++ points coefs expo (start + 0.1)
        else if p * l >= 45 then
          [(start * l + offsetx, 45)] ++ points coefs expo (start + 0.1)
        else if p * l <= -45 then
          [(start * l + offsetx, -45)] ++ points coefs expo (start + 0.1)
        else points coefs expo (start + 0.1)
  else []

  
myShapes model =
  [
    case model.state of 
        Hint -> 
            GraphicSVG.group [
                GraphicSVG.square 1000 |> GraphicSVG.filled (colours (inve White model.dark model.colourBlind))
                , pageText model.pageInt model
                ,
                (button model.pageInt model) |> GraphicSVG.move (0, -55)
                , page0 model.pageInt model
                , page1 model.pageInt model
                , page2 model.pageInt model
                , page3 model.pageInt model
                , page4 model.pageInt model
                , page5 model.pageInt model
                ,
                GraphicSVG.group[
                  
                  grid model
                  ,
                  display model |> GraphicSVG.group
                  
                  ,
                  box model
                ]
               
                ,
                values model.currentx model.currenty model |> GraphicSVG.group
                
                
                , 
                GraphicSVG.group[
                    GraphicSVG.roundedRect 30 10 5 |> GraphicSVG.filled (colours (inve White model.dark model.colourBlind)) 
                        |> GraphicSVG.addOutline (GraphicSVG.solid 1) (colours (inve Orange model.dark model.colourBlind)) |> GraphicSVG.move (20, 0)
                    , GraphicSVG.text "Exit" |> GraphicSVG.customFont "Comic Sans MS" |> GraphicSVG.centered |> GraphicSVG.size 6 |> GraphicSVG.filled (colours (inve Orange model.dark model.colourBlind)) |> GraphicSVG.move (20, -2)
                ] |> GraphicSVG.move (0, -55) |> GraphicSVG.notifyTap Exit
            ] |> GraphicSVG.notifyMouseUp Exited 
            
         
        NoHint -> 
            GraphicSVG.group [
            ]
  ]
  
  ++
  if model.pageInt >= 5 && model.state == Hint then 
  [
    grapherN [4, 0, 10, 0, 0] 3 (-45 / l) model Blue
    ,
    display model |> GraphicSVG.group
    ,
    box model 
  ]
  else 
  []
 
values x y model = 
  if model.exit == False then 
      [
        GraphicSVG.text ("x: " ++ String.fromFloat(truncate((x - offsetx)/l)))
        |> GraphicSVG.customFont "Comic Sans MS"
        |> GraphicSVG.size 6
        |> GraphicSVG.filled (colours (inve Orange model.dark model.colourBlind))
        |> GraphicSVG.move (-85, -24)
        ,
        GraphicSVG.text ("y: " ++ String.fromFloat(truncate(calculator [1, 0, 5, 0, -1] 4 ((x - offsetx)/l))))
        |> GraphicSVG.customFont "Comic Sans MS"
        |> GraphicSVG.size 6
        |> GraphicSVG.filled (colours (inve Orange model.dark model.colourBlind))
        |> GraphicSVG.move (-85, -32)
      ]
      ++
      if model.pageInt >= 5 then 
          [
            GraphicSVG.text ("Slope: " ++ String.fromFloat(truncate(calculator [4, 0, 10, 0, 0] 3 ((x - offsetx)/l))))
              |> GraphicSVG.customFont "Comic Sans MS"
              |> GraphicSVG.size 6
              |> GraphicSVG.filled (colours (inve Orange model.dark model.colourBlind))
              |> GraphicSVG.move (-85, -40)
          ]
     else []
  else []
  

display model = 
  case model.state of 
      Hint -> 
        let p = (grapherN [1, 0, 5, 0, -1] 4 (-45 / l) model Red)
            q = (points [1, 0, 5, 0, -1] 4 (-45 / l)) in
            q
            |> List.map2
            ( \ x y -> x |> GraphicSVG.notifyMouseDownAt (Mouse y) 
            ) [p]
      otherwise -> []
        
arrow x y model =
  GraphicSVG.group[
      GraphicSVG.rect 4 1 |> GraphicSVG.filled (colours (inve Charcoal model.dark model.colourBlind))
      ,
      GraphicSVG.triangle 2 |> GraphicSVG.filled (colours (inve Charcoal model.dark model.colourBlind)) |> GraphicSVG.move (2, 0) 
  ] |> GraphicSVG.rotate (3*pi/2) |> GraphicSVG.move (x, y)

truncate val = toFloat(round (val * 100)) / 100

pageText int model =
  case int of
    0 -> GraphicSVG.group[ 
            GraphicSVG.text "Suppose you have the function listed here" 
              |> GraphicSVG.customFont "Comic Sans MS" 
              |> GraphicSVG.size 6 
              |> GraphicSVG.filled (colours (inve Charcoal model.dark model.colourBlind)) 
              |> GraphicSVG.move (-90, 50)
              ]
    1 -> GraphicSVG.group[
              GraphicSVG.text "GraphicSVG.move the exponent down and multiply by the coefficient"  
                |> GraphicSVG.customFont "Comic Sans MS" 
                |> GraphicSVG.size 6 
                |> GraphicSVG.filled (colours (inve Charcoal model.dark model.colourBlind)) 
                |> GraphicSVG.move (-90, 50)
              ]
    2 -> GraphicSVG.group[
              GraphicSVG.text "Subtract 1 from the exponent"  
               |> GraphicSVG.customFont "Comic Sans MS" 
               |> GraphicSVG.size 6 
               |> GraphicSVG.filled (colours (inve Charcoal model.dark model.colourBlind)) 
               |> GraphicSVG.move (-90, 50)
              ]
    3 -> GraphicSVG.group[
              GraphicSVG.text "Simplify"  
                |> GraphicSVG.customFont "Comic Sans MS" 
                |> GraphicSVG.size 6 
                |> GraphicSVG.filled (colours (inve Charcoal model.dark model.colourBlind)) 
                |> GraphicSVG.move (-90, 50)
            
              ]
    4 -> GraphicSVG.group[
              GraphicSVG.text "For constants, the derivative will be 0" 
                |> GraphicSVG.customFont "Comic Sans MS" 
                |> GraphicSVG.size 6 
                |> GraphicSVG.filled (colours (inve Charcoal model.dark model.colourBlind)) 
                |> GraphicSVG.move (-90, 50)
              ]
    5 -> GraphicSVG.group[
              GraphicSVG.text "Finally, simplify the equation again to get the derivative"
                |> GraphicSVG.customFont "Comic Sans MS" 
                |> GraphicSVG.size 6 
                |> GraphicSVG.filled (colours (inve Charcoal model.dark model.colourBlind)) 
                |> GraphicSVG.move (-90, 50)
              ]
    6 -> GraphicSVG.group[
              GraphicSVG.text "The derivative is the slope at any given point on the original function"
                |> GraphicSVG.customFont "Comic Sans MS" 
                |> GraphicSVG.size 5 
                |> GraphicSVG.filled (colours (inve Charcoal model.dark model.colourBlind)) 
                |> GraphicSVG.move (-90, 50)
              ]
    otherwise -> GraphicSVG.group []


page0 int model =
    if int >= 0 then 
        GraphicSVG.group[ 

            printer 50 35 "left" [1, 0, 5, 0, -1] [4, 3, 2, 1, 0] 6 5 True model |> GraphicSVG.group 
        ]
    else GraphicSVG.group []
    
page1 int model =
    if int >= 1 then 
        GraphicSVG.group[ 
          GraphicSVG.oval 5 7.5 |> GraphicSVG.outlined (GraphicSVG.solid 1) (colours (inve Red model.dark model.colourBlind)) |> GraphicSVG.makeTransparent 0.75 |> GraphicSVG.move (62, 37) 
          ,
          GraphicSVG.oval 5 7.5 |> GraphicSVG.outlined (GraphicSVG.solid 1) (colours (inve Red model.dark model.colourBlind)) |> GraphicSVG.makeTransparent 0.75 |> GraphicSVG.move (47, 37) 
          ,
          GraphicSVG.text "Coefficient" |> GraphicSVG.size 4 |> GraphicSVG.customFont "Comic Sans MS" |> GraphicSVG.filled (colours (inve Red model.dark model.colourBlind)) |> GraphicSVG.move (20, 38) 
          ,
          GraphicSVG.text "Exponents" |> GraphicSVG.size 4 |> GraphicSVG.customFont "Comic Sans MS" |> GraphicSVG.filled (colours (inve Blue model.dark model.colourBlind)) |> GraphicSVG.move (20, 33) 
          ,
          GraphicSVG.text "1" |> GraphicSVG.size 6 |> GraphicSVG.customFont "Comic Sans MS" |> GraphicSVG.filled (colours (inve Red model.dark model.colourBlind)) |> GraphicSVG.move (45, 35) 
          ,
          GraphicSVG.circle 2 |> GraphicSVG.outlined (GraphicSVG.solid 1) (colours (inve Blue model.dark model.colourBlind)) |> GraphicSVG.makeTransparent 0.75 |> GraphicSVG.move (68, 39)
          ,
          GraphicSVG.circle 2 |> GraphicSVG.outlined (GraphicSVG.solid 1) (colours (inve Blue model.dark model.colourBlind)) |> GraphicSVG.makeTransparent 0.75 |> GraphicSVG.move (53, 39)
          ,
          printer 30 25 "left" [4] [4] 6 1 True model |> GraphicSVG.group 
          ,
          GraphicSVG.text "+"
                |> GraphicSVG.size 6
                |> GraphicSVG.customFont "Comic Sans MS"
                |> GraphicSVG.filled (colours (inve Charcoal model.dark model.colourBlind))
                |> GraphicSVG.move (38, 25)
           ,
           GraphicSVG.text "(2 ⋅ 5)x"
                |> GraphicSVG.size 6
                |> GraphicSVG.customFont "Comic Sans MS"
                |> GraphicSVG.filled (colours (inve Charcoal model.dark model.colourBlind))
                |> GraphicSVG.move (42, 25)
           ,
           GraphicSVG.text "2"
                |> GraphicSVG.size 3
                |> GraphicSVG.customFont "Comic Sans MS"
                |> GraphicSVG.filled (colours (inve Charcoal model.dark model.colourBlind))
                |> GraphicSVG.move (63, 28)
           ,
           printer 72 25 "left" [-1] [0] 6 1 True model |> GraphicSVG.group 
           
            
        ]
    else GraphicSVG.group []
    
page2 int model= 
    if int >= 2 then 
        GraphicSVG.group[ 
          arrow 50 20 model 
          ,
          GraphicSVG.text "4x"
                |> GraphicSVG.size 6
                |> GraphicSVG.customFont "Comic Sans MS"
                |> GraphicSVG.filled (colours (inve Charcoal model.dark model.colourBlind))
                |> GraphicSVG.move (30, 8) 
          ,
          GraphicSVG.text "4 - 1"
                |> GraphicSVG.size 3
                |> GraphicSVG.customFont "Comic Sans MS"
                |> GraphicSVG.filled (colours (inve Charcoal model.dark model.colourBlind))
                |> GraphicSVG.move (37, 11)
           ,
          GraphicSVG.text "+"
                |> GraphicSVG.size 6
                |> GraphicSVG.customFont "Comic Sans MS"
                |> GraphicSVG.filled (colours (inve Charcoal model.dark model.colourBlind))
                |> GraphicSVG.move (44, 8)
           ,
           GraphicSVG.text "10x"
                |> GraphicSVG.size 6
                |> GraphicSVG.customFont "Comic Sans MS"
                |> GraphicSVG.filled (colours (inve Charcoal model.dark model.colourBlind))
                |> GraphicSVG.move (50, 8)
           ,
           GraphicSVG.text "2 - 1"
                |> GraphicSVG.size 3
                |> GraphicSVG.customFont "Comic Sans MS"
                |> GraphicSVG.filled (colours (inve Charcoal model.dark model.colourBlind))
                |> GraphicSVG.move (60, 11)
           ,
           printer 75 8 "left" [-1] [0] 6 1 True model |> GraphicSVG.group  

        ]
    else GraphicSVG.group []

page3 int model= 
    if int >= 3 then 
        GraphicSVG.group[ 
          arrow 50 3 model 
          ,     
          printer 35 -6 "left" [4, 0, 10, -1, 0] [3, 2, 1, 0, 0] 6 5 True model |> GraphicSVG.group 
        ]
    else GraphicSVG.group []

page4 int model= 
    if int >= 4 then 
        GraphicSVG.group[ 
            arrow 50 -11 model 
            ,
            printer 35 -20 "left" [4, 0, 10, 0, 0] [3, 2, 1, 0, 0] 6 5 True model |> GraphicSVG.group 
            ,
            GraphicSVG.text "- 0"
                |> GraphicSVG.size 6
                |> GraphicSVG.customFont "Comic Sans MS"
                |> GraphicSVG.filled (colours (inve Charcoal model.dark model.colourBlind))
                |> GraphicSVG.move (58, -20)
           
        ]
    else GraphicSVG.group []

page5 int model= 
    if int >= 5 then 
        GraphicSVG.group[ 
            arrow 50 -25 model 
            ,
            printer 40 -35 "left" [4, 0, 10, 0, 0] [3, 2, 1, 0, 0] 6 5 True model |> GraphicSVG.group 
        ]
    else GraphicSVG.group []


button i model = 
    if i < 6 then
        GraphicSVG.group
        [GraphicSVG.roundedRect 30 10 5 |> GraphicSVG.filled (colours (inve White model.dark model.colourBlind)) 
            |> GraphicSVG.addOutline (GraphicSVG.solid 1) (colours (inve Orange model.dark model.colourBlind)) |> GraphicSVG.move (80, 0)
        , GraphicSVG.text "Next" |> GraphicSVG.customFont "Comic Sans MS" |> GraphicSVG.centered |> GraphicSVG.size 6 |> GraphicSVG.filled (colours (inve Orange model.dark model.colourBlind)) |> GraphicSVG.move (80, -2)
        ] |> GraphicSVG.notifyTap Next
    else 
        GraphicSVG.group
        [GraphicSVG.roundedRect 30 10 5 |> GraphicSVG.filled (colours (inve White model.dark model.colourBlind))
            |> GraphicSVG.addOutline (GraphicSVG.solid 1) (colours (inve Orange model.dark model.colourBlind)) |> GraphicSVG.move (80, 0)
        , GraphicSVG.text "Restart" |> GraphicSVG.customFont "Comic Sans MS" |> GraphicSVG.centered |> GraphicSVG.size 6 |> GraphicSVG.filled (colours (inve Orange model.dark model.colourBlind)) |> GraphicSVG.move (80, -2)
        ] |> GraphicSVG.notifyTap Restart
        


getSign x bool = 
  if x < 0 && bool == True then " -"
  else if x < 0 && bool == False then " - " 
  else if x > 0 && bool == False then " + " 
  else ""

valueString i = 
  if i == 1 then "" else String.fromInt (i)

printer x y mode coefs power fo terms addSign model = 
  case coefs of 
    a :: rest ->
      case power of 
          c :: rest1 ->
               if a /= 0 && c /= 0 then
                    [
                      GraphicSVG.text (getSign a addSign) 
                        |> GraphicSVG.centered 
                        |> GraphicSVG.customFont "Comic Sans MS"
                        |> GraphicSVG.size fo  
                        |> GraphicSVG.filled (colours (inve Charcoal model.dark model.colourBlind))
                        |> GraphicSVG.move ((x - toFloat(String.length(valueString(abs(a)) ++ "x")) - fo * 0.75) , y)
                      ,
                      GraphicSVG.text (valueString(abs(a)) ++ "x") 
                        |> GraphicSVG.centered 
                        |> GraphicSVG.customFont "Comic Sans MS"
                        |> GraphicSVG.size fo  
                        |> GraphicSVG.filled (colours (inve Charcoal model.dark model.colourBlind))
                        |> GraphicSVG.move (x, y)
                      ,
                      GraphicSVG.text (if c > 1 then String.fromInt(c) else "") 
                        |> GraphicSVG.customFont "Comic Sans MS"
                        |> GraphicSVG.size (fo / 2) 
                        |> GraphicSVG.filled (colours (inve Charcoal model.dark model.colourBlind))
                        |> GraphicSVG.move 
                        (x + toFloat(String.length(valueString(abs(a)) ++ "x")) / 3 * fo, y + fo * 0.5)
                    ]
                    ++
                    printer (x + toFloat(String.length(valueString(abs(a)) ++ "x"))* 1.5 + 2 * fo) 
                      y "left" rest rest1 fo terms False model
                    
                 else if c == 0 then
                    [
                      GraphicSVG.text (getSign a addSign) 
                        |> GraphicSVG.centered 
                        |> GraphicSVG.customFont "Comic Sans MS"
                        |> GraphicSVG.size fo  
                        |> GraphicSVG.filled (colours (inve Charcoal model.dark model.colourBlind))
                        |> GraphicSVG.move (x - fo, y)
                      ,
                      GraphicSVG.text ((if a == 0 then "" else String.fromInt(abs a))) 
                        |> GraphicSVG.centered 
                        |> GraphicSVG.customFont "Comic Sans MS"
                        |> GraphicSVG.size fo  
                        |> GraphicSVG.filled (colours (inve Charcoal model.dark model.colourBlind))
                        |> GraphicSVG.move (x, y)
                     
                    ]
                    ++
                    printer (x + fo ) y "left" rest rest1 fo terms False model
                 else
                    printer x y "left" rest rest1 fo terms addSign model
      
                
            
          otherwise -> [GraphicSVG.circle 0 |> GraphicSVG.filled GraphicSVG.white |> GraphicSVG.move (1000, 1000)]        
        
    otherwise -> [GraphicSVG.circle 0 |> GraphicSVG.filled GraphicSVG.white |> GraphicSVG.move (1000, 1000)]




type State = Hint
            | NoHint

type Msg = Tick Float GraphicSVG.App.GetKeyState
          | Next
          | Restart
          | Mouse (Float, Float) (Float,Float)
          | Exit
          | Exited 

type alias Model = { time : Float
                   , pageInt : Int
                   , state : State
                   , dark : Bool
                   , colourBlind : Bool
                   , currentx : Float
                   , currenty : Float
                   , exit : Bool}

update : Msg -> Model -> Model
update msg model = case msg of
                     Tick t _ -> { model | time = t }
                     Next ->  { model | pageInt = model.pageInt + 1 }
                     Restart ->  { model | pageInt = 0}
                     Mouse (x, y) (x1, y1) -> { model | currentx = x1
                                                      , currenty = y1
                                                      , exit = False}
                     Exited -> { model | exit = True }                    
                     Exit ->  { model | state = NoHint}
init : Model
init = { time = 0
        , pageInt = 0
        , state = Hint
        , dark = False
        , colourBlind = False
        , currentx = 0
        , currenty = 0
        , exit = True }

b = { time = 0
        , pageInt = 0
        , state = Hint
        , dark = False
        , colourBlind = True
        , currentx = 0
        , currenty = 0
        , exit = True }

d = { time = 0
        , pageInt = 0
        , state = Hint
        , dark = True
        , colourBlind = False
        , currentx = 0
        , currenty = 0
        , exit = True }

porquenolosdos = { time = 0
        , pageInt = 0
        , state = Hint
        , dark = True
        , colourBlind = True
        , currentx = 0
        , currenty = 0
        , exit = True }

main = GraphicSVG.App.gameApp Tick { model = init, view = view, update = update, title = "Game Slot" }

view model = GraphicSVG.collage 192 128 (myShapes model)




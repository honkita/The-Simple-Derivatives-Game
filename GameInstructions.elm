module GameInstructions exposing (..)
import Html exposing (Html)
import GraphicSVG
import GraphicSVG.App
import Collage
import Color
import Time 


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


myShapes model =
  if model.state == Play || model.state == Freeplay then
    [
      GraphicSVG.square 1000 |> GraphicSVG.filled (colours (inve White model.dark model.colourBlind))
      ,
      GraphicSVG.text (texter model)
        |> GraphicSVG.centered
        |> GraphicSVG.customFont "Comic Sans MS"
        |> GraphicSVG.filled (colours (inve Charcoal model.dark model.colourBlind))
        |> GraphicSVG.move (0, 40)
      ,
      GraphicSVG.text ("Pick the correct derivative on the side")
        |> GraphicSVG.centered
        |> GraphicSVG.size 8
        |> GraphicSVG.customFont "Comic Sans MS"
        |> GraphicSVG.filled (colours (inve Charcoal model.dark model.colourBlind))
        |> GraphicSVG.move (0, 28)
      ,
      GraphicSVG.text (texter1 model)
        |> GraphicSVG.centered
        |> GraphicSVG.size 8
        |> GraphicSVG.customFont "Comic Sans MS"
        |> GraphicSVG.filled (colours (inve Charcoal model.dark model.colourBlind))
        |> GraphicSVG.move (0, 16)

      ,
      GraphicSVG.text ("Click on the main function for the x and y ")
        |> GraphicSVG.centered
        |> GraphicSVG.size 6
        |> GraphicSVG.customFont "Comic Sans MS"
        |> GraphicSVG.filled (colours (inve Charcoal model.dark model.colourBlind))
        |> GraphicSVG.move (0, 4)
      ,
      GraphicSVG.text ("coordinates along with the slope after answering the question")
        |> GraphicSVG.centered
        |> GraphicSVG.size 6
        |> GraphicSVG.customFont "Comic Sans MS"
        |> GraphicSVG.filled (colours (inve Charcoal model.dark model.colourBlind))
        |> GraphicSVG.move (0, -2)
      ,
      GraphicSVG.group[
      GraphicSVG.roundedRect 30 10 5 |> GraphicSVG.filled (colours (inve White model.dark model.colourBlind)) 
      |> GraphicSVG.addOutline (GraphicSVG.solid 1) (colours (inve Orange model.dark model.colourBlind)) |> GraphicSVG.move (0, 0)
      , GraphicSVG.text "Back" |> GraphicSVG.customFont "Comic Sans MS" |> GraphicSVG.centered |> GraphicSVG.size 6 |> GraphicSVG.filled (colours (inve Orange model.dark model.colourBlind)) |> GraphicSVG.move (0, -2)
      ] |> GraphicSVG.move (-55, -55) |> GraphicSVG.notifyTap Exit
      ,
      GraphicSVG.group[
      GraphicSVG.roundedRect 30 10 5 |> GraphicSVG.filled (colours (inve White model.dark model.colourBlind)) 
      |> GraphicSVG.addOutline (GraphicSVG.solid 1) (colours (inve Orange model.dark model.colourBlind)) |> GraphicSVG.move (0, 0)
      , GraphicSVG.text "Next" |> GraphicSVG.customFont "Comic Sans MS" |> GraphicSVG.centered |> GraphicSVG.size 6 |> GraphicSVG.filled (colours (inve Orange model.dark model.colourBlind)) |> GraphicSVG.move (0, -2)
      ] |> GraphicSVG.move (55, -55) |> GraphicSVG.notifyTap Next
    ]
  else []

texter model = 
  if model.state == Play then "This is normal play mode"
  else "This is freeplay"

texter1 model = 
  if model.state == Play then "You have 3 lives"
  else "You can do as many questions as you can"


type Msg = Tick Float GraphicSVG.App.GetKeyState
          | Exit
          | Next

type alias Model = { time : Float
                   , state : State
                   , dark : Bool
                   , colourBlind : Bool
                   , currentx : Float
                   , currenty : Float}

update msg model = case msg of
                     Tick t _ -> {model | time = t }                 
                     Exit ->  { model | state = Back}
                     Next ->  { model | state = Done}

init : Model
init = { time = 0
        , state = Play
        , dark = False
        , colourBlind = False
        , currentx = 0
        , currenty = 0}

init1 = { time = 0
        , state = Freeplay
        , dark = False
        , colourBlind = False
        , currentx = 0
        , currenty = 0}

b = { time = 0
        , state = Play
        , dark = False
        , colourBlind = True
        , currentx = 0
        , currenty = 0}

b1 = { time = 0
        , state = Freeplay
        , dark = False
        , colourBlind = True
        , currentx = 0
        , currenty = 0}
        
d = { time = 0
        , state = Play
        , dark = True
        , colourBlind = False
        , currentx = 0
        , currenty = 0}

d1 = { time = 0
        , state = Freeplay
        , dark = True
        , colourBlind = False
        , currentx = 0
        , currenty = 0}

porquenolosdos = { time = 0
        , state = Play
        , dark = True
        , colourBlind = True
        , currentx = 0
        , currenty = 0}

porquenolosdos1 = { time = 0
        , state = Freeplay
        , dark = True
        , colourBlind = True
        , currentx = 0
        , currenty = 0}

type State = Play
            | Freeplay
            | Done
            | Back

view model = GraphicSVG.collage 192 128 (myShapes model)

main = GraphicSVG.App.gameApp Tick { model = init, view = view, update = update, title = "Game Slot" }






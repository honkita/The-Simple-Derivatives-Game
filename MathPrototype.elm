-- using elm/random
-- https://package.elm-lang.org/packages/elm/random/1.0.0/

module MathPrototype exposing (Model)
import MathLatex exposing (..)
import DerivativesHint
import GameInstructions
import GraphicSVG
import GraphicSVG.EllieApp exposing (..)
import GraphicSVG.App
import Html exposing (Html)
import Random

myTitle = "Prototype"


type alias Model = { time : Float
                   , debug : String
                   , correct : Int
                   , wrong : Int
                   , question : Question
                   , randCoefs : List Int
                   , displayAnswers : List Equation
                   , questionInt : Int
                   , state : State
                   , showAnswer : Bool
                   , freeplay : Bool
                   , derivativesHint : DerivativesHint.Model
                   , instructions : GameInstructions.Model
                   , colourBlind : Bool
                   , dark : Bool
                   , right : Bool
                   , prev : State
                   , currentx : Float
                   , currenty : Float
                   , exit : Bool
                   , zoom : Float
                   }

type Equation = E Int (List Int)

init = { time = 0
       , debug = ""
       , correct = 0
       , wrong = 0
       , question = Q "" 0 answerGenD
       , randCoefs = []
       , displayAnswers = []
       , questionInt = 0
       , state = Home
       , showAnswer = False
       , freeplay = False
       , derivativesHint = DerivativesHint.init
       , instructions = GameInstructions.init
       , colourBlind = False
       , dark = True
       , right = False
       , prev = Home
       , currentx = 0
       , currenty = 0
       , exit = True
       , zoom = 3
       }

type State = Home
            | Question 
            | Complete
            | Results
            | Hint
            | Settings
            | Tutorial

--question bank
type Question = 
  Q String Int (List Int -> Int -> Bool -> List Equation)

--Question start, number of random integers needed, list of other integers,
--function to print question, function to generate answer

questions = 
  [
    Q "What is the derivative of " 5 answerGenD
  ]
  
 
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



type Msg = Tick Float GetKeyState
         | Choice String
         | Questions Int 
         | Generate (List Int)
         | Answer Equation
         | Store (List Int)
         | GenAnswer
         | Hints
         | DerivativesHint DerivativesHint.Msg
         | Instructions GameInstructions.Msg
         | CSwap String
         | ChangeSettings
         | ToQuestion String
         | Restart String
         | ToHome
         | Next
         | Exited
         | Mouse (Float, Float) (Float,Float)
         | Zoom Int
         | Tut Bool


positivecheck x t = 
  if x < 0 then " - " ++ String.fromInt(abs x)
  else if t == True then String.fromInt(abs x)
  else " + " ++ String.fromInt(x)


    
answerGenD rand pow t =
  [ E (pow - 1) (derivativePow rand (formList pow)) ] 
  ++ [ E pow (derivativePow rand (formList (pow - 1))) ] 
  ++ [ E (pow - 1) (derivativePow rand ([1,1,1,1,0]))]


listOnes n p = 
  if n >= 0 then [1] ++ listOnes (n-1) p
  else []

calculator coefs expo start = 
  case coefs of 
    a :: rest -> 
      if expo > 0 then toFloat a * start ^ expo + calculator rest (expo - 1) start
      else toFloat a * start ^ expo  
    otherwise -> 0


grid model = 
  GraphicSVG.group[
      GraphicSVG.rect 0.5 80
        |> GraphicSVG.filled (colours (inve Charcoal model.dark model.colourBlind))
        |> GraphicSVG.move (offsetx, offsety)
      ,
       GraphicSVG.rect 80 0.5
        |> GraphicSVG.filled (colours (inve Charcoal model.dark model.colourBlind))
        |> GraphicSVG.move (offsetx, offsety)
     
  ]

box model = 
  GraphicSVG.group[
       GraphicSVG.square 80
        |> GraphicSVG.outlined (GraphicSVG.solid 3) (colours (inve Charcoal model.dark model.colourBlind))
        |> GraphicSVG.move (offsetx, offsety)
  ]
  
grapherN coefs expo start model c =
    GraphicSVG.group[
      printcircle coefs expo start model c
      
    ]

values model = 
  if model.exit == False then 
      [
        GraphicSVG.text ("x: " ++ String.fromFloat(truncate((model.currentx - offsetx)/model.zoom)))
        |> GraphicSVG.customFont "Comic Sans MS"
        |> GraphicSVG.size 5
        |> GraphicSVG.filled (colours (inve Orange model.dark model.colourBlind))
        |> GraphicSVG.move (15, -24)
        ,
        GraphicSVG.text ("y: " ++ String.fromFloat(truncate(calculator model.randCoefs 4 ((model.currentx - offsetx)/model.zoom))))
        |> GraphicSVG.customFont "Comic Sans MS"
        |> GraphicSVG.size 5
        |> GraphicSVG.filled (colours (inve Orange model.dark model.colourBlind))
        |> GraphicSVG.move (15, -32)
      ]
      ++
      if model.showAnswer == True then 
          [
            GraphicSVG.text ("Slope: " ++ String.fromFloat(truncate(calculator (derivativePow model.randCoefs (formList 4)) 3 ((model.currentx - offsetx)/l))))
              |> GraphicSVG.customFont "Comic Sans MS"
              |> GraphicSVG.size 5
              |> GraphicSVG.filled (colours (inve Orange model.dark model.colourBlind))
              |> GraphicSVG.move (15, -40)
          ]
     else []
  else []  

truncate val = toFloat(round (val * 100)) / 100

display model = 
  case model.state of 
      Question -> 
        let p = [
                  grapherN model.randCoefs 4 (-40 / model.zoom) model White
                    |> GraphicSVG.addOutline (GraphicSVG.solid 1) (colours (inve White model.dark model.colourBlind))
                  ,
                  grapherN model.randCoefs 4 (-40 / model.zoom) model Red 
                 ]
            q = (points model.randCoefs 4 (-40 / model.zoom) model) in
            q
            |> List.map2
            ( \ x y -> x |> GraphicSVG.notifyMouseDownAt (Mouse y) 
            ) p
      otherwise -> []


l = 2

offsetx = 50
offsety = -10
printcircle coefs expo start model c = 
    GraphicSVG.openPolygon (points coefs expo start model)
        |> GraphicSVG.outlined (GraphicSVG.solid (model.zoom / 3)) (colours (inve c model.dark model.colourBlind))


points coefs expo start model =
  if start * model.zoom < 40 then
    let p = (calculator coefs expo start) in
        if p * model.zoom <= 40 && p * model.zoom >= -40 then
          [(start * model.zoom + offsetx, p * model.zoom + offsety)] ++ points coefs expo (start + 0.1) model
        else if p * model.zoom >= 40 then
          [(start * model.zoom + offsetx, 40 + offsety)] ++ points coefs expo (start + 0.1) model
        else if p * model.zoom <= -40 then
          [(start * model.zoom + offsetx, -40 + offsety)] ++ points coefs expo (start + 0.1) model
        else points coefs expo (start + 0.1) model
  else []

-- function to show the message to help us understand our program
toString msg = case msg of
                  Tick t _ -> "Tick "++ String.fromFloat t
                  Choice c -> c
                  Questions q -> ""
                  Generate p -> "" 
                  Answer a -> ""
                  Store s -> ""
                  Hints -> ""
                  GenAnswer -> ""
                  DerivativesHint d -> ""
                  CSwap d -> ""
                  ChangeSettings -> ""
                  ToQuestion x -> ""
                  otherwise -> ""

pr list = 
  case list of 
      a :: rest -> String.fromInt (a) ++ " " ++ pr rest
      otherwise -> ""
      
--gets the question from the list of questions
getQ n list = 
  let con = n > 0
  in
  case list of
    (a :: rest) ->
      case con of 
        True -> getQ (n - 1) rest
        False -> a
    _ -> 
      let p = List.isEmpty questions
        in 
        case p of
          True -> Q "" 0 answerGenD
          False -> getQ n questions

update : Msg -> Model -> ( Model, Cmd Msg )
update msg m =
 let model = case msg of
               Tick _ _ -> m
               _ -> { m | debug = toString msg }
 in
  case msg of
    Tick t _ ->
      ( 
      { model | time = t }
      , Cmd.none )
    ToHome ->
      ( 
      { model | state = Home }
      , Cmd.none )
    Questions question -> 
      (
      { model | question = getQ question questions
              , questionInt = 0}
      , 
      let (Q a b c) = getQ question questions
      in generateVals b) 
    Restart a ->
      (
       {model | time = 0
       , debug = ""
       , correct = 0
       , wrong = 0
       , question = Q "" 0 answerGenD
       , randCoefs = []
       , displayAnswers = []
       , questionInt = 0
       , freeplay = if a == "Freeplay" then True else if a == "Play" then False else model.freeplay
       , state = Question
       , showAnswer = False
       , derivativesHint = DerivativesHint.init
       , right = False
       , prev = Home
       , zoom = 3
       }
      , generate (List.length questions))
    Generate q->
      (
      { model | randCoefs = q
      }
      , if q == [0,0,0,0,0] then generate (List.length questions) else generateAnswers (List.length q)) 
    
    Choice c -> 
      (model, generate (List.length questions))
    
    Store vals -> 
      
      ({model | displayAnswers = let (Q ques coef fun) = model.question in
                shuffle vals (fun model.randCoefs (coef - 1) True)
                
                }, Cmd.none)
    ChangeSettings ->
      ({model | state = Settings
                , prev = model.state }, Cmd.none)
    
    Hints ->
      ({model | state = Hint
              , prev = model.state
              , derivativesHint = if model.dark == True && model.colourBlind == True then DerivativesHint.porquenolosdos
                                  else if model.dark == True then DerivativesHint.d
                                  else if model.colourBlind == True then DerivativesHint.b
                                  else DerivativesHint.init
                
                }, Cmd.none)
    ToQuestion c ->
      ({model | state = model.prev
                
                }, if c == "Settings" then Cmd.none else generate (List.length questions))
    GenAnswer ->
      ({model | showAnswer = True
                
                }, Cmd.none)
      
    
    DerivativesHint d -> 
                let 
                  newModel = DerivativesHint.update d model.derivativesHint
                  
                in
                  ( { model | derivativesHint = if newModel.state == DerivativesHint.NoHint then DerivativesHint.init
                                        else newModel
                            , state = if newModel.state == DerivativesHint.NoHint then model.prev
                                      else Hint}
                  , Cmd.none)
    Tut x -> ( { model | state = Tutorial
                       , freeplay = if x then True else False
                       , instructions = whichone x model
                       , correct = 0
                       , wrong = 0
                       , question = Q "" 0 answerGenD
                       , randCoefs = []
                       , displayAnswers = []
                       , questionInt = 0
                       , showAnswer = False
                       , derivativesHint = DerivativesHint.init
                       , right = False
                       , prev = Home
                       , zoom = 3} , Cmd.none)              
    Instructions i -> 
                let 
                  newModel = GameInstructions.update i model.instructions
                  
                in
                  ( { model | state = if newModel.state == GameInstructions.Done then Question
                                      else if newModel.state == GameInstructions.Back then Home
                                      else model.state}
                  , generate (List.length questions))
        
    CSwap c -> 
        ({ model | colourBlind = if c == "colourblind" then not model.colourBlind else model.colourBlind
                 , dark = if c == "dark" then not model.dark else model.dark}, Cmd.none)
    Answer an -> 
      (let (Q a b c) = model.question
          in {model | correct = 
                        if an == (E (b - 2) (derivativePow model.randCoefs (formList (4)))) 
                          && model.showAnswer == False && model.showAnswer == False then model.correct + 1
                        else model.correct
                      ,
                      wrong = 
                        if an /= (E (b - 2) (derivativePow model.randCoefs (formList (b - 1)))) 
                            && model.showAnswer == False then model.wrong + 1
                        else model.wrong
                      ,
                      state = if model.freeplay == False && model.wrong + 1 >= 3 &&
                              an /= (E (b - 2) (derivativePow model.randCoefs (formList (b - 1)))) 
                            && model.showAnswer == False then Results 
                          else model.state
                      ,
                      right = if an == (E (b - 2) (derivativePow model.randCoefs (formList (4)))) 
                          && model.showAnswer == False && model.showAnswer == False then True
                          else if  an /= (E (b - 2) (derivativePow model.randCoefs (formList (b - 1)))) 
                            && model.showAnswer == False then False
                          else model.right
                      ,
                      showAnswer = True 
                        
       }, Cmd.none)
      
    Mouse (x, y) (x1, y1) -> ({ model | currentx = x1
                                     , currenty = y1
                                     , exit = False}, Cmd.none)
    Exited -> ({ model | exit = True }, Cmd.none)
    Next -> 
      ({model | showAnswer = False}, generate (List.length questions))
    Zoom i -> 
      ({model | zoom = if (round model.zoom) + i > 5 || (round model.zoom) + i < 1 then model.zoom
                       else model.zoom + (toFloat i) }, Cmd.none)               
                 
whichone x model = 
  if x == False then whichonept2 model
  else whichonept3 model
      
      
whichonept2 model = 
  if model.dark == True && model.colourBlind == True then GameInstructions.porquenolosdos
  else if model.dark == True then GameInstructions.d
  else if model.colourBlind == True then GameInstructions.b
  else GameInstructions.init 

whichonept3 model = 
  if model.dark == True && model.colourBlind == True then GameInstructions.porquenolosdos1
  else if model.dark == True then GameInstructions.d1
  else if model.colourBlind == True then GameInstructions.b1
  else GameInstructions.init1

view model = GraphicSVG.collage 192 128 (myShapes model)

myShapes model = 
    case model.state of
      Home ->
          [
              GraphicSVG.square 1000
              |> GraphicSVG.filled (colours (inve White model.dark model.colourBlind))
              ,
              GraphicSVG.text "The Simple Derivatives Game"
              |> GraphicSVG.customFont "Comic Sans MS"
              |> GraphicSVG.size 10
              |> GraphicSVG.centered
              |> GraphicSVG.filled (colours (inve Orange model.dark model.colourBlind))
              |> GraphicSVG.move (0, 40)
              ,
              GraphicSVG.group [
              GraphicSVG.roundedRect 60 20 5
                |> GraphicSVG.filled (colours (inve White model.dark model.colourBlind))
                |> GraphicSVG.addOutline (GraphicSVG.solid 1) (colours (inve Orange model.dark model.colourBlind))
              ,
              GraphicSVG.text "Play"
              |> GraphicSVG.customFont "Comic Sans MS"
              |> GraphicSVG.size 10
              |> GraphicSVG.centered
              |> GraphicSVG.filled (colours (inve Orange model.dark model.colourBlind))
              |> GraphicSVG.move (0, -2)
              ]
                |> GraphicSVG.move(-35, -20)
                |> GraphicSVG.notifyTap (Tut False)
                
              ,
              GraphicSVG.group [
              GraphicSVG.roundedRect 60 20 5
                |> GraphicSVG.filled (colours (inve White model.dark model.colourBlind))
                |> GraphicSVG.addOutline (GraphicSVG.solid 1) (colours (inve Orange model.dark model.colourBlind))
              ,
              GraphicSVG.text "Freeplay"
              |> GraphicSVG.customFont "Comic Sans MS"
              |> GraphicSVG.size 10
              |> GraphicSVG.centered
              |> GraphicSVG.filled (colours (inve Orange model.dark model.colourBlind))
              |> GraphicSVG.move (0, -2)
              ]
                |> GraphicSVG.move(35, -20)
                |> GraphicSVG.notifyTap (Tut True)
                
             ,
             GraphicSVG.group [
              GraphicSVG.roundedRect 60 20 5
                |> GraphicSVG.filled (colours (inve White model.dark model.colourBlind))
                |> GraphicSVG.addOutline (GraphicSVG.solid 1) (colours (inve Orange model.dark model.colourBlind))
              ,
              GraphicSVG.text "Settings"
              |> GraphicSVG.customFont "Comic Sans MS"
              |> GraphicSVG.size 10
              |> GraphicSVG.centered
              |> GraphicSVG.filled (colours (inve Orange model.dark model.colourBlind))
              |> GraphicSVG.move (0, -2)
              ]
                |> GraphicSVG.move(-35, -45)
                |> GraphicSVG.notifyTap ChangeSettings
              
             ,
             GraphicSVG.group [
              GraphicSVG.roundedRect 60 20 5
                |> GraphicSVG.filled (colours (inve White model.dark model.colourBlind))
                |> GraphicSVG.addOutline (GraphicSVG.solid 1) (colours (inve Orange model.dark model.colourBlind))
              ,
              GraphicSVG.text "Help"
              |> GraphicSVG.customFont "Comic Sans MS"
              |> GraphicSVG.size 10
              |> GraphicSVG.centered
              |> GraphicSVG.filled (colours (inve Orange model.dark model.colourBlind))
              |> GraphicSVG.move (0, -2)
              ]
                |> GraphicSVG.move(35, -45)
                |> GraphicSVG.notifyTap Hints 
             ]   
             
             
      Results ->
          [
              GraphicSVG.square 1000
              |> GraphicSVG.filled (colours (inve White model.dark model.colourBlind))
              ,
              GraphicSVG.text "You Lost All Your Lives"
              |> GraphicSVG.customFont "Comic Sans MS"
              |> GraphicSVG.centered
              |> GraphicSVG.filled (colours (inve Orange model.dark model.colourBlind))
              |> GraphicSVG.move (0, 40)
              ,
              GraphicSVG.text "Correctly Answered"
              |> GraphicSVG.customFont "Comic Sans MS"
              |> GraphicSVG.size 7
              |> GraphicSVG.filled (colours (inve Orange model.dark model.colourBlind))
              |> GraphicSVG.move (-60, 17)
              ,
              GraphicSVG.text (String.fromInt model.correct)
                |> GraphicSVG.customFont "Comic Sans MS"
                |> GraphicSVG.size 7
                |> GraphicSVG.filled (colours (inve Orange model.dark model.colourBlind))
                |> GraphicSVG.move (60, 17)
              ,
              home 20 -50 model
              ,
              back -20 -50 model       
              
          ]
      Settings ->
          [
              GraphicSVG.square 1000
              |> GraphicSVG.filled (colours (inve White model.dark model.colourBlind))
              ,
              GraphicSVG.text "Settings"
              |> GraphicSVG.customFont "Comic Sans MS"
              |> GraphicSVG.centered
              |> GraphicSVG.filled (colours (inve Orange model.dark model.colourBlind))
              |> GraphicSVG.move (0, 40)
              ,
              GraphicSVG.text "Dark Mode"
              |> GraphicSVG.customFont "Comic Sans MS"
              |> GraphicSVG.size 7
              |> GraphicSVG.filled (colours (inve Orange model.dark model.colourBlind))
              |> GraphicSVG.move (-60, 17)
              ,
              GraphicSVG.text "Colourblind Mode"
              |> GraphicSVG.customFont "Comic Sans MS"
              |> GraphicSVG.size 7
              |> GraphicSVG.filled (colours (inve Orange model.dark model.colourBlind))
              |> GraphicSVG.move (-60, -3)
              ,
              blindButton 60 0 model
              ,
              darkButton 60 20 model
              ,
              backButton 0 -40 model
              
          ]
      Question ->
          let (Q a b c) = model.question in
           [
                GraphicSVG.group[
                  GraphicSVG.square 1000
                  |> GraphicSVG.filled (colours (inve White model.dark model.colourBlind))
                  ,
                  GraphicSVG.roundedRect 80 20 10
                  |> GraphicSVG.filled (GraphicSVG.darkBlue)
                  |> GraphicSVG.move (-50, 45)
                  ,
                  printer -80 40 model.randCoefs (formList 4) 6 5 True model
                  |> GraphicSVG.group                
                  ,
                  GraphicSVG.text (a)
                  |> GraphicSVG.customFont "Comic Sans MS"
                  |> GraphicSVG.size 6
                  |> GraphicSVG.filled (colours White)            
                  |> GraphicSVG.move (-85, 48)
                  ,
                  scoreBoard model
                  --graph
                  ,
                  GraphicSVG.group[
                    grid model
                    ,
                    display model |> GraphicSVG.group
                    ,
                    box model
                  ]
                  ,
                  values model |> GraphicSVG.group
                  , 
                  buttons model.displayAnswers 5 model.questionInt model --buttons for answers
                  ,
                  --hints button
                  hintButton 0 -40 model
                  ,
                  --settings button
                  settings 0 -25 model
                  ,
                  --home button
                  home 0 -55 model
                  , 
                  --zoom buttons
                  zoomButtons model |> GraphicSVG.group
                  ] |> GraphicSVG.notifyMouseUp Exited
                  --,
                  --ballGraph model Red|> GraphicSVG.group
                  
             ] 
             ++
             if model.showAnswer == True then 
                 [
                   GraphicSVG.roundedRect 60 10 5
                       |> GraphicSVG.filled (colours (inve (correctnessCol model.right) model.dark model.colourBlind))
                       |> GraphicSVG.move (-50, 25)
                   ,
                   printer -70 24 (derivativePow model.randCoefs (formList 4)) (formList 3) 5 (toFloat(List.length model.randCoefs)) True model
                       |> GraphicSVG.group
                       
                   ,
                   GraphicSVG.text (correctness model.right) 
                       |> GraphicSVG.customFont "Comic Sans MS"
                       |> GraphicSVG.alignRight
                       |> GraphicSVG.size 8
                       |> GraphicSVG.filled (colours White)
                       |> GraphicSVG.move (-23, 22)
                   ,
                   GraphicSVG.group[
                   grapherN (derivativePow model.randCoefs (formList 4)) (3) (-40 / model.zoom) model Blue
                   ,
                   display model |> GraphicSVG.group
                   ,
                   box model
                   
                   ]
                   ,
                   nextButton -50 -50 model 
                 ]
             else []
                
      Hint -> 
          case model.questionInt of 
          0 -> 
              [(DerivativesHint.myShapes model.derivativesHint
                      |> List.map
                    (GraphicSVG.map DerivativesHint)
                 ) |> GraphicSVG.group]
          otherwise -> []
              
      Tutorial -> 
              [(GameInstructions.myShapes model.instructions
                      |> List.map
                    (GraphicSVG.map Instructions)
                 ) |> GraphicSVG.group]
                 
      otherwise  -> []

ballGraph model col = 
  if model.exit == False && (calculator model.randCoefs 4 ((model.currentx - offsetx)/model.zoom)) * model.zoom <= 40 
      && (calculator model.randCoefs 4 ((model.currentx - offsetx)/model.zoom)) * model.zoom >= -40
      then
      [
      GraphicSVG.circle (model.zoom / 2 + 1)
      |> GraphicSVG.filled (colours (inve col model.dark model.colourBlind))
      |> GraphicSVG.move (model.currentx, (calculator model.randCoefs 4 ((model.currentx - offsetx)/model.zoom)) * model.zoom + offsety)
      ]  
  else []      
      
-- the zoom buttons
-- the zoom in button will not appear at 5 and the zoom out button will not appear at 1


zoomButtons model = 
  [
    --zoom in
    if model.zoom /= 5 then
      GraphicSVG.group[
        ci Orange model |> GraphicSVG.scale 0.5
        , 
        GraphicSVG.text "+" 
          |> GraphicSVG.customFont "Comic Sans MS"
          |> GraphicSVG.centered
          |> GraphicSVG.size 4
          |> GraphicSVG.filled (colours (inve Orange model.dark model.colourBlind))
          |> GraphicSVG.move (0, -1)
      ] |> GraphicSVG.move (81, 23) |> GraphicSVG.notifyTap (Zoom 1)
    else GraphicSVG.group[]
  ]
  ++
  [
    --zoom out
    if model.zoom /= 1 then
      GraphicSVG.group[
        ci Orange model |> GraphicSVG.scale 0.5
        , 
        GraphicSVG.text "-" 
          |> GraphicSVG.customFont "Comic Sans MS"
          |> GraphicSVG.centered
          |> GraphicSVG.size 4
          |> GraphicSVG.filled (colours (inve Orange model.dark model.colourBlind))
          |> GraphicSVG.move (0, -1)
      ] |> GraphicSVG.move (81, 13) |> GraphicSVG.notifyTap (Zoom -1)
    else GraphicSVG.group[]
  ]

correctness x = if x then "✓" else "X"

correctnessCol x = if x then Green else Red

heartShaker y lives model =
  if lives > 0 then
      [
      GraphicSVG.group
      [
          GraphicSVG.curve (0, -15) [
                GraphicSVG.Pull (10, 0) (10, 0),
                GraphicSVG.Pull (5, 11) (0, 0),
                GraphicSVG.Pull (-5, 11) (-10, 0),
                GraphicSVG.Pull (0, -15) (0, -15)] 
            |> GraphicSVG.filled (colours (inve Red model.dark model.colourBlind))
         ,
         GraphicSVG.curve (0, -15) [
                GraphicSVG.Pull (10, 0) (10, 0),
                GraphicSVG.Pull (5, 11) (0, 0),
                GraphicSVG.Pull (-5, 11) (-10, 0),
                GraphicSVG.Pull (0, -15) (0, -15)] 
            |> GraphicSVG.outlined (GraphicSVG.solid 1) (colours (inve Charcoal model.dark model.colourBlind))
      ] |> GraphicSVG.scale 0.5 |> GraphicSVG.move (y, 45)
      ]
      ++
      heartShaker (y + 11) (lives - 1) model
  else []

nextButton x y model = 
  GraphicSVG.group [
      GraphicSVG.roundedRect 40 10 5
        |> GraphicSVG.filled (colours (inve White model.dark model.colourBlind))
        |> GraphicSVG.addOutline (GraphicSVG.solid 1) (colours (inve (correctnessCol model.right) model.dark model.colourBlind))
      ,
      GraphicSVG.text "Next"
        |> GraphicSVG.centered
        |> GraphicSVG.customFont "Comic Sans MS"
        |> GraphicSVG.size 6
        |> GraphicSVG.filled (colours (inve (correctnessCol model.right) model.dark model.colourBlind))
        |> GraphicSVG.move (0, -2)
  ] |> GraphicSVG.move (x, y) |> GraphicSVG.notifyTap Next 


home x y model = 
  GraphicSVG.group [
      ci Orange model
      ,

      GraphicSVG.group[
      GraphicSVG.polygon [(5, 0), (-5, 0), (0, 4)] |> GraphicSVG.filled (colours (inve Orange model.dark model.colourBlind))|> GraphicSVG.move (0, 0.5)
      ,
      GraphicSVG.square 8 |> GraphicSVG.filled (colours (inve Orange model.dark model.colourBlind)) |> GraphicSVG.move (0, -3) 
      ,
      GraphicSVG.rect 2 4 |> GraphicSVG.filled (colours (inve White model.dark model.colourBlind)) |> GraphicSVG.move (0, -5) 
      ] |> GraphicSVG.scale 0.75 |> GraphicSVG.move (0, 1)

  ] |> GraphicSVG.scale 0.8 |> GraphicSVG.move (x, y) |> GraphicSVG.notifyTap ToHome

back x y model = 
    GraphicSVG.group [
        ci Orange model,
        GraphicSVG.group[
          GraphicSVG.triangle 9 |> GraphicSVG.filled (colours (inve Orange model.dark model.colourBlind)) |> GraphicSVG.move (7, 0)
          ,
          GraphicSVG.square 8 |> GraphicSVG.filled (colours (inve Orange model.dark model.colourBlind)) |> GraphicSVG.move (-1, 0) 
        ] |> GraphicSVG.rotate pi |> GraphicSVG.scale 0.4 |> GraphicSVG.move (2, 0) 
    ] |> GraphicSVG.scale 0.8 |> GraphicSVG.move (x, y) |> GraphicSVG.notifyTap (Restart "Play")


ci c model =
  GraphicSVG.circle 7
    |> GraphicSVG.filled (colours (inve White model.dark model.colourBlind))
    |> GraphicSVG.addOutline (GraphicSVG.solid 1) (colours (inve c model.dark model.colourBlind))
  

scoreBoard model =
  if model.freeplay == True then
    GraphicSVG.group[
        GraphicSVG.roundedRect 80 10 5
            |> GraphicSVG.filled (colours (inve Blue model.dark model.colourBlind))
            |> GraphicSVG.move (55, -58)
        ,
        GraphicSVG.text ("Correct: " ++ String.fromInt model.correct) 
        |> GraphicSVG.alignLeft
        |> GraphicSVG.customFont "Comic Sans MS"
        |> GraphicSVG.filled GraphicSVG.white
        |> GraphicSVG.scale 0.5
        |> GraphicSVG.move (20, -60)
        ,
        GraphicSVG.text ("Wrong: " ++ String.fromInt model.wrong) 
        |> GraphicSVG.alignRight
        |> GraphicSVG.customFont "Comic Sans MS"
        |> GraphicSVG.filled GraphicSVG.white
        |> GraphicSVG.scale 0.5
        |> GraphicSVG.move (90, -60)
        ]
  else 
    GraphicSVG.group[
        heartShaker 20 (3 - model.wrong) model |> GraphicSVG.group
        ]

buttons list start i model = 
  --rectangle
  if i == 0 then
      case list of 
        a :: rest ->
            let (E pow coefs) = a in
            GraphicSVG.group
            [
              GraphicSVG.group [
              GraphicSVG.roundedRect 70 15 2
                |> GraphicSVG.filled (colours (inve Blue model.dark model.colourBlind))
                |> GraphicSVG.move (-50, start)
              ,
              printer -70 start coefs (formList pow) 5 (toFloat(List.length model.randCoefs)) True model
                |> GraphicSVG.group
           ] |> GraphicSVG.notifyTap (Answer (E pow coefs))
           , 
           buttons rest (start - 20) i model
           ]

        otherwise -> 
           GraphicSVG.rect 0 0
             |> GraphicSVG.filled (colours White)
             |> GraphicSVG.move (-1000, -1000)
  else 
           GraphicSVG.rect 0 0
             |> GraphicSVG.filled (colours White)
             |> GraphicSVG.move (-1000, -1000)
             
--generate questions
generate u = Random.generate  (identity) 
  <| Random.map Questions (Random.int 0 (u-1))

--generates answers and shuffles
generateAnswers p = Random.generate  (identity) 
  <| Random.map Store (Random.list 10 (Random.int 0 (p - 1)))

--generate coefficients or needed values
generateVals v = Random.generate  (identity) 
  <| Random.map Generate (Random.list v (Random.int -5 5))

--shuffles the deck given a list where it will repeat it x number of times, which is 10
shuffle x n = 
  case x of
    (c :: rest) -> 
      if c > 0 then 
        let (a, b) = cut c [] n
        in shuffle rest (b++a)
      else n
    otherwise -> n

--the shuffler that cuts the list at a specific index, swaps the order of the list
cut location start end = 
  if location > 0 then
    case end of
      (a::rest) -> cut (location - 1) (a :: start) rest 
      otherwise -> (start, end)
  else (start, end)

derivativePow listcoefs listpows = 
  case listcoefs of 
      a :: rest ->
          case listpows of 
              b :: rest1 -> [a * b] ++ derivativePow rest rest1
              otherwise -> []
      otherwise -> []


settings x y model = 
  GraphicSVG.group[
   ci Orange model
        |> GraphicSVG.scale 0.8
   ,
   GraphicSVG.group [
         GraphicSVG.circle 5.5
         |> GraphicSVG.outlined (GraphicSVG.solid 3) (colours (inve Orange model.dark model.colourBlind)),
         GraphicSVG.rect 3.5 4
         |> GraphicSVG.filled (colours (inve Orange model.dark model.colourBlind))
         |> GraphicSVG.move (0, 8),
         GraphicSVG.rect 3.5 4
         |> GraphicSVG.filled (colours (inve Orange model.dark model.colourBlind))
         |> GraphicSVG.move (0, -8),
         GraphicSVG.rect 3.5 4
         |> GraphicSVG.filled (colours (inve Orange model.dark model.colourBlind))
         |> GraphicSVG.rotate (degrees 90)
         |> GraphicSVG.move (8, 0),
         GraphicSVG.rect 3.5 4
         |> GraphicSVG.filled (colours (inve Orange model.dark model.colourBlind))
         |> GraphicSVG.rotate (degrees 90)
         |> GraphicSVG.move (-8, 0),
         GraphicSVG.rect 3.5 4
         |> GraphicSVG.filled (colours (inve Orange model.dark model.colourBlind))
         |> GraphicSVG.rotate (degrees -45)
         |> GraphicSVG.move (6, 6),
         GraphicSVG.rect 3.5 4
         |> GraphicSVG.filled (colours (inve Orange model.dark model.colourBlind))
         |> GraphicSVG.rotate (degrees -45)
         |> GraphicSVG.move (-6, -6),
         GraphicSVG.rect 3.5 4
         |> GraphicSVG.filled (colours (inve Orange model.dark model.colourBlind))
         |> GraphicSVG.rotate (degrees 45)
         |> GraphicSVG.move (-6, 6),
         GraphicSVG.rect 3.5 4
         |> GraphicSVG.filled (colours (inve Orange model.dark model.colourBlind))
         |> GraphicSVG.rotate (degrees 45)
         |> GraphicSVG.move (6, -6)] |> GraphicSVG.scale 0.4
         
        
         
    ] |> GraphicSVG.move (x, y) |> GraphicSVG.notifyTap ChangeSettings
       
backButton x y model = 
  GraphicSVG.group [
      ci Orange model
      ,
      GraphicSVG.triangle 4
          |> GraphicSVG.filled (colours (inve Orange model.dark model.colourBlind))
          |> GraphicSVG.move (1.5, 0)
      ,
      GraphicSVG.rect 7 3
          |> GraphicSVG.filled (colours (inve Orange model.dark model.colourBlind))
          |> GraphicSVG.move (-0.75, 0)
    ] |> GraphicSVG.rotate pi |> GraphicSVG.move (x, y) |> GraphicSVG.notifyTap (ToQuestion "Settings")


hintButton x y model = 
  GraphicSVG.group [
      ci Orange model
      ,
      GraphicSVG.text "!"
        |> GraphicSVG.centered
        |> GraphicSVG.customFont "Comic Sans MS"
        |> GraphicSVG.bold
        |> GraphicSVG.size 10
        |> GraphicSVG.filled (colours (inve Orange model.dark model.colourBlind))
        |> GraphicSVG.move (0, -3)
    ]
      |> GraphicSVG.scale 0.8
      |> GraphicSVG.move(x, y)
      |> GraphicSVG.notifyTap Hints

formList x = 
    if x >= 0 then [x] ++ formList (x - 1)
    else []


getSign x bool = 
  if x < 0 && bool == True then " -"
  else if x < 0 && bool == False then " - " 
  else if x > 0 && bool == False then " + " 
  else ""

valueString i = 
  if i == 1 then "" else String.fromInt (i)

printer x y coefs power fo terms addSign model= 
  case coefs of 
    a :: rest ->
      case power of 
          b :: rest1 ->
               if a /= 0 && b /= 0 then
                    [
                      GraphicSVG.text (getSign a addSign) 
                        |> GraphicSVG.centered 
                        |> GraphicSVG.customFont "Comic Sans MS"
                        |> GraphicSVG.size fo  
                        |> GraphicSVG.filled (colours White)
                        |> GraphicSVG.move (x - fo, y)
                      ,
                      GraphicSVG.text (valueString(abs(a)) ++ "x") 
                        |> GraphicSVG.centered 
                        |> GraphicSVG.customFont "Comic Sans MS"
                        |> GraphicSVG.size fo  
                        |> GraphicSVG.filled (colours White)
                        |> GraphicSVG.move (x, y)
                      ,
                      GraphicSVG.text (if b > 1 then String.fromInt(b) else "") 
                        |> GraphicSVG.customFont "Comic Sans MS"
                        |> GraphicSVG.size (fo / 2) 
                        |> GraphicSVG.filled (colours White)
                        |> GraphicSVG.move 
                        (x + toFloat(String.length(valueString(abs(a)) ++ "x")) / 3 * fo, y + fo * 0.5)
                    ]
                    ++
                    printer (x + toFloat(String.length(valueString(abs(a)) ++ "x")) + 2 * fo) 
                      y rest rest1 fo terms False model
                    
                 else if b == 0 then
                    [
                      GraphicSVG.text (getSign a addSign) 
                        |> GraphicSVG.centered 
                        |> GraphicSVG.customFont "Comic Sans MS"
                        |> GraphicSVG.size fo  
                        |> GraphicSVG.filled (colours White)
                        |> GraphicSVG.move (x - fo, y)
                      ,
                      GraphicSVG.text ((if a == 0 then "" else String.fromInt(abs a))) 
                        |> GraphicSVG.centered 
                        |> GraphicSVG.customFont "Comic Sans MS"
                        |> GraphicSVG.size fo  
                        |> GraphicSVG.filled (colours White)
                        |> GraphicSVG.move (x, y)
                     
                    ]
                    ++
                    printer (x + fo ) y rest rest1 fo terms False model
            
       
             else if a == 0 then printer x y rest rest1 fo terms addSign model
             else []
                
            
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


      
blindButton x y model = 
   GraphicSVG.group [
      GraphicSVG.roundedRect 40 20 10
        |> GraphicSVG.filled (colours (inve White model.dark model.colourBlind))
        |> GraphicSVG.addOutline (GraphicSVG.solid 1) (colours (inve Orange model.dark model.colourBlind))
      ,
      GraphicSVG.circle 8
        |> GraphicSVG.filled (colours (inve Orange model.dark model.colourBlind))
        |> GraphicSVG.addOutline (GraphicSVG.solid 1) (colours (inve Orange model.dark model.colourBlind))
        |> GraphicSVG.move (if model.colourBlind  == True then (10, 0) else (-10, 0))
     ,
     GraphicSVG.text (if model.colourBlind == True then "On" else "Off")
        |> GraphicSVG.customFont "Comic Sans MS"
        |> GraphicSVG.size 8
        |> GraphicSVG.centered
        |> GraphicSVG.filled (colours (inve Orange model.dark model.colourBlind)) 
        |> GraphicSVG.move (if model.colourBlind  == True then (-10, -2) else (10, -2))

    ]
      |> GraphicSVG.scale 0.8
      |> GraphicSVG.move(x, y)
      |> GraphicSVG.notifyTap (CSwap "colourblind")
      

darkButton x y model = 
  GraphicSVG.group [
      GraphicSVG.roundedRect 40 20 10
        |> GraphicSVG.filled (colours (inve White model.dark model.colourBlind))
        |> GraphicSVG.addOutline (GraphicSVG.solid 1) (colours (inve Orange model.dark model.colourBlind))
      ,
      GraphicSVG.circle 8
        |> GraphicSVG.filled (colours (inve Orange model.dark model.colourBlind))
        |> GraphicSVG.addOutline (GraphicSVG.solid 1) (colours (inve Orange model.dark model.colourBlind))
        |> GraphicSVG.move (if model.dark == True then (10, 0) else (-10, 0))
     ,
     GraphicSVG.text (if model.dark == True then "On" else "Off")
        |> GraphicSVG.customFont "Comic Sans MS"
        |> GraphicSVG.size 8
        |> GraphicSVG.centered
        |> GraphicSVG.filled (colours (inve Orange model.dark model.colourBlind)) 
        |> GraphicSVG.move (if model.dark == True then (-10, -2) else (10, -2))

    ]
      |> GraphicSVG.scale 0.8
      |> GraphicSVG.move(x, y)
      |> GraphicSVG.notifyTap (CSwap "dark")

main : EllieAppWithTick () Model Msg
main =
    ellieAppWithTick Tick
        { init = \ _ -> ( init, generate (List.length questions))
        , update = update
        , view = \ model -> { title = myTitle, body = view model }
        , subscriptions = \_ -> Sub.none
        }

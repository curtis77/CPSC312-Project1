-- CPSC 312 Project 1 by Curtis Fox and Jennifer Ahn
module Explorer where
-- To run the program, use the following commands in this order:
-- ghci
-- :l explorer.hs
-- main
import Graphics.Gloss
import Graphics.Gloss.Interface.Pure.Game

main = play (InWindow "UI Explorer" (800, 800) (0,0)) blue 20 startingWorld renderWorld handleKeys stepWorld

data Game = ContinueGame Player TextInput Monster
          | GameOver
          deriving (Eq,Show)

data TextInput = TextInput String
          deriving (Eq,Show)

data Player = Player Position
          deriving (Eq,Show)

data Monster = Monster Position
          deriving (Eq,Show)

type Position = (Float, Float)

startingWorld = (ContinueGame (Player (0,0)) (TextInput "") (Monster (100,100)))

renderWorld GameOver = (scale 0.5 0.5 (translate 0 250 (text "Game Over")))

renderWorld (ContinueGame (Player (x,y)) (TextInput c) (Monster (xm,ym))) = 
        pictures [(translate x y (color red (circleSolid 5))), 
                  (scale 0.1 0.1 (translate 0 200 (text c))),
                  (rectangleWire 610 610),
                  (translate xm ym (color green (circleSolid 10)))]

handleKeys _ GameOver = GameOver

handleKeys (EventKey (SpecialKey KeyUp) Down _ _) (ContinueGame (Player (x,y)) (TextInput c) (Monster (xm,ym)))  
    | y < 300 = (ContinueGame (Player (x, y + 20)) (TextInput c) (Monster (xm,ym)))
    | y > 300 = (ContinueGame (Player (x, 300)) (TextInput c)(Monster (xm,ym)))

handleKeys (EventKey (SpecialKey KeyLeft) Down _ _) (ContinueGame (Player (x,y)) (TextInput c) (Monster (xm,ym))) 
    | x > -300 = (ContinueGame (Player (x - 20, y)) (TextInput c) (Monster (xm,ym)))
    | x < -300 = (ContinueGame (Player (-300, y)) (TextInput c) (Monster (xm,ym)))

handleKeys (EventKey (SpecialKey KeyDown) Down _ _) (ContinueGame (Player (x,y)) (TextInput c) (Monster (xm,ym))) 
    | y > -300 = (ContinueGame (Player (x, y - 20)) (TextInput c) (Monster (xm,ym)))
    | y < -300 = (ContinueGame (Player (x, -300)) (TextInput c) (Monster (xm,ym)))

handleKeys (EventKey (SpecialKey KeyRight) Down _ _) (ContinueGame (Player (x,y)) (TextInput c) (Monster (xm,ym))) 
    | x < 300 = (ContinueGame (Player (x + 20,y)) (TextInput c) (Monster (xm,ym)))
    | x > 300 = (ContinueGame (Player (300,y)) (TextInput c)(Monster (xm,ym)))

handleKeys (EventKey (Char b) Up _ _) (ContinueGame (Player (x,y)) (TextInput c) (Monster (xm,ym))) = 
    (ContinueGame (Player (x,y)) (TextInput (newTypedInput b c)) (Monster (xm,ym)))

handleKeys _ game = game

newTypedInput c currInput = currInput ++ [c]

stepWorld time (ContinueGame (Player (x,y)) (TextInput c) (Monster (xm,ym))) 
    | (x - xm <= 10) && (y - ym <= 10) =  GameOver
    | x < xm = (ContinueGame (Player (x,y)) (TextInput c) (Monster (xm - 1,ym))) 
    | x > xm = (ContinueGame (Player (x,y)) (TextInput c) (Monster (xm + 1,ym))) 
    | y < ym = (ContinueGame (Player (x,y)) (TextInput c) (Monster (xm,ym - 1)))
    | y > ym = (ContinueGame (Player (x,y)) (TextInput c) (Monster (xm,ym + 1)))

stepWorld _ GameOver = GameOver

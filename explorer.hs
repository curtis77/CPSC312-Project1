-- CPSC 312 Project 1 by Curtis Fox and Jennifer Ahn
module Explorer where
-- To run the program, use the following commands in this order:
-- ghci
-- :l explorer.hs
-- :run main
import Graphics.Gloss
import Graphics.Gloss.Interface.Pure.Game

main :: IO ()
main = play (InWindow "UI Explorer" (800, 800) (0,0)) blue 20 startingWorld renderWorld handleKeys stepWorld

data Game = ContinueGame Player TextInput
          deriving (Eq,Show)

data TextInput = TextInput String
          deriving (Eq,Show)

data Player = Player Position
          deriving (Eq,Show)

type Position = (Float, Float)

startingWorld = (ContinueGame (Player (0,0)) (TextInput ""))

renderWorld (ContinueGame (Player (x,y)) (TextInput c)) = 
    pictures [(translate x y (color red (circleSolid 5))), 
              (scale 0.1 0.1 (translate 0 200 (text c))),
              (rectangleWire 610 610)]

handleKeys (EventKey (SpecialKey KeyUp) _ _ _) (ContinueGame (Player (x,y)) (TextInput c))  
    | y < 300 = (ContinueGame (Player (x, y + 20)) (TextInput c))
    | y > 300 = (ContinueGame (Player (x, 300)) (TextInput c))

handleKeys (EventKey (SpecialKey KeyLeft) _ _ _) (ContinueGame (Player (x,y)) (TextInput c)) 
    | x > -300 = (ContinueGame (Player (x - 20, y)) (TextInput c))
    | x < -300 = (ContinueGame (Player (-300, y)) (TextInput c))

handleKeys (EventKey (SpecialKey KeyDown) _ _ _) (ContinueGame (Player (x,y)) (TextInput c)) 
    | y > -300 = (ContinueGame (Player (x, y - 20)) (TextInput c))
    | y < -300 = (ContinueGame (Player (x, -300)) (TextInput c))

handleKeys (EventKey (SpecialKey KeyRight) _ _ _) (ContinueGame (Player (x,y)) (TextInput c)) 
    | x < 300 = (ContinueGame (Player (x + 20,y)) (TextInput c))
    | x > 300 = (ContinueGame (Player (300,y)) (TextInput c))

handleKeys (EventKey (Char b) _ _ _) (ContinueGame (Player (x,y)) (TextInput c)) = 
    (ContinueGame (Player (x,y)) (TextInput (newInput b c)))

handleKeys _ game = game

newInput c currInput = c : currInput

stepWorld time (ContinueGame (Player (x,y)) (TextInput c)) = (ContinueGame (Player (x,y)) (TextInput c))

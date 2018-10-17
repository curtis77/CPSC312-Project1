-- CPSC 312 Project 1 by Curtis Fox and Jennifer Ahn
module Explorer where
-- To run the program, use the following commands in this order:
-- ghci
-- :l explorer.hs
-- main
import Graphics.Gloss
import Graphics.Gloss.Interface.Pure.Game

-- main function 
main :: IO ()
main = play (InWindow "UI Explorer" (610, 610) (0,0)) blue 2 startingWorld renderWorld handleKeys stepWorld

-- This data definition defines the state of the game
data Game = ContinueGame Player TextInput Monster
          | GameEnd
          deriving (Eq,Show)

-- This data definition defines the current string 
data TextInput = TextInput String
          deriving (Eq,Show)

-- This data definition defines the player with his/her respective position on the UI
data Player = Player Position
          deriving (Eq,Show)

-- This data definition defines the monster with its respective position on the UI		  
data Monster = Monster Position
          deriving (Eq,Show)

-- This type defines a position, with an x-coordinate and y-coordinate
type Position = (Float, Float)

-- This is the function returns the starting world state
startingWorld :: Game
startingWorld = (ContinueGame (Player (0,0)) (TextInput "") (Monster (100,100)))

-- This function takes in the current world state, and renders the world state on the UI
renderWorld :: Game -> Picture
renderWorld (ContinueGame (Player (x,y)) (TextInput c) (Monster (xm,ym))) = 
        pictures [(translate x y (color red (circleSolid 5))), 
                  (scale 0.1 0.1 (translate 0 200 (text c))),
                  (rectangleWire 610 610),
                  (translate xm ym (color green (circleSolid 10)))]  
renderWorld GameEnd = color red (scale 0.75 0.75 (translate (-350) 250 (text "Game Over")))

-- This function handles all key events, by taking in a key event and the current world state,
-- and returning the new world state
handleKeys :: Event -> Game -> Game
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
handleKeys _ GameEnd = GameEnd
handleKeys _ game = game

-- This is a helper function that takes in a character c and a string, and returns a appended
-- to the front of the string
newTypedInput :: a -> [a] -> [a]
newTypedInput c currInput = currInput ++ [c]

-- This function takes in the time passed and the current world state, and updates the world
-- state accordingly based the amount of time passed
stepWorld :: Float -> Game -> Game
stepWorld time (ContinueGame (Player (x,y)) (TextInput c) (Monster (xm,ym))) 
    | (absVal (x - xm) <= 10) && (absVal (y - ym) <= 10) =  GameEnd
    | x < xm = (ContinueGame (Player (x,y)) (TextInput c) (Monster (xm - 20,ym))) 
    | x > xm = (ContinueGame (Player (x,y)) (TextInput c) (Monster (xm + 20,ym))) 
    | y < ym = (ContinueGame (Player (x,y)) (TextInput c) (Monster (xm,ym - 20)))
    | y > ym = (ContinueGame (Player (x,y)) (TextInput c) (Monster (xm,ym + 20)))
stepWorld _ GameEnd = GameEnd

-- This function takes in an input x, and returns the absolute of x
absVal :: (Ord a, Num a) => a -> a
absVal x 
    | x >= 0 = x
    | x < 0 = (-x)
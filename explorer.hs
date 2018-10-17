-- CPSC 312 Project 1 by Curtis Fox and Jennifer Ahn
module Explorer where
-- To run the program, use the following commands in this order:
-- ghci
-- :l explorer.hs
-- main
import Graphics.Gloss
import Graphics.Gloss.Interface.Pure.Game

-- main function that starts the UI/game
main :: IO ()
main = play (InWindow "UI Explorer" (770, 770) (0,0)) blue 2 startingWorld renderWorld handleKeys stepWorld

-- This data definition defines the state of the game
data Game = ContinueGame Player TextInput Monster
          | GameEnd
          deriving (Eq,Show)

-- This data definition defines the current string output on the UI
data TextInput = TextInput String
          deriving (Eq,Show)

-- This data definition defines the player with his/her respective position on the UI
data Player = Player Position
          deriving (Eq,Show)

-- This data definition defines the monster with its respective position on the UI		  
data Monster = Monster Position Float
          deriving (Eq,Show)

-- This type defines a position, with an x-coordinate and y-coordinate
type Position = (Float, Float)

-- This is the function returns the starting world state
startingWorld :: Game
startingWorld = (ContinueGame (Player (0,0)) (TextInput "") (Monster (100,100) 10))

-- This function takes in the current world state, and renders the world state on the UI
renderWorld :: Game -> Picture
renderWorld (ContinueGame (Player (x,y)) (TextInput c) (Monster (xm,ym) s)) = 
        pictures [(translate x y (color red (circleSolid 5))), 
                  (scale 0.1 0.1 (translate 0 200 (text c))),
                  (rectangleWire 770 770),
                  (translate xm ym (color green (circleSolid 10)))]  
renderWorld GameEnd = color red (scale 0.75 0.75 (translate (-350) 250 (text "Game Over")))

-- This function handles all key events, by taking in a key event and the current world state,
-- and returning the new world state
handleKeys :: Event -> Game -> Game
handleKeys (EventKey (SpecialKey KeyUp) Down _ _) (ContinueGame (Player (x,y)) (TextInput c) (Monster (xm,ym) s))   -- player moves up when 
    | y < 380 = (ContinueGame (Player (x, y + 20)) (TextInput c) monster)
    | y > 380 = (ContinueGame (Player (x, 380)) (TextInput c) monster)
	where monster = (Monster (xm,ym) s)
handleKeys (EventKey (SpecialKey KeyLeft) Down _ _) (ContinueGame (Player (x,y)) (TextInput c) (Monster (xm,ym) s)) 
    | x > -380 = (ContinueGame (Player (x - 20, y)) (TextInput c) monster)
    | x < -380 = (ContinueGame (Player (-380, y)) (TextInput c) monster)
	where monster = (Monster (xm,ym) s)
handleKeys (EventKey (SpecialKey KeyDown) Down _ _) (ContinueGame (Player (x,y)) (TextInput c) (Monster (xm,ym) s)) 
    | y > -380 = (ContinueGame (Player (x, y - 20)) (TextInput c) monster)
    | y < -380 = (ContinueGame (Player (x, -380)) (TextInput c) monster)
	where monster = (Monster (xm,ym) s)
handleKeys (EventKey (SpecialKey KeyRight) Down _ _) (ContinueGame (Player (x,y)) (TextInput c) (Monster (xm,ym) s)) 
    | x < 380 = (ContinueGame (Player (x + 20,y)) (TextInput c) monster)
    | x > 380 = (ContinueGame (Player (380,y)) (TextInput c) monster)
	where monster = (Monster (xm,ym) s)
handleKeys (EventKey (Char b) Up _ _) (ContinueGame (Player (x,y)) (TextInput c) (Monster (xm,ym) s)) = 
    (ContinueGame (Player (x,y)) (TextInput (newTypedInput b c)) (Monster (xm,ym) s))
handleKeys (EventKey (SpecialKey KeyShiftL) Down _ _) (ContinueGame (Player (x,y)) (TextInput c) (Monster (xm,ym) s)) 
    | s < 40 = (player (TextInput c) (Monster (xm,ym) (s + 5)))
    | s >= 40 = (player (TextInput c) (Monster (xm,ym) s))
    where player = ContinueGame (Player (x,y))
handleKeys (EventKey (SpecialKey KeyCtrlL) Down _ _) (ContinueGame (Player (x,y)) (TextInput c) (Monster (xm,ym) s)) 
    | s >= 5 = (player (TextInput c) (Monster (xm,ym) (s - 5)))
    | s < 5 = (player (TextInput c) (Monster (xm,ym) s))
    where player = ContinueGame (Player (x,y))
handleKeys _ game = game

-- This is a helper function that takes in a character c and a string, and returns a appended
-- to the front of the string
newTypedInput :: a -> [a] -> [a]
newTypedInput c currInput = currInput ++ [c]

-- This function takes in the time passed and the current world state, and updates the world
-- state accordingly based the amount of time passed
stepWorld :: Float -> Game -> Game
stepWorld time (ContinueGame (Player (x,y)) (TextInput c) (Monster (xm,ym) s)) 
    | (distx <= 10) && (disty <= 10) = GameEnd  -- monster has caught player
    | distx <= s && x > xm = (player (TextInput c) (Monster (xm + distx,ym) s)) 
    | distx <= s && x < xm = (player (TextInput c) (Monster (xm - distx,ym) s)) 
    | disty <= s && y > ym = (player (TextInput c) (Monster (xm, ym + disty) s)) 
    | disty <= s && y < ym = (player (TextInput c) (Monster (xm, ym - disty) s)) 
    | x < xm = (player (TextInput c) (Monster (xm - s,ym) s)) 
    | x > xm = (player (TextInput c) (Monster (xm + s,ym) s)) 
    | y < ym = (player (TextInput c) (Monster (xm,ym - s) s))
    | y > ym = (player (TextInput c) (Monster (xm,ym + s) s))
    where distx = absVal (x - xm) 
          disty = absVal (y - ym)
		  player = ContinueGame (Player (x,y))
stepWorld _ GameEnd = GameEnd

-- This function takes in an input x, and returns the absolute of x
absVal :: (Ord a, Num a) => a -> a
absVal x 
    | x >= 0 = x
    | x < 0 = (-x)
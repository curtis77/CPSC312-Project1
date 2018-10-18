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
data Game = ContinueGame Player Monster
          | GameEnd
          deriving (Eq,Show)

-- This data definition defines the player with his/her respective position on the UI
data Player = Player Position
          deriving (Eq,Show)

-- This data definition defines the monster with its respective position on the UI		  
data Monster = Monster Position Float Int
          deriving (Eq,Show)

-- This type defines a position, with an x-coordinate and y-coordinate
type Position = (Float, Float)

-- This is the function returns the starting world state
startingWorld :: Game
startingWorld = (ContinueGame (Player (0,0)) (Monster (100,100) 10 0))

-- This function takes in the current world state, and renders the world state on the UI
renderWorld :: Game -> Picture
renderWorld (ContinueGame (Player (x,y)) (Monster (xm,ym) s countx)) = 
        pictures [(translate x y (color red (circleSolid 5))),
                  (rectangleWire 770 770),
                  (translate xm ym (color green (circleSolid 10)))]  
renderWorld GameEnd = color red (scale 0.75 0.75 (translate (-350) 250 (text "Game Over")))

-- This function handles all key events, by taking in a key event and the current world state,
-- and returning the new world state
handleKeys :: Event -> Game -> Game
handleKeys (EventKey (SpecialKey KeyUp) Down _ _) (ContinueGame (Player (x,y)) (Monster (xm,ym) s countx))   -- player moves up when 
    | y < 380 = (ContinueGame (Player (x, y + 20)) monster)
    | y > 380 = (ContinueGame (Player (x, 380)) monster)
    where monster = (Monster (xm,ym) s countx)

handleKeys (EventKey (SpecialKey KeyLeft) Down _ _) (ContinueGame (Player (x,y)) (Monster (xm,ym) s countx)) 
    | x > -380 = (ContinueGame (Player (x - 20, y)) monster)
    | x < -380 = (ContinueGame (Player (-380, y)) monster)
    where monster = (Monster (xm,ym) s countx)

handleKeys (EventKey (SpecialKey KeyDown) Down _ _) (ContinueGame (Player (x,y)) (Monster (xm,ym) s countx)) 
    | y > -380 = (ContinueGame (Player (x, y - 20)) monster)
    | y < -380 = (ContinueGame (Player (x, -380)) monster)
    where monster = (Monster (xm,ym) s countx)

handleKeys (EventKey (SpecialKey KeyRight) Down _ _) (ContinueGame (Player (x,y)) (Monster (xm,ym) s countx)) 
    | x < 380 = (ContinueGame (Player (x + 20,y)) monster)
    | x > 380 = (ContinueGame (Player (380,y)) monster)
    where monster = (Monster (xm,ym) s countx)

handleKeys (EventKey (SpecialKey KeyShiftL) Down _ _) (ContinueGame (Player (x,y)) (Monster (xm,ym) s countx)) 
    | s < 40 = (player (Monster (xm,ym) (s + 5) countx))
    | s >= 40 = (player (Monster (xm,ym) s countx))
    where player = ContinueGame (Player (x,y))

handleKeys (EventKey (SpecialKey KeyCtrlL) Down _ _) (ContinueGame (Player (x,y)) (Monster (xm,ym) s countx)) 
    | s >= 5 = (player (Monster (xm,ym) (s - 5) countx))
    | s < 5 = (player (Monster (xm,ym) s countx))
    where player = ContinueGame (Player (x,y))

handleKeys _ game = game

-- This function takes in the time passed and the current world state, and updates the world
-- state accordingly based the amount of time passed
stepWorld :: Float -> Game -> Game
stepWorld time (ContinueGame (Player (x,y)) (Monster (xm,ym) s countx)) 
    | (distx <= 10) && (disty <= 10) = GameEnd  -- monster has caught player
    | distx <= s && x > xm = (player (Monster (xm + distx,ym) s (incr countx))) 
    | distx <= s && x < xm = (player (Monster (xm - distx,ym) s (incr countx))) 
    | disty <= s && y > ym = (player (Monster (xm, ym + disty) s 0))
    | disty <= s && y < ym = (player (Monster (xm, ym - disty) s 0))
    | x < xm = (player (Monster (xm - s,ym) s (incr countx))) 
    | x > xm = (player (Monster (xm + s,ym) s (incr countx)))
    | y < ym = (player (Monster (xm,ym - s) s 0))
    | y > ym = (player (Monster (xm,ym + s) s 0))
    where distx = absVal (x - xm) 
          disty = absVal (y - ym)
          player = ContinueGame (Player (x,y))
stepWorld _ GameEnd = GameEnd

-- This function takes in an input x, and returns the absolute of x
absVal :: (Ord a, Num a) => a -> a
absVal x 
    | x >= 0 = x
    | x < 0 = (-x)

-- This helper function takes in a number, and increments it by 1
incr x = x + 1
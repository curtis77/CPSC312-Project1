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
data Game = ContinueGame Player Monster [Food] Score
          | GameEnd Int
          deriving (Eq,Show)

-- This data definition defines the player with his/her respective position on the UI
data Player = Player Position
          deriving (Eq,Show)

-- This data definition defines the monster with its respective position on the UI		  
data Monster = Monster Position Float Int
          deriving (Eq,Show)

-- This data definition defines a Food item	  
data Food = Food Position
          deriving (Eq,Show)

-- This type defines a position, with an x-coordinate and y-coordinate
type Position = (Float, Float)

-- This type defines the current player score
type Score = Int

-- This is the function returns the starting world state
startingWorld :: Game
startingWorld = (ContinueGame (Player (0,0)) (Monster (100,100) 10 0) [(Food (100,150)), (Food (-100,-150)), (Food (200,-150))] 0)

-- This function takes in the current world state, and renders the world state on the UI
renderWorld :: Game -> Picture
renderWorld (ContinueGame (Player (x,y)) (Monster (xm,ym) s countx) foodList score) = 
        pictures ([(translate x y (color red (circleSolid 5))),
                  (rectangleWire 770 770),
                  (translate xm ym (color green (circleSolid 10)))] ++ (allFoodRender foodList))
renderWorld (GameEnd score) = pictures [(color red (scale 0.75 0.75 (translate (-350) 250 (text "Game Over")))),
                                        (color red (scale 0.50 0.50 (translate (-450) 150 (text "Total Score: "))))] 

-- This function takes in a list of Food and renders each food and produces a yellow rectangle for each food piece
allFoodRender :: [Food] -> [Picture]
allFoodRender [] = []
allFoodRender ((Food (x,y)):xs) = (translate x y (color yellow (rectangleSolid 10 10))) : allFoodRender xs

-- This function handles all key events, by taking in a key event and the current world state,
-- and returning the new world state
handleKeys :: Event -> Game -> Game

-- Moves player down if down key pressed
handleKeys (EventKey (SpecialKey KeyUp) Down _ _) (ContinueGame (Player (x,y)) (Monster (xm,ym) s countx) foodList score) 
    | y < 380 && check = (ContinueGame (Player (x, y + 10)) monster newList (score + 1)) -- increases score and removes food if player passes over food 
    | y < 380 = (ContinueGame (Player (x, y + 10)) monster foodList score)
    | y > 380 && check = (ContinueGame (Player (x, 380)) monster newList (score + 1)) -- increases score and removes food if player passes over food
    | y > 380 = (ContinueGame (Player (x, 380)) monster foodList score)
    where monster = (Monster (xm,ym) s countx)
          newList = (newFoodList x y foodList)
          check = (checkFood x y foodList)

-- Moves player left if left key pressed
handleKeys (EventKey (SpecialKey KeyLeft) Down _ _) (ContinueGame (Player (x,y)) (Monster (xm,ym) s countx) foodList score) 
    | x > -380 && check = (ContinueGame (Player (x - 10, y)) monster newList (score + 1)) -- increases score and removes food if player passes over food
    | x > -380 = (ContinueGame (Player (x - 10, y)) monster foodList score)
    | x < -380 && check = (ContinueGame (Player (-380, y)) monster newList (score + 1)) -- increases score and removes food if player passes over food
    | x < -380 = (ContinueGame (Player (-380, y)) monster foodList score)
    where monster = (Monster (xm,ym) s countx)
          newList = (newFoodList x y foodList)
          check = (checkFood x y foodList)

-- Moves player down if down key pressed
handleKeys (EventKey (SpecialKey KeyDown) Down _ _) (ContinueGame (Player (x,y)) (Monster (xm,ym) s countx) foodList score) 
    | y > -380 && check = (ContinueGame (Player (x, y - 10)) monster newList (score + 1)) -- increases score and removes food if player passes over food
    | y > -380 = (ContinueGame (Player (x, y - 10)) monster foodList score)
    | y < -380 && check = (ContinueGame (Player (x, -380)) monster newList (score + 1)) -- increases score and removes food if player passes over food
    | y < -380 = (ContinueGame (Player (x, -380)) monster foodList score)
    where monster = (Monster (xm,ym) s countx)
          newList = (newFoodList x y foodList)
          check = (checkFood x y foodList)

-- Moves player right if right key pressed
handleKeys (EventKey (SpecialKey KeyRight) Down _ _) (ContinueGame (Player (x,y)) (Monster (xm,ym) s countx) foodList score)
    | x < 380 && check = (ContinueGame (Player (x + 10, y)) monster newList (score + 1)) -- increases score and removes food if player passes over food
    | x < 380 = (ContinueGame (Player (x + 10,y)) monster foodList score)
    | x > 380 && check = (ContinueGame (Player (380, y)) monster newList (score + 1)) -- increases score and removes food if player passes over food
    | x > 380 = (ContinueGame (Player (380, y)) monster foodList score)
    where monster = (Monster (xm,ym) s countx)
          newList = (newFoodList x y foodList)
          check = (checkFood x y foodList)

-- Speeds up mosnter if left shift pressed
handleKeys (EventKey (SpecialKey KeyShiftL) Down _ _) (ContinueGame (Player (x,y)) (Monster (xm,ym) s countx) foodList score) 
    | s < 40 = (player (Monster (xm,ym) (s + 5) countx) foodList score)
    | s >= 40 = (player (Monster (xm,ym) s countx) foodList score)
    where player = ContinueGame (Player (x,y))

-- Slows monter down if left ctrl pressed
handleKeys (EventKey (SpecialKey KeyCtrlL) Down _ _) (ContinueGame (Player (x,y)) (Monster (xm,ym) s countx) foodList score) 
    | s >= 5 = (player (Monster (xm,ym) (s - 5) countx) foodList score)
    | s < 5 = (player (Monster (xm,ym) s countx) foodList score)
    where player = ContinueGame (Player (x,y))

-- Does nothing if no key pressed
handleKeys _ game = game

-- This function takes in the x and y coordinate of the player, as well as the current list of food, and
-- removes a food item from the list of food if the player passes over it
newFoodList :: Float -> Float -> [Food] ->[Food]
newFoodList _ _ [] = [] 
newFoodList xp yp ((Food (x,y)):xs) 
    | absVal (xp - x) <= 10 && absVal (yp - y) <= 10 = xs
    | otherwise = (Food (x,y)) : newFoodList xp yp xs

-- This function takes in the x and y coordinate of the player, as well as the current list of food, and
-- returns true if a player passes over a food item, elase false
checkFood :: Float -> Float -> [Food] -> Bool
checkFood _ _ [] = False
checkFood xp yp ((Food (x,y)):xs) 
    | absVal (xp - x) <= 10 && absVal (yp - y) <= 10 = True
    | otherwise = checkFood xp yp xs

-- This function takes in the time passed and the current world state, and updates the world
-- state accordingly based the amount of time passed
stepWorld :: Float -> Game -> Game
stepWorld time (ContinueGame (Player (x,y)) (Monster (xm,ym) s countx) foodList score) 
    | (distx <= 10) && (disty <= 10) = (GameEnd score) -- monster has caught player, and game ends
    | distx <= s && x > xm = (player (Monster (xm + distx,ym) s (incr countx)) foodList score) 
    | distx <= s && x < xm = (player (Monster (xm - distx,ym) s (incr countx)) foodList score) 
    | disty <= s && y > ym = (player (Monster (xm, ym + disty) s 0) foodList score)
    | disty <= s && y < ym = (player (Monster (xm, ym - disty) s 0) foodList score)
    | x < xm = (player (Monster (xm - s,ym) s (incr countx)) foodList score) 
    | x > xm = (player (Monster (xm + s,ym) s (incr countx)) foodList score)
    | y < ym = (player (Monster (xm,ym - s) s 0) foodList score)
    | y > ym = (player (Monster (xm,ym + s) s 0) foodList score)
    | otherwise = (player (Monster (xm, ym) s 0) foodList score)
    where distx = absVal (x - xm) 
          disty = absVal (y - ym)
          player = ContinueGame (Player (x,y))
stepWorld _ (GameEnd score) = (GameEnd score)

-- This function takes in an input x, and returns the absolute of x
absVal :: (Ord a, Num a) => a -> a
absVal x 
    | x >= 0 = x
    | x < 0 = (-x)

-- This helper function takes in a number, and increments it by 1
incr :: Num a => a -> a
incr x = x + 1
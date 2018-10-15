-- CPSC 312 Project 1 by Curtis Fox and Jennifer Ahn
module Explorer where

import Graphics.Gloss
import Graphics.Gloss.Interface.Pure.Game

main :: IO ()
main = play (InWindow "UI Explorer" (1000, 1000) (0,0)) blue 25 startingWorld renderWorld handleKeys stepWorld

data Game = ContinueGame Player
          | EndGame
          deriving (Eq,Show)
 
data Player = Player Position
              deriving (Eq,Show)

type Position = (Float, Float)

startingWorld = (ContinueGame (Player (0,0)))

renderWorld (ContinueGame (Player (x,y))) = pictures [translate x y (color red (circleSolid 5))]

handleKeys (EventKey (Char 'w') _ _ _) (ContinueGame (Player (x,y)))  
    | y < 300 = (ContinueGame (Player (x, y + 20)))
    | y > 300 = (ContinueGame (Player (x, 300)))
handleKeys (EventKey (Char 'a') _ _ _) (ContinueGame (Player (x,y))) 
    | x > -300 = (ContinueGame (Player (x - 20, y)))
    | x < -300 = (ContinueGame (Player (-300, y)))
handleKeys (EventKey (Char 's') _ _ _) (ContinueGame (Player (x,y))) 
    | y > -300 = (ContinueGame (Player (x, y - 20)))
    | y < -300 = (ContinueGame (Player (x, -300)))
handleKeys (EventKey (Char 'd') _ _ _) (ContinueGame (Player (x,y))) 
    | x < 300 = (ContinueGame (Player (x + 20,y)))
    | x > 300 = (ContinueGame (Player (300,y)))
handleKeys _ game = game

stepWorld time (ContinueGame (Player (x,y))) = (ContinueGame (Player (x,y)))



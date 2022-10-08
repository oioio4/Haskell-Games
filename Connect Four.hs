 {-# LANGUAGE OverloadedStrings #-}
import CodeWorld

type Player = Int
type Posn = (Int, Int)
type Piece = (Player, Posn)
type Board = [Piece]
type BoardOld = [[Int]] 

data GameState = GameState{board :: Board, player :: Int}

playerColor player 
  | player == 1 = red
  | otherwise = blue
-- drawingOf is only for the final render
switchPlayer player 
  | player == 1 = 2
  | otherwise = 1

displayPiece :: Piece -> Picture
displayPiece (p, (x, y)) = 
  (colored  (playerColor p) (translated x' y' (solidCircle 0.5)))
  where
    x' = fromIntegral x
    y' = fromIntegral y
 
maximum2 [] = -1
maximum2 xs = maximum xs

topOfPile [] col = 0
topOfPile board col = (maximum2 [y | (n, (x, y)) <- board, x == col]) + 1

is_move_legal board col = col >= 0 && col < 7 && topOfPile board col < 6  

posn_add (x, y) (a, b) = (x + a, y + b)
posn_mult n (x, y) = (n*x, n*y)
four_posns posn dir = [posn_add posn (posn_mult n dir) | n <- [0..3]]

on_board posns = [(x, y) | (x, y) <- posns, x >= 0, x <= 5, y >= 0, y <= 5] == posns

all_fours [] _ = []
all_fours (x:xs) dir = [four_posns x dir] ++ all_fours xs dir

--possible_fours [(1,0),(2,0)] (1,0)
possible_fours xs dir = [n | n <- all_fours xs dir, on_board n, length n == 4]

allpossible_fours xs [] = []
allpossible_fours xs (d:ds) = possible_fours xs d ++ allpossible_fours xs ds

getPlayer x y [] = 0
getPlayer x y board
  | (x, y) == snd (head board) = fst (head board)
  | otherwise = getPlayer x y (tail board)
  
returnPlayer (x,y) board = getPlayer x y board
four_inarow' _ [] = False
four_inarow' [a] _ = True
four_inarow' (x:y:xs) board
  | (returnPlayer x board == returnPlayer y board) && (returnPlayer x board /= 0) = four_inarow' (y:xs) board
  | otherwise = False

--four_inarow [[(1,0),(2,0),(3,0),(4,0)], [(1,1),(2,1), (3,1),(4,1)]]
four_inarow [] _ = False
four_inarow (x:xs) board = four_inarow' x board || four_inarow xs board

directions :: [(Int, Int)]
directions = [(1,0), (0,1), (1,1), (1,-1)]

possible_points :: [(Int, Int)]
possible_points = [(x, y) | x <- [0..5], y <- [0..5]]

--four_inarow (allpossible_fours possible_points directions) board1

is_won board = four_inarow (allpossible_fours possible_points directions) board

playerWon player
  | player == 1 = colored blue (lettering ("Player 2 Won"))
  | otherwise = colored red (lettering ("Player 1 Won"))
drawHandler g 
  | is_won (board g) = playerWon (player g)
  | length (board g) == 42 = lettering "Draw"
  | otherwise = pictures ((translated (-0.5) (-0.5) coordinatePlane) : [displayPiece p | p <- board g])
  
make_move board col player = (player, (col, topOfPile board col)) : board

eventHandler (PointerPress (x, y)) g 
  | is_move_legal (board g) x' = GameState (make_move (board g) x' (player g))  (switchPlayer (player g))
  | otherwise = g
    where
      x' = round x
eventHandler _ g = g
    
  
main :: IO()
main = activityOf (GameState [] 1) eventHandler drawHandler 
  
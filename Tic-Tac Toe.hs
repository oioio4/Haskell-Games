 {-# LANGUAGE OverloadedStrings #-}
import CodeWorld

type Player = Int
type Posn = (Int, Int)
type Piece = (Player, Posn)
type Board = [Piece]

data GameState = GameState{board :: Board, player :: Int}

playerColor player 
  | player == 1 = red
  | otherwise = blue
-- drawingOf is only for the final render
switchPlayer player 
  | player == 1 = 2
  | otherwise = 1

displayPiece :: Piece -> Picture
displayPiece (p, (x, y)) 
  | p == 1 = (colored  (playerColor 1) (translated x' y' (lettering "X")))
  | p == 2 = (colored  (playerColor 2) (translated x' y' (lettering "O")))
  where
    x' = fromIntegral x
    y' = fromIntegral y
 
getPlayer x y [] = 0
getPlayer x y board
  | (x, y) == snd (head board) = fst (head board)
  | otherwise = getPlayer x y (tail board)

toMatrix :: Int -> Int -> [(Int, (Int, Int))] -> [[Int]]
takeN _ [] = []
takeN n xs = [take n xs] ++ takeN n (drop n xs)
toMatrix' xmax ymax board = [getPlayer x y board |y <- [0..ymax - 1], x <- [0..xmax - 1]]
toMatrix _ _ [] = []
toMatrix xmax ymax board = takeN xmax (toMatrix' xmax ymax board)

merge [] = []
merge (x:xs) = x ++ merge xs
toPieces'' xs = zip (merge xs) [0..]

toPieces xs = [(a, (mod n (length (head xs)), quot n (length (head xs)))) | (a, n) <- toPieces'' xs, a /= 0]

drawBoard' [0] = "_"
drawBoard' [1] = "X"
drawBoard' [2] = "O"
drawBoard' (x:xs) 
  | x == 1 = 'X' : drawBoard' xs
  | x == 2 = 'O' : drawBoard' xs
  | otherwise = '_' : drawBoard' xs

drawBoard [x] = drawBoard' x
drawBoard (x:xs) = drawBoard' x ++ "\n" ++ drawBoard xs

is_move_legal board col row = not (col < 0 || col > 2 || row < 0 || row > 2)

make_move board col row player = (player, (col, row)) : board

posn_add (x, y) (a, b) = (x + a, y + b)
posn_mult n (x, y) = (n*x, n*y)
three_posns posn dir = [posn_add posn (posn_mult n dir) | n <- [0..2]]

on_board posns = [(x, y) | (x, y) <- posns, x >= 0, x <= 2, y >= 0, y <= 2] == posns

all_threes [] _ = []
all_threes (x:xs) dir = [three_posns x dir] ++ all_threes xs dir

--possible_threes [(1,0),(2,0)] (1,0)
possible_threes xs dir = [n | n <- all_threes xs dir, on_board n, length n == 3]

allpossible_threes xs [] = []
allpossible_threes xs (d:ds) = possible_threes xs d ++ allpossible_threes xs ds

returnPlayer (x,y) board = getPlayer x y board
three_inarow' _ [] = False
three_inarow' [a] _ = True
three_inarow' (x:y:xs) board
  | (returnPlayer x board == returnPlayer y board) && (returnPlayer x board /= 0) = three_inarow' (y:xs) board
  | otherwise = False

--three_inarow [[(1,0),(2,0),(3,0),(4,0)], [(1,1),(2,1), (3,1),(4,1)]]
three_inarow [] _ = False
three_inarow (x:xs) board = three_inarow' x board || three_inarow xs board

directions :: [(Int, Int)]
directions = [(1,0), (0,1), (1,1), (1,-1)]

possible_points :: [(Int, Int)]
possible_points = [(x, y) | x <- [0..6], y <- [0..5]]

--three_inarow (allpossible_threes possible_points directions) board1

is_won board = three_inarow (allpossible_threes possible_points directions) board

playerWon player
  | player == 1 = colored blue (lettering ("Player 2 Won"))
  | otherwise = colored red (lettering ("Player 1 Won"))
drawHandler g 
  | is_won (board g) = playerWon (player g)
  | length (board g) == 9 && not (is_won (board g)) = lettering "Draw"
  | otherwise = pictures ((translated (-0.5) (-0.5) coordinatePlane) : [displayPiece p | p <- board g])

eventHandler (PointerPress (x, y)) g 
  | is_move_legal (board g) x' y' = GameState (make_move (board g) x' y' (player g))  (switchPlayer (player g))
  | otherwise = g
    where
      x' = round x
      y' = round y
eventHandler _ g = g
    
  
main :: IO()
main = activityOf (GameState [] 1) eventHandler drawHandler 
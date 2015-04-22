--Christian Wert and Jacob Adelgren

module NIM where

import Control.Exception

main = do
	putStrLn "Welcome to NIM\n"
	humanMove newSet

printBoard :: [Int] -> IO ()
printBoard board = putStrLn (stringBoard board)

stringBoard :: [Int] -> String
stringBoard board = "1: " ++ stringSticks (board !! 0) ++ "\n2: " ++ stringSticks (board !! 1) ++ "\n3: " ++ stringSticks (board !! 2)

stringSticks :: Int -> String
stringSticks 1 = "X"
stringSticks n = "X" ++ stringSticks (n - 1)

removeSticks :: Int -> Int -> [Int] -> [Int]
removeSticks row sticks board
	| row == 1 = [(board !! 0 - sticks), (board !! 1), (board !! 2)]
	| row == 2 = [(board !! 0), (board !! 1 - sticks), (board !! 2)]
	| row == 3 = [(board !! 0), (board !! 1), (board !! 2 - sticks)]
	| otherwise = []

sticksInRow :: Int -> [Int] -> Int
sticksInRow row board
	| row == 1 = board !! 0
	| row == 2 = board !! 1
	| row == 3 = board !! 2

newSet :: [Int]
newSet = [4,3,7]

gameOver :: [Int] -> Bool
gameOver board
	| sum board == 0 = True
	| otherwise = False

humanMove :: [Int] -> IO String
humanMove board = do
	printBoard board
	return "woiefh"

	


--GRADING CRITERIA
--TODO(2) Display NIM to start the game
--TODO(4) Display the board as rows of Xs. Should display before human's turn and also before the computer's turn. Be sure to display the row number.
--TODO(4) Prompt the user and accept a row
--TODO(2) Ensure the row number is valid
--TODO(2) Ensure the row contains sticks
--TODO(2) Prompt the user and accept a number of sticks
--TODO(6) Update the game state (board) based on row and # sticks
--TODO(4) Detect if the human wins, display a message
--TODO(4) Detect if the computer wins, display a message
--TODO(5) Include type signatures for all methods

--TODO(10) The "dumb" computer player always chooses the first row that contains any sticks, and takes all the sticks in that row
--TODO(15) The "random" computer randomly selects a valid row and a number of sticks that does not exceed the available sticks in that row.
--TODO(15) The "smart" computer always wins. Do not use trial and error.

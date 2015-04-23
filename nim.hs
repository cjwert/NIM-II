--Christian Wert and Jacob Adelgren

module NIM where

import Control.Exception

printBoard :: [Int] -> IO ()
printBoard board = putStrLn (stringBoard board)

stringBoard :: [Int] -> String
stringBoard board = "\n1: " ++ stringSticks (board !! 0) ++ "\n2: " ++ stringSticks (board !! 1) ++ "\n3: " ++ stringSticks (board !! 2) ++ "\n"

stringSticks :: Int -> String
stringSticks 0 = ""
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
	| otherwise = 0

makeMove :: [Int] -> Int -> Int -> Maybe [Int]
makeMove board row sticks
	| ((row < 1) || (row > 3)) = Nothing
	| sticks > inRow = Nothing
	| ((sticks > 0) && (sticks <= inRow)) = Just $ removeSticks row sticks board
	| otherwise = Nothing
	where
		inRow = sticksInRow row board


newSet :: [Int]
newSet = [4,3,7]

gameOver :: [Int] -> Bool
gameOver board
	| sum board == 0 = True
	| otherwise = False
	
firstRowWithSticks :: [Int] -> Int
firstRowWithSticks board
	| ((sticksInRow 1 board) > 0) = 1
	| ((sticksInRow 2 board) > 0) = 2
	| ((sticksInRow 3 board) > 0) = 3

--getInt :: IO Int
--getInt = do
--	putStrLn "Enter a Number: "
--	input <- try getLine
--	case input of
--		Left (SomeException e) -> do
--			putStrLn $ "Invalid input." ++ show e
--			getInt
--		Right valid -> do
--			return (read valid :: Int)

getInt :: String -> IO Int
getInt message = do
	putStrLn message
	input <- getLine
	case (reads input :: [(Int, String)]) of
		[] -> do
			putStrLn $ "Invalid input."
			getInt message
		[(n,_)] -> do
			return n


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

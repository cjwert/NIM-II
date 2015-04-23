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

--nim_smartComputer.hs
module Smart where

import NIM
import Data.Bits


main = do
	putStrLn "Welcome to NIM\n"
	result <- humanMove newSet
	putStrLn result

humanMove :: [Int] -> IO String
humanMove board = do
	printBoard board
	row <- getInt "Enter a row:"
	sticks <- getInt "Enter number of sticks:"
	let result = makeMove board row sticks
	case result of 
		Nothing -> do
			putStrLn "Move invalid. Try again."
			humanMove board
		Just board -> do
			if (gameOver board)
				then do
					return "Congrats! You win!"
				else
					smartComputerMove board

smartComputerMove :: [Int] -> IO String
smartComputerMove board = do
	printBoard board
	let parity = sumRow board
	let result = smartMove board parity	
	case result of 
		Nothing -> do
			smartComputerMove board
		Just board -> do
			if (gameOver board)
				then do
					return "Sorry. The computer wins."
				else
					humanMove board

sumRow :: (Num b, Bits b) => [b] -> b										 
sumRow board = foldr xor 0 board

smartMove :: [Int] -> Int -> Maybe [Int]
smartMove board parity
	| ((head board - parity) >= 0) && ((xor 0 (head board)) > (xor (head board) parity)) = makeMove board 1 parity
	| ((board !! 1 - parity) >= 0) && ((xor 0 (board !! 1)) > (xor (board !! 1) parity)) = makeMove board 2 parity
	| ((last board - parity) >= 0) && ((xor 0 (last board)) > (xor (last board) parity)) = makeMove board 3 parity
	| (xor 0 (head board)) > ((xor (head board) parity)) = makeMove board 1 (parity - head board)
	| (xor 0 (board !! 1)) > ((xor (board !! 1) parity)) = makeMove board 2 (parity - board !! 1)
	| (xor 0 (last board)) > ((xor (last board) parity)) = makeMove board 3 (parity - last board)
	| otherwise = Nothing
	
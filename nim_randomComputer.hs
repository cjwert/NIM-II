--nim_randomComputer.hs

module Random where

import NIM
import System.Random

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
					randomComputerMove board

randomComputerMove :: [Int] -> IO String
randomComputerMove board = do
	printBoard board
	row <- randomRIO(1,3)
	sticks <- randomRIO(1, sticksInRow row board)
	let result = makeMove board row sticks
	case result of 
		Nothing -> do
			randomComputerMove board
		Just board -> do
			if (gameOver board)
				then do
					return "Sorry. The computer wins."
				else do
					putStrLn "Board after computer move:"
					humanMove board
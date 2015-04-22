--nim_dumbComputer.hs

module Dumb where

import NIM

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
					dumbComputerMove board

dumbComputerMove :: [Int] -> IO String
dumbComputerMove board = do
	printBoard board
	let row = 1
	let sticks = sticksInRow row board
	let result = makeMove board row sticks
	case result of 
		Nothing -> do
			dumbComputerMove board
		Just board -> do
			if (gameOver board)
				then do
					return "Sorry. The computer wins."
				else
					humanMove board

firstRowWithSticks :: [Int] -> Int
firstRowWithSticks board
	| ((sticksInRow 1 board) > 0) = 1
	| ((sticksInRow 2 board) > 0) = 2
	| ((sticksInRow 3 board) > 0) = 3
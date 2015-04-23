--nim_smartComputer.hs
module Smart where

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
					smartComputerMove board

smartComputerMove :: [Int] -> IO String
smartComputerMove board = do
	printBoard board
        let row = 1
        let sticks = 1
	let result = makeMove board row sticks
	case result of 
		Nothing -> do
			smartComputerMove board
		Just board -> do
			if (gameOver board)
				then do
					return "Sorry. The computer wins."
				else
					humanMove board
                                        
rowSums :: Monad m => [Int] -> m [Int]                                      
rowSums board = do
        let one =to4Bit $ sticksInRow 1 board
        let two = to4Bit $ sticksInRow 2 board
        let three = to4Bit $ sticksInRow 3 board
        let a = [ addBits (one !! 0) (two !! 0), addBits (one !! 1) (two !! 1), addBits (one !! 2) (two !! 2), addBits (one !! 03) (two !! 3) ] 
        let b = [ addBits (a !! 0) (three !! 0), addBits (a !! 1) (three !! 1), addBits (a !! 2) (three !! 2), addBits (a !! 03) (three !! 3) ] 
        return b

addBits :: Int -> Int -> Int
addBits 1 1 = 0
addBits 1 0 = 1
addBits 0 1 = 1
addBits 0 0 = 0

--stableState :: (Num a, Eq a) => [a] -> Bool
stableState board
        | total == 0 = True
        | otherwise = False
        where total = foldr (+) 0 board
        
reduceRowOne board
        | status = board
        | otherwise = [ board !! 0 - 1] ++ tail board
        where status = stableState (rowSums board)
        


to4Bit :: Int -> [Int]
to4Bit x = replicate (4 - length ys) 0 ++ ys
    where ys = toBinary x
                             
bitsToInt :: [Int] -> Int
bitsToInt x = (2*2*2*(x !! 0)) + (2*2*(x !! 1)) + (2*(x !! 2)) + (1*(x !! 3))
                             
-- following from http://stackoverflow.com/questions/9166148/how-to-implement-decimal-to-binary-function-in-haskell
toBinary :: Int -> [ Int ]
toBinary 0 = [ 0 ]
toBinary n = toBinary ( n `quot` 2 ) ++ [ n `rem` 2 ]
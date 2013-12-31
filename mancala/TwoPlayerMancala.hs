module Main where
import MancalaBoard
import Data.List
import Data.Char
import System.IO


main :: IO ()
main = do
    showInstructions    
    let board = initial
    run board
	

run :: MancalaBoard -> IO ()
run board = do
    putStrLn (show board)
    putStrLn ("It's "++ (show (getCurPlayer board))++ "'s Turn. Please input a move: ")
    potMove <- getLine
    if potMove == "q" then 
        return () --quit
    else if (isNum potMove) then do --valid number input
            let potentialMove = read potMove :: Int
            if (not (isAllowedMove board potentialMove)) then
                putStrLn "Invalid Move, please Try Again!"
            else
                putStrLn ("That move was successful")
            let newBoard = move board potentialMove
            if (gameOver newBoard) then do
                putStrLn ("The Game is over, the winner(s) is(are): " ++ (show (winners newBoard)))
                return ()
            else 
                (run newBoard)
    else do
            putStrLn $ "Invalid input, Please try Again!" --user accidentally inputted non-numerals
            run board

showInstructions :: IO ()
showInstructions = do
	    putStrLn $ "Welcome to Mancala! Please Decide who goes first, he/she is PlayerA and the other is PlayerB"
	    putStrLn $ "Each turn, the current player will be asked to select a pit to play, when asked, please enter in the pit number that you want to play, the pit will then be played and the new board will be displayed. If you enter in an invalid choice, it will tell you and you may pick again."
	    putStrLn $ "The game ends when one player has no more stones on his/her side and the player with the most stones in his/her pit plus the stones on his/her side wins. At the end of the game, the computer will tell you who has won."
	    putStrLn $ "To quit at any time, enter in 'q' when asked for a move"
	    putStrLn $ "The game will now begin. Good Luck!"

isNum :: String -> Bool
isNum [] = True
isNum (x:xs)
    | isDigit x = isNum xs
    | not (isDigit x) = False
---------------------------------------
----------------Tests------------------
---------------------------------------
{-
$ ghci MancalaMain.hs
> main
Welcome to Mancala! Please Decide who goes first, he/she is PlayerA and the other is PlayerB
Each turn, the current player will be asked to select a pit to play, when asked, please enter in the pit number that you want to play, the pit will then be played and the new board will be displayed. If you enter in an invalid choice, it will tell you and you may pick again.
The game ends when one player has no more stones on his/her side and the player with the most stones in his/her pit plus the stones on his/her side wins. At the end of the game, the computer will tell you who has won.
To quit at any time, enter in 'q' when asked for a move
The game will now begin. Good Luck!
                   ______________________________________________________________________________
                 |  pit 12: 4  |  pit 11: 4  |  pit 10: 4  |  pit 9: 4  |  pit 8: 4  |  pit 7: 4  | 
PlayerB store: 0 | ------------------------------------------------------------------------------ |  PlayerA store: 0
                 |  pit 0: 4   |  pit 1: 4   |  pit 2: 4   |  pit 3: 4  |  pit 4: 4  |  pit 5: 4  | 
                   ______________________________________________________________________________

It's PlayerA's Turn. Please input a move: 
asdf   -- checks if the program sees nonsense input
Invalid input, Please try Again!
                   ______________________________________________________________________________
                 |  pit 12: 4  |  pit 11: 4  |  pit 10: 4  |  pit 9: 4  |  pit 8: 4  |  pit 7: 4  | 
PlayerB store: 0 | ------------------------------------------------------------------------------ |  PlayerA store: 0
                 |  pit 0: 4   |  pit 1: 4   |  pit 2: 4   |  pit 3: 4  |  pit 4: 4  |  pit 5: 4  | 
                   ______________________________________________________________________________

It's PlayerA's Turn. Please input a move: 
2
That move was successful
                   ______________________________________________________________________________
                 |  pit 12: 4  |  pit 11: 4  |  pit 10: 4  |  pit 9: 4  |  pit 8: 4  |  pit 7: 4  | 
PlayerB store: 0 | ------------------------------------------------------------------------------ |  PlayerA store: 1
                 |  pit 0: 4   |  pit 1: 4   |  pit 2: 0   |  pit 3: 5  |  pit 4: 5  |  pit 5: 5  | 
                   ______________________________________________________________________________

It's PlayerA's Turn. Please input a move:  --see how because A's turn ended in his store, he goes again
10
Invalid Move, please Try Again! --invalid move, he gets another chance
                   ______________________________________________________________________________
                 |  pit 12: 4  |  pit 11: 4  |  pit 10: 4  |  pit 9: 4  |  pit 8: 4  |  pit 7: 4  | 
PlayerB store: 0 | ------------------------------------------------------------------------------ |  PlayerA store: 1
                 |  pit 0: 4   |  pit 1: 4   |  pit 2: 0   |  pit 3: 5  |  pit 4: 5  |  pit 5: 5  | 
                   ______________________________________________________________________________

It's PlayerA's Turn. Please input a move: 
q --checks if it quits
$
Have Fun Playing!
-}

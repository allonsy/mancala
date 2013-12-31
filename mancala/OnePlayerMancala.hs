-- Alec Snyder
-- Mancala with Ai
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
    if (isPlayerB (getCurPlayer board))  then do
        let newBoard = move board (bestAiMove board)
        putStrLn $ "The Computer has moved. It moved pit " ++ (show (bestAiMove board)) ++ "."
        if (gameOver newBoard) then do
                    putStrLn ("The Game is over, the winner(s) is(are): " ++ (show (winners newBoard)))
                    return ()
        else 
                (run newBoard)
    else do
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
	    putStrLn $ "Welcome to Mancala versus the Computer! You are PlayerA and the computer is PlayerB"
	    putStrLn $ "Each turn, the current player will be asked to select a pit to play, when asked, please enter in the pit number that you want to play, the pit will then be played and the new board will be displayed. If you enter in an invalid choice, it will tell you and you may pick again."
	    putStrLn $ "The game ends when one player has no more stones on his/her side and the player with the most stones in his/her pit plus the stones on his/her side wins. At the end of the game, the computer will tell you who has won."
	    putStrLn $ "To quit at any time, enter in 'q' when asked for a move"
	    putStrLn $ "The game will now begin. Good Luck!"

isNum :: String -> Bool
isNum [] = True
isNum (x:xs)
    | isDigit x = isNum xs
    | not (isDigit x) = False


-------------------------------------------------
-------------------Tests-------------------------
-------------------------------------------------
{-
 - Ai Game:
 Welcome to Mancala versus the Computer! You are PlayerA and the computer is PlayerB
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
0
That move was successful
                   ______________________________________________________________________________
                 |  pit 12: 4  |  pit 11: 4  |  pit 10: 4  |  pit 9: 4  |  pit 8: 4  |  pit 7: 4  | 
PlayerB store: 0 | ------------------------------------------------------------------------------ |  PlayerA store: 0
                 |  pit 0: 0   |  pit 1: 5   |  pit 2: 5   |  pit 3: 5  |  pit 4: 5  |  pit 5: 4  | 
                   ______________________________________________________________________________

The Computer has moved. It moved pit 9.
                   ______________________________________________________________________________
                 |  pit 12: 5  |  pit 11: 5  |  pit 10: 5  |  pit 9: 0  |  pit 8: 4  |  pit 7: 4  | 
PlayerB store: 1 | ------------------------------------------------------------------------------ |  PlayerA store: 0
                 |  pit 0: 0   |  pit 1: 5   |  pit 2: 5   |  pit 3: 5  |  pit 4: 5  |  pit 5: 4  | 
                   ______________________________________________________________________________

The Computer has moved. It moved pit 12.
                   ______________________________________________________________________________
                 |  pit 12: 0  |  pit 11: 5  |  pit 10: 5  |  pit 9: 0  |  pit 8: 4  |  pit 7: 4  | 
PlayerB store: 2 | ------------------------------------------------------------------------------ |  PlayerA store: 0
                 |  pit 0: 1   |  pit 1: 6   |  pit 2: 6   |  pit 3: 6  |  pit 4: 5  |  pit 5: 4  | 
                   ______________________________________________________________________________

It's PlayerA's Turn. Please input a move: 

--ommited rest of gameplay for conciseness --

--endgame:
It's PlayerA's Turn. Please input a move: 
2
That move was successful
                    ______________________________________________________________________________
                  |  pit 12: 1  |  pit 11: 0  |  pit 10: 0  |  pit 9: 0  |  pit 8: 0  |  pit 7: 0  | 
PlayerB store: 31 | ------------------------------------------------------------------------------ |  PlayerA store: 13
                  |  pit 0: 0   |  pit 1: 0   |  pit 2: 0   |  pit 3: 1  |  pit 4: 2  |  pit 5: 0  | 
                    ______________________________________________________________________________

The Computer has moved. It moved pit 12.
The Game is over, the winner(s) is(are): [PlayerB]
-- Alas, to be beaten so badly by our own Ai!
-}

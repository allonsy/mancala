module MancalaBoard (MancalaBoard, Player, initial, getCurPlayer,
            getBoardData, numCaptured, move, allowedMoves, isAllowedMove,
            gameOver, winners, bestAiMove, isPlayerB) where

import Data.List as List -- for List.elemIndex
import Data.Maybe as Maybe -- for List.elemIndex

{-
 - The stones on a Mancala board are simply recorded as a list of Ints.  The
 -  Ints come in the following order:
 - 1. The boardSize pits belonging to PlayerA
 - 2. The store belonging to PlayerA
 - 3. The boardSize pits belonging to PlayerB
 - 4. The store belonging to PlayerB
 -}

data MancalaBoard = MancalaBoardImpl [Int] Player deriving (Eq)

data Player = PlayerA | PlayerB deriving (Eq, Show)

---- Functions/constants for Player ----

allPlayers = [PlayerA, PlayerB]
numPlayers = length allPlayers


playerNum :: Player -> Int
playerNum p = fromJust $ List.elemIndex p allPlayers


playerWithNum :: Int -> Player
playerWithNum i = allPlayers !! i


nextPlayer :: Player -> Player
{- Find the player whose turn is next -}
nextPlayer p = playerWithNum $ ((playerNum p) + 1) `mod` numPlayers


---- Functions/constants for MancalaBoard ----

{- number of pits on each side -}
boardSize = 6
{- number of stones in each pit -}
startStones = 4

{- the initial mancala board -}
initial :: MancalaBoard
initial = MancalaBoardImpl (concat $ take numPlayers (repeat boardSide)) PlayerA
                        -- One side of board                pit at end
    where boardSide = take boardSize (repeat startStones) ++ [0]


{- return the index of the first pit belonging to a player -}
indexForFirstPit :: Player -> Int
indexForFirstPit p = (playerNum p) * (boardSize + 1)


{- return the index of the store for that player -}
indexForPlayerStore :: Player -> Int
indexForPlayerStore p = boardSize + (indexForFirstPit p)


{- return the indices for the pits (without the store) for a player -}
indicesForPlayerSide :: Player -> [Int]
indicesForPlayerSide p = [firstPit .. lastPit] where
    firstPit = indexForFirstPit p
    lastPit = firstPit + boardSize - 1


---- Retrieve information about Mancala Board
{- return the player who has the current turn -}
getCurPlayer :: MancalaBoard -> Player
getCurPlayer (MancalaBoardImpl _ player) = player


{- return the list of all pits in the board -}
getBoardData :: MancalaBoard -> [Int]
getBoardData (MancalaBoardImpl pits _) = playerAPits ++ playerBPits where
    playerAPits = (take 6 pits)
    playerBPits = take 6 (drop 7 pits) --takes the first 6 pits, skips the store of PlayerA, then then next six pits, dropping the last value which is PlayerB's store


{- return the side of the board for a specified player, including the store at
 - the end -}
playerSide :: MancalaBoard -> Player -> [Int]
playerSide (MancalaBoardImpl pits _) player = take 7 (drop (indexForFirstPit player) pits)

{- return the number of captured pieces in specified player's store -}
numCaptured :: MancalaBoard -> Player -> Int
numCaptured (MancalaBoardImpl pits _) player = (!!) (pits) (indexForPlayerStore player)


{- allowedMoves returns a list of valid moves for the current player:
 - ie. the indices of pits which belong to that player, and which contain one
 - or more pieces -}
allowedMoves :: MancalaBoard -> Player -> [Int]
allowedMoves (MancalaBoardImpl pits _) player = checkMovesByIndices (indicesForPlayerSide player) where
    checkMovesByIndices [] = []
    checkMovesByIndices (x:xs)
        |  (!!) pits x >= 1 = x : checkMovesByIndices xs
        | otherwise = checkMovesByIndices xs


{- check that a move is valid for the current player -}
isAllowedMove :: MancalaBoard -> Int -> Bool
isAllowedMove (MancalaBoardImpl pits player) potentialMove 
    | notElem potentialMove (allowedMoves (MancalaBoardImpl pits player) player) = False
    | otherwise = True


{- We number the pits from 0 to 13 (2 players, 6 pits each and 1 store each)
 - This function takes a board and applies the move where the player selects
 - the numbered pit, giving back an updated board after the move -}
move :: MancalaBoard -> Int -> MancalaBoard
move (MancalaBoardImpl pits player) potMove
    | not (isAllowedMove (MancalaBoardImpl pits player) potMove) = (MancalaBoardImpl pits player) --if this potential move is illegal, keep the board the same. Don't change turns.
    | otherwise = MancalaBoardImpl (playPit ((!!) pits potMove) (potMove +1) (changeTo0 potMove pits)) (nextTurn) where --if  a valid move, perform it and return the board, with it being the next player's turn.
        playPit 0 pos newpits = newpits
        playPit n 14 newpits = playPit n 0 newpits
        playPit n pos newpits
            | pos == opponentStore = playPit n ((indexForPlayerStore (nextPlayer player)) + 1) newpits --skip this pit if its the opponent's store.
            | otherwise = playPit (n-1) (pos +1) (changeBy1 pos newpits)
        changeBy1 pos list = (take pos list) ++ ((((!!) list pos) + 1): []) ++ (drop (pos+1) list) --adds one to the number at this position
        changeTo0 pos list = (take pos list) ++ ([0]) ++ (drop (pos+1) list) --changes the number at this position to 0, i.e. when this pit is this move.
        opponentStore = indexForPlayerStore (nextPlayer player)
        nextTurn -- checks to see if the player's turn ends at his store, if so, give him/her another turn.
            | (potMove + (pits !! potMove)) `mod` 14 == (indexForPlayerStore player) = player
            | otherwise = nextPlayer player
{- gameOver checks to see if the game is over (i.e. if one player's side of the
 - board is all empty -}
gameOver :: MancalaBoard -> Bool
gameOver (MancalaBoardImpl pits player) = (take 6 pits)==[0,0,0,0,0,0] || (take 6 (drop 7 pits)) == [0,0,0,0,0,0]


{- winner returns a list of players who have the top score: there will only be 
 - one in the list if there is a clear winner, and none if it is a draw -}
winners :: MancalaBoard -> [Player]
winners (MancalaBoardImpl pits player) 
    | sum (take 7 pits) > sum (drop 7 pits) = [PlayerA]
    | sum (take 7 pits) < sum (drop 7 pits) = [PlayerB]
    | otherwise = [PlayerA, PlayerB]

---- show
instance Show MancalaBoard where --ASCII mancala board
    show (MancalaBoardImpl pits player) = strA ++ strB ++ strC ++ strD ++ strE where
            strA = take (seg) (repeat ' ') ++ str0 ++ "\n"
            strB = take (seg-2) (repeat ' ') ++"| " ++ str2 ++ " | " ++ "\n"
            strC = pit13 ++ " | " ++str3++" | "++pit6 ++ "\n"
            strD = take (seg-2) (repeat ' ') ++"| "++ str4 ++ " | " ++ "\n"
            strE = take (seg) (repeat ' ')++ str0 ++ "\n"
            pit0 = "pit 0: " ++ (show (pits !! 0))
            pit1 = "pit 1: " ++ (show (pits !! 1))
            pit2 = "pit 2: " ++ (show (pits !! 2))
            pit3 = "pit 3: " ++ (show (pits !! 3))
            pit4 = "pit 4: " ++ (show (pits !! 4))
            pit5 = "pit 5: " ++ (show (pits !! 5))
            pit6 = " PlayerA store: " ++ (show (pits !! 6))
            pit7 = "pit 7: " ++ (show (pits !! 7))
            pit8 = "pit 8: " ++ (show (pits !! 8))
            pit9 = "pit 9: " ++ (show (pits !! 9))
            pit10 = "pit 10: " ++ (show (pits !! 10))
            pit11 = "pit 11: " ++ (show (pits !! 11))
            pit12 = "pit 12: " ++ (show (pits !! 12))
            pit13 = "PlayerB store: " ++ (show (pits !! 13))
            pairUp num1 num2 --make proper spacing for the numbers.
                | (length num1) == (length num2) = (" " ++ num1 ++ " "," " ++ num2 ++ " ")
                | (length num1) < (length num2) = pairUp (num1 ++ " ") num2
                | (length num2) < (length num1) = pairUp num1 (num2 ++ " ")
            str2 = (snd $ pairUp pit0 pit12) ++ " | " ++ (snd $ pairUp pit1 pit11) ++ " | " ++ (snd $ pairUp pit2 pit10) ++ " | " ++ (snd $ pairUp pit3 pit9) ++ " | " ++ (snd $ pairUp pit4 pit8) ++ " | " ++ (snd $ pairUp pit5 pit7)
            str1 = take (length str2) (repeat ' ')
            str3 = take (length str2) (repeat '-')
            str0 = take (length str2) (repeat '_')
            str4 = (fst $ pairUp pit0 pit12) ++ " | " ++ (fst $ pairUp pit1 pit11) ++ " | " ++ (fst $ pairUp pit2 pit10) ++ " | " ++ (fst $ pairUp pit3 pit9) ++ " | " ++ (fst $ pairUp pit4 pit8) ++ " | " ++ (fst $ pairUp pit5 pit7)
            seg = length (pit13 ++ " | ")
{-
Note: The board that I have set up sets pit 0 to be the pit on the bottom far left with moves going counterclockwise. The Store of PlayerA is always to pit 6 and the Store of playerB is always at pit 13. The bottom Row is always the home row of Player A and the top row is always the home row of Player B. Ai Always plays as PlayerB. 
-}


-------------------------------------------------
-----The Following are AI functions--------------
------AI is always PlayerB-----------------------
getAiStoreMoves :: MancalaBoard -> Int --want Ai's move to end on his own store so he/she can go again, if possible
getAiStoreMoves (MancalaBoardImpl pits PlayerB)
    | getCurPlayer (move (MancalaBoardImpl pits PlayerB) 12) == PlayerB && (isAllowedMove  (MancalaBoardImpl pits PlayerB) 12) = 12
    | getCurPlayer (move (MancalaBoardImpl pits PlayerB) 11) == PlayerB && (isAllowedMove  (MancalaBoardImpl pits PlayerB) 11) = 11
    | getCurPlayer (move (MancalaBoardImpl pits PlayerB) 10) == PlayerB && (isAllowedMove  (MancalaBoardImpl pits PlayerB) 10) = 10
    | getCurPlayer (move (MancalaBoardImpl pits PlayerB) 9) == PlayerB && (isAllowedMove  (MancalaBoardImpl pits PlayerB) 9) = 9
    | getCurPlayer (move (MancalaBoardImpl pits PlayerB) 8) == PlayerB && (isAllowedMove  (MancalaBoardImpl pits PlayerB) 8) = 8
    | getCurPlayer (move (MancalaBoardImpl pits PlayerB) 7) == PlayerB && (isAllowedMove  (MancalaBoardImpl pits PlayerB) 7) = 7
    | otherwise = (-1)

getBestAiStoreMove :: MancalaBoard -> [Int] -- for each pit, return the amount of stones that it increases Ai's store
getBestAiStoreMove board = map (numStoreIncreased board) [7,8,9,10,11,12]

numStoreIncreased :: MancalaBoard -> Int -> Int --just for Ai, tells how much the Ai's store increases for the given move
numStoreIncreased board pos = (getAiStore (move board stones)) - (getAiStore board) where
    stones = getStonesAtPos board pos

getAiStore :: MancalaBoard -> Int --returns amount of stones in Ai Store
getAiStore (MancalaBoardImpl pits _) = pits !! 13

getStonesAtPos :: MancalaBoard -> Int -> Int
getStonesAtPos (MancalaBoardImpl pits _) pos = pits !! pos

getNumStonesOtherPlayer :: MancalaBoard -> [Int] -- for each Ai pit, returns how much it increases the number of stones of the oppponent's pits. want to minimize this. 
getNumStonesOtherPlayer board = map (numStonesOtherPlayer board) [7,8,9,10,11,12]

numStonesOtherPlayer :: MancalaBoard -> Int -> Int
numStonesOtherPlayer board pos =(-1) * (sum (map (getStonesAtPos (move board pos)) [0,1,2,3,4,5]) - sum (map (getStonesAtPos (board)) [0,1,2,3,4,5]))  

getBestAiPos :: MancalaBoard -> Int -- returns a move that balances out the amount of stones increased in the store with the increase in stones for the opponent.
getBestAiPos board = snd (maxi pits3)  where 
	pits3 = filter allowed pits2
	pits1 = addLists (map (*3) (getBestAiStoreMove board)) (getNumStonesOtherPlayer board)
	pits2 = zip pits1 [7..12]
	allowed (a,b) = isAllowedMove board b
	maxi ((h,t):hs) = maxim (h,t) hs
	maxim (v, pos) [] = (v, pos)
	maxim (v,pos) ((h,t):hs) 
		| v > h = maxim (h,t) hs
		| otherwise = maxim (v,pos) hs

addLists :: [Int] -> [Int] -> [Int]
addLists [] [] = []
addLists (x:xs) (c:cs)= (x+c): addLists xs cs

bestAiMove :: MancalaBoard -> Int --overall best Ai move
bestAiMove board
    | (getAiStoreMoves board) /= (-1) = getAiStoreMoves board
    | otherwise = getBestAiPos board

getPits :: MancalaBoard -> [Int]
getPits (MancalaBoardImpl pits _) = pits

isPlayerB :: Player -> Bool
isPlayerB = (==) PlayerB


------------------------------------------------
----------------Tests---------------------------
------------------------------------------------
a = initial
b = MancalaBoardImpl [0,1,2,0,3,4,7,1,19,0,3,0,4,1] PlayerA
test1 = getCurPlayer a == PlayerA
test2 = getBoardData a == [4,4,4,4,4,4,4,4,4,4,4,4]
test3 = playerSide a PlayerA == [4,4,4,4,4,4,0]
test4 = numCaptured a PlayerA == 0
test5 = allowedMoves b PlayerA == [1,2,4,5] -- checks if it skips all zero pits, skips all stores, and skips all opponent's pits and store.
test6 = isAllowedMove b 10 == False --checks if it prohibits you from choosing an oppponent's pit.
test7 = isAllowedMove b 0 == False --checks if it prohibits you from choosing a pit with zero in it.
test8 = isAllowedMove b 1 == True
test9 = move a 2 == (MancalaBoardImpl [4,4,0,5,5,5,1,4,4,4,4,4,4,0] PlayerB) --checks the move function, checks to make sure that it zeros out the pit, deposits one in the correct store when you move, switches turn to other player.
test10 = gameOver a == False --checks to see if it can detect a GameOver
test11 = winners b == [PlayerB] --checks the winners function
test12 = test1 && test2 && test3 && test4 && test5 && test6 && test7 && test8 && test9 && test10 && test11
{- test of show function
$ ghci MancalaBoard.hs
> a
                   ______________________________________________________________________________
                 |  pit 12: 4  |  pit 11: 4  |  pit 10: 4  |  pit 9: 4  |  pit 8: 4  |  pit 7: 4  | 
PlayerB store: 0 | ------------------------------------------------------------------------------ |  PlayerA store: 0
                 |  pit 0: 4   |  pit 1: 4   |  pit 2: 4   |  pit 3: 4  |  pit 4: 4  |  pit 5: 4  | 
                   ______________________________________________________________________________


see TwoPlayerMancala for game IO program
see OnePlayerMancala for tests with Ai playing.
-}


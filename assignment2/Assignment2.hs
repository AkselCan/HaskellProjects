{- This is a framework in which all functions to be written are "undefined".  -
 - Note that in most cases parameters, pattern-matching and guards have been  -
 - omitted! You will have to add those yourself.                              -}

{-# LANGUAGE TupleSections #-} {- A handy syntax extension. See:

    http://www.haskell.org/ghc/docs/7.6.3/html/users_guide/syntax-extns.html#tuple-sections

-}

module Assignment2 where -- Rename to "Main" if you want to compile the game.
                         -- Don't forget to rename it back when submitting!

import Control.Monad()

import Data.Char
import Data.List
import Data.Maybe()
import Data.Ord
import System.IO

-- | Rose trees

data Rose a = MkRose a [Rose a]
    deriving (Eq, Show)

-- Exercise 1

root :: Rose a -> a
root (MkRose a _) = a

children :: Rose a -> [Rose a]
children (MkRose _ as) = as

-- Exercise 2

size :: Rose a -> Int
size (MkRose _ children) = 1 + sum (map size children)

leaves :: Rose a -> Int
leaves (MkRose _ []) = 1
leaves (MkRose _ children) = sum (map leaves children)

-- | State representation

-- * Players

data Player = P1 | P2
    deriving (Eq, Ord)

instance Show Player where
    show P1 = "Player 1"
    show P2 = "Player 2"

-- Exercise 3

nextPlayer :: Player -> Player
nextPlayer P1 = P2
nextPlayer P2 = P1

-- * Board

data Field = X | O | B
    deriving (Eq, Ord)

instance Show Field where
    show X = "X"
    show O = "O"
    show B = " "

-- Exercise 4

symbol :: Player -> Field
symbol P1 = X
symbol P2 = O

type Row   = (Field, Field, Field)
type Board = (Row, Row, Row)


-- Exercise 5

verticals :: Board -> (Row, Row, Row)
verticals ((field1, field2, field3), (field4, field5, field6), (field7, field8, field9)) = ((field1, field4, field7), (field2, field5, field8), (field3, field6, field9))

diagonals :: Board -> (Row, Row)
diagonals ((field1, field2, field3), (field4, field5, field6), (field7, field8, field9)) = ((field1, field5, field9), (field3, field5, field7))

-- Exercise 6

emptyBoard :: Board
emptyBoard = ((B, B, B), (B, B, B), (B, B, B))

-- Exercise 7

printField :: Field -> Char
printField X = 'X'
printField O = 'O'
printField B = ' '

printRow :: Row -> String
printRow (field1, field2, field3) = [printField field1] ++ "|" ++ [printField field2] ++ "|" ++ [printField field3] ++ "\n"

printBoard :: Board -> String
printBoard (row1, row2, row3) =
    printRow row1 ++ "-+-+-\n" ++
    printRow row2 ++ "-+-+-\n" ++
    printRow row3

-- | Move generation

-- Exercise 8

traverseFst :: (a -> [d]) -> (a, b, c) -> [(d, b, c)]
traverseFst f (a, b, c) = [(d, b, c) | d <- f a]

traverseSnd :: (b -> [d]) -> (a, b, c) -> [(a, d, c)]
traverseSnd f (a, b, c) = [(a, d, c) | d <- f b]

traverseThd :: (c -> [d]) -> (a, b, c) -> [(a, b, d)]
traverseThd f (a, b, c) = [(a, b, d) | d <- f c]

traverseAll :: (a -> [a]) -> (a, a, a) -> [(a, a, a)]
traverseAll f (a, b, c) = 
    traverseFst f (a, b, c) ++ traverseSnd f (a, b, c) ++ traverseThd f (a, b, c)


moveField :: Player -> Field -> [Field]
moveField player B = [symbol player]
moveField _ _ = []

movesRow :: Player -> Row -> [Row]
movesRow player row = traverseAll (moveField player) row

moves :: Player -> Board -> [Board]
moves player = traverseAll (movesRow player)


-- | Gametree generation

-- Exercise 9

hasWinner :: Board -> Maybe Player
hasWinner board 
    | any (\row -> row == (X,X,X)) possRows = Just P1
    | any (\row -> row == (O,O,O)) possRows = Just P2
    | otherwise = Nothing
    where
        (r1, r2, r3) = board
        (vert1, vert2, vert3) = verticals board
        (diag1, diag2) = diagonals board
        possRows = [r1, r2, r3, vert1, vert2, vert3, diag1, diag2] 

-- Exercise 10

gameTree :: Player -> Board -> Rose Board
gameTree player board =
    if isJust (hasWinner board)  -- Stop if there's a winner
    then MkRose board []
    else MkRose board children
  where
    children = map (gameTree (nextPlayer player)) (moves player board)
    isJust Nothing = False
    isJust (Just _) = True

     {-case hasWinner board of
        Just _  -> MkRose board []  -- If there's a winner, no further moves are allowed
        Nothing -> 
            if null nextBoards then  -- If no further moves are possible (it's a draw)
                MkRose board [] 
            else
                MkRose board (map (gameTree (nextPlayer player)) nextBoards)  -- Otherwise, continue the game tree
            where nextBoards = moves player board -}
            

-- | Game complexity

-- Exercise 11

gameTreeComplexity :: Int
gameTreeComplexity = leaves (gameTree P1 emptyBoard)

-- | Minimax

-- Exercise 12

minimax :: Player -> Rose Board -> Rose Int
minimax originalPlayer tree = minimax' originalPlayer tree
  where
    minimax' :: Player -> Rose Board -> Rose Int
    minimax' currentP (MkRose board [])
        | hasWinner board == Just originalPlayer = MkRose 1 []
        | hasWinner board == Just (nextPlayer originalPlayer) = MkRose (-1) []
        | otherwise = MkRose 0 []

    minimax' currentP (MkRose board children)
      = let childRoses = map (minimax' (nextPlayer currentP)) children
            values = map root childRoses
        in MkRose (if currentP == originalPlayer then maximum' values else minimum' values) childRoses

-- * Lazier minimum and maximums

-- Exercise 13

minimum' :: [Int] -> Int
minimum' = foldl' (\acc x -> if acc == -1 then acc else min acc x) 1

maximum' :: [Int] -> Int
maximum' = foldl' (\acc x -> if acc == 1 then acc else max acc x) (-1)

-- | Gameplay

-- Exercise 14

-- Helper function to pair each board with its minimax value
boardValues :: Player -> Board -> [(Board, Int)]
boardValues player board = zip (moves player board) (map root (children (minimax player (gameTree player board))))

makeMove :: Player -> Board -> Maybe Board
makeMove player board = 
    case boardValues player board of
        [] -> Nothing  -- No possible moves
        bs -> Just $ fst $ maximumBy (comparing snd) bs  -- Pick the board with the maximum minimax value

-- | Main

data PlayerType = Human | Computer

instance Show PlayerType where
    show Human    = "H"
    show Computer = "C"

main :: IO ()
main = do
    typeOfP1 <- askFor "Should Player 1 be a (H)uman or a (C)omputer player?"
                       [Human, Computer]
    typeOfP2 <- askFor "Should Player 2 be a (H)uman or a (C)omputer player?"
                       [Human, Computer]

    let playerType :: Player -> PlayerType
        playerType P1 = typeOfP1
        playerType P2 = typeOfP2

        gameLoop :: Player -> Board -> IO ()
        gameLoop p b = do
            putStrLn ("\n" ++ printBoard b)
            case hasWinner b of
                Just p  -> putStrLn (show p ++ " has won!")
                Nothing -> do
                    putStr   ("It's " ++ show p ++ "'s turn. ")
                    mb' <- case playerType p of
                        Human    -> humanMove    p b
                        Computer -> computerMove p b
                    case mb' of
                        Nothing -> do putStr   "No more moves are possible. "
                                      putStrLn "It's a draw."
                        Just b' -> gameLoop (nextPlayer p) b'

        humanMove :: Player -> Board -> IO (Maybe Board)
        humanMove p b =
            case moves p b of
              [] -> return Nothing
              possibleMoves -> do
                putStrLn "Possible moves are:"
                putStrLn (listMoves possibleMoves)
                i <- askFor "Make your choice:" [1..length possibleMoves]
                return (Just (possibleMoves !! (i-1)))

        computerMove :: Player -> Board -> IO (Maybe Board)
        computerMove p b = do
            putStrLn "Thinking..."
            return (makeMove p b)

        listMoves :: [Board] -> String
        listMoves = intercalate "\n"
                    . map (intercalate "    ")
                    . transpose
                    . map lines
                    . map (\(i,b) -> "(" ++ show i ++ "): \n" ++ printBoard b)
                    . zip [1 :: Integer ..]

    gameLoop P1 emptyBoard

askFor :: Show a => String -> [a] -> IO a
askFor m xs = do
    putStr (m ++ " ")
    hFlush stdout
    i <- getLine
    case find ((map toLower i ==) . map toLower . show) xs of
        Nothing -> do putStrLn $ "I didn't understand you. Enter one of: "
                                 ++ intercalate ", " (map show xs) ++ "."
                      askFor m xs
        Just y  -> return y
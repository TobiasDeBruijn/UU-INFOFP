-- Copyright 2023 Tobias de Bruijn

{- This is a framework in which all functions to be written are "undefined".  -
 - Note that in most cases parameters, pattern-matching and guards have been  -
 - omitted! You will have to add those yourself.                              -}

{-# LANGUAGE TupleSections #-} {- A handy syntax extension. See:

    http://www.haskell.org/ghc/docs/7.6.3/html/users_guide/syntax-extns.html#tuple-sections

-}

module Assignment2 where -- Rename to "Main" if you want to compile the game.
                         -- Don't forget to rename it back when submitting!

import Control.Monad

import Data.Char
import Data.List
import Data.Maybe

import System.IO

-- | Rose trees

data Rose a = MkRose a [Rose a]
    deriving (Eq, Show)

-- Exercise 1

root :: Rose a -> a
root (MkRose val _ ) = val

children :: Rose a -> [Rose a]
children (MkRose _ children) = children

-- Exercise 2

size :: Rose a -> Int
size (MkRose x xs) = 1 + sum(map size xs)

leaves :: Rose a -> Int
leaves (MkRose x []) = 1
leaves (MkRose x xs) = sum(map leaves xs)

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
verticals ((r1f1, r1f2, r1f3), (r2f1, r2f2, r2f3), (r3f1, r3f2, r3f3)) 
    = (
        (r1f1, r2f1, r3f1), 
        (r1f2, r2f2, r3f2), 
        (r1f3, r2f3, r3f3)
    )


diagonals :: Board -> (Row, Row)
diagonals ((r1f1, _, r1f3), (_, r2f2, _), (r3f1, _, r3f3))
    = (
        (r1f1, r2f2, r3f3), 
        (r1f3, r2f2, r3f1)
    )

-- Exercise 6

emptyBoard :: Board
emptyBoard = ((B, B, B), (B, B, B), (B, B, B))

-- Exercise 7

printBoard :: Board -> String
printBoard (r1, r2, r3) = printRow r1 ++ printLine ++ printRow r2 ++ printLine ++ printRow r3 ++ "\n" where
    printLine = "\n-+-+-\n"
    printRow (f1, f2, f3) = show f1 ++ "|" ++ show f2 ++ "|" ++ show f3 
-- | Move generation
             
-- Exercise 8
             
moves :: Player -> Board -> [Board]
moves player (r1, r2, r3) = undefined where
    
    movesRow :: Player -> Row -> [Row]
    movesRow player (f1, f2, f3)
        | canMoveField f1 && canMoveField f2 && canMoveField f3 
            = [(symbol player, f2, f3), (f1, symbol player, f3), (f1, f2, symbol player)]
        | canMoveField f1 && canMoveField f2 
            = [(symbol player, f2, f3), (f1, symbol player, f3)]
        | canMoveField f2 && canMoveField f3
            = [(f1, symbol player, f3), (f1, f2, symbol player)]
        | canMoveField f1 && canMoveField f3
            = [(symbol player, f2, f3), (f1, f2, symbol player)]
        | otherwise = []

    canMoveField :: Field -> Bool
    canMoveField field = field == B

-- | Gametree generation

-- Exercise 9

hasWinner :: Board -> Maybe Player
hasWinner board
    | hasPlayerWon P1 board = Just P1
    | hasPlayerWon P2 board = Just P2
    | otherwise = Nothing
    
    where
    -- Check whether the player has won the game
    hasPlayerWon :: Player -> Board -> Bool
    hasPlayerWon player board 
        -- Verticals
        = checkTriple (verticals board) player 
        -- Horizontals
        || checkTriple board player 
        -- Diagonals
        || checkDouble (diagonals board) player

    -- Check whether a player has won any of the rows in the double
    checkDouble :: (Row, Row) -> Player -> Bool
    checkDouble (r1, r2) player 
        = hasPlayerWonRow player r1 
        || hasPlayerWonRow player r2

    -- Check whether the player has won any of the rows in the triple
    checkTriple :: (Row, Row, Row) -> Player -> Bool
    checkTriple (r1, r2, r3) player 
        = hasPlayerWonRow player r1 
        || hasPlayerWonRow player r2 
        || hasPlayerWonRow player r3

    -- Check whether all of the symbols in the row belon to the players
    hasPlayerWonRow :: Player -> Row -> Bool
    hasPlayerWonRow player (f1, f2, f3) 
        = (f1 == symbol player) 
        && (f2 == symbol player) 
        && (f3 == symbol player) 

-- Exercise 10

gameTree :: Player -> Board -> Rose Board
gameTree = undefined

-- | Game complexity

-- Exercise 11

gameTreeComplexity :: Int
gameTreeComplexity = undefined

-- | Minimax

-- Exercise 12

minimax :: Player -> Rose Board -> Rose Int
minimax = undefined

-- * Lazier minimum and maximums

-- Exercise 13

minimum' :: [Int] -> Int
minimum' = undefined

maximum' :: [Int] -> Int
maximum' = undefined

-- | Gameplay

-- Exercise 14

makeMove :: Player -> Board -> Maybe Board
makeMove = undefined

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

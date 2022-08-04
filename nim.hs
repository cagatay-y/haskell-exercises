{-# LANGUAGE FlexibleInstances #-} 
import System.IO
import Data.ByteString (intercalate)

newtype Board = Board [Int]

instance Show Board where
    show (Board b) = unlines (map showRow (zip [1..] b)) where
        showRow (i, c) = show i ++ ": " ++ unwords (replicate c "*")

finished :: Board -> Bool
finished (Board b) = sum b == 0

starsRemoved :: Board -> Int -> Int -> Board
starsRemoved (Board b) row stars = Board [if r == row then max 0 (s - stars) else s | (r, s) <- zip [1..] b]

getMove :: IO (Int, Int)
getMove = do
    putStr "Row: "
    rowStr <- getLine
    let row = read rowStr
    putStr "Stars: "
    starsStr <- getLine
    let stars = read starsStr
    return (row, stars)
    

nimLoop :: Board -> IO ()
nimLoop b = do
    print b
    (row, stars) <- getMove
    let newBoard = starsRemoved b row stars
    if finished newBoard then
        putStrLn "You won!"
    else
        nimLoop newBoard

nim = nimLoop (Board (reverse [1..5]))

main = do
    hSetBuffering stdout NoBuffering
    nim
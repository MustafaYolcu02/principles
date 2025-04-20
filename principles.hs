import Data.Char (toUpper)
import Data.List (elemIndex)
import Data.Maybe (isJust, fromJust)

type Position = (Int, Int)
type Board = [[Char]]

initialBoard :: Board
initialBoard =
  [ ['X', 'A', '-', '-', 'X'],
    ['B', '-', '-', '-', 'Z'],
    ['X', 'C', '-', '-', 'X']
  ]

boardWidth :: Int
boardWidth = length (head initialBoard)

-- Flat board printer (no indices, no spaces)
printBoard :: Board -> IO ()
printBoard = putStrLn . unlines

-- Index helpers
indexToPos :: Int -> Position
indexToPos idx = (idx `div` boardWidth, idx `mod` boardWidth)

posToIndex :: Position -> Int
posToIndex (r, c) = r * boardWidth + c

-- Find a letter's current position
findLetter :: Board -> Char -> Maybe Position
findLetter board ch = let
  flat = concat board
  idx = elemIndex ch flat
  in case idx of
       Just i -> Just (indexToPos i)
       Nothing -> Nothing

-- Update board value
updateBoard :: Board -> Position -> Char -> Board
updateBoard board (r, c) val =
  take r board ++
  [take c (board !! r) ++ [val] ++ drop (c + 1) (board !! r)] ++
  drop (r + 1) board

-- Clear a cell
clearCell :: Board -> Position -> Board
clearCell board pos = updateBoard board pos '-'

-- Move a piece
makeMove :: Board -> Char -> Int -> Maybe Board
makeMove board ch targetIdx = do
  sourcePos <- findLetter board ch
  let targetPos = indexToPos targetIdx
      (tr, tc) = targetPos
      (sr, sc) = sourcePos
      valid = targetPos /= sourcePos &&
              abs (sr - tr) <= 1 && abs (sc - tc) <= 1 &&
              board !! tr !! tc == '-'
  if valid
    then Just $ updateBoard (clearCell board sourcePos) targetPos ch
    else Nothing

-- Main game loop
gameLoop :: Board -> Int -> Int -> Bool -> IO ()
gameLoop board maxMoves moveCount isFirstsTurn
  | notElem 'Z' (concat board) = do
      printBoard board
      putStrLn "Z captured! Firsts win!"
  | moveCount >= maxMoves = do
      printBoard board
      putStrLn "Maximum number of moves reached."
  | otherwise = do
      printBoard board
      if isFirstsTurn
        then do
          putStrLn "Please select one of the first three letters and a cell to move it (e.g., A 6):"
          input <- getLine
          let (ch:idxStr) = words input
              targetIdx = read (head idxStr) :: Int
              letter = toUpper (head ch)
          if letter `elem` "ABC"
            then case makeMove board letter targetIdx of
                   Just newBoard -> gameLoop newBoard maxMoves (moveCount + 1) False
                   Nothing -> do
                     putStrLn "Invalid move."
                     gameLoop board maxMoves moveCount False
            else do
              putStrLn "Invalid letter."
              gameLoop board maxMoves moveCount False
        else do
          putStrLn "Please select a cell for the Z:"
          input <- getLine
          let targetIdx = read input :: Int
          case makeMove board 'Z' targetIdx of
            Just newBoard -> gameLoop newBoard maxMoves (moveCount + 1) True
            Nothing -> do
              putStrLn "Invalid move."
              gameLoop board maxMoves moveCount True

main :: IO ()
main = do
  putStrLn "***Welcome!"
  printBoard initialBoard
  putStrLn "Enter the maximum number of total moves allowed:"
  movesStr <- getLine
  let maxMoves = read movesStr :: Int
  putStrLn "Who starts first? Type 'last' or 'firsts':"
  starter <- getLine
  let firstsTurn = starter == "firsts"
  gameLoop initialBoard maxMoves 0 firstsTurn

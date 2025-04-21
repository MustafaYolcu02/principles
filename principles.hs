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

-- Print the board with space between characters
printBoard :: Board -> IO ()
printBoard board = do
  putStrLn "\nCurrent Board:\n"
  mapM_ (putStrLn . unwords . map (:[])) board

-- Index helpers
indexToPos :: Int -> Position
indexToPos idx = (idx `div` boardWidth, idx `mod` boardWidth)

posToIndex :: Position -> Int
posToIndex (r, c) = r * boardWidth + c

-- Find a letter's current position
findLetter :: Board -> Char -> Maybe Position
findLetter board ch =
  let flat = concat board
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
  fromPos@(r1, c1) <- findLetter board ch
  let (r2, c2) = indexToPos targetIdx
      validMove = board !! r2 !! c2 == '-'
      isLetter = ch `elem` ['A', 'B', 'C']
      moveLeft = c2 < c1

  if isLetter && moveLeft then
    Nothing
  else if validMove then do
    let boardCleared = clearCell board fromPos
    return $ updateBoard boardCleared (r2, c2) ch
  else
    Nothing

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

-- Attempt a move and print result
attemptMove :: Board -> Char -> Int -> IO Board
attemptMove board ch targetIdx =
  case makeMove board ch targetIdx of
    Just newBoard -> return newBoard
    Nothing -> do
      putStrLn "Invalid move!"
      return board

-- Z'nin geçerli hamlesi var mı? (Bool döner)
canZMove :: Board -> Bool
canZMove board =
  case findLetter board 'Z' of
    Just pos -> not (null (getValidMovesForZ board pos))  -- En az 1 hamle varsa True
    Nothing -> False  -- Z tahtada yoksa (imkansız, ama yine de güvenlik)

-- Z'nin geçerli tüm hamlelerini (yatay/dikey/çapraz) döner
getValidMovesForZ :: Board -> Position -> [Position]
getValidMovesForZ board (r, c) =
  let directions = [ (r-1, c), (r+1, c), (r, c-1), (r, c+1)  -- Yatay/dikey
                   , (r-1, c-1), (r-1, c+1), (r+1, c-1), (r+1, c+1) ]  -- Çapraz
  in filter (isValidMove board) directions

-- Bir hamle geçerli mi? (Tahta sınırları içinde ve boş mu?)
isValidMove :: Board -> Position -> Bool
isValidMove board (r, c) =
  let rows = length board
      cols = if null board then 0 else length (head board)
  in r >= 0 && r < rows &&    -- Satır sınır kontrolü
     c >= 0 && c < cols &&    -- Sütun sınır kontrolü
     board !! r !! c == '-'   -- Hücre boş mu?

-- Main game loop
gameLoop :: Board -> Int -> Int -> Bool -> IO ()
gameLoop board maxMoves moveCount isFirstsTurn
  | not (canZMove board) = do
      printBoard board
      putStrLn "A&B&C wins."
  | moveCount >= maxMoves = do
      printBoard board
      putStrLn "DRAW!!!"
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

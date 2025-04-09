import System.IO
import Data.Char (toUpper)
import Data.Maybe (listToMaybe)
import Control.Monad (when)

-- Tarafları belirleyen veri tipi: A, B, C için Firsts, Z için Last.
data Side = Firsts | Last deriving (Eq, Show)

-- Oyun alanı; her satır bir String
type Board = [String]

-- Pozisyon: (satır, sütun)
type Pos = (Int, Int)

-- Oyun alanı boyutları
numRows, numCols :: Int
numRows = 5
numCols = 7

-- Toplam hücre sayısı
totalCells :: Int
totalCells = numRows * numCols

-- Başlangıç oyun alanı
-- 'A','B','C' : ilk harfler
-- 'Z'         : son harf
-- 'X'         : sabit engel
-- '-'         : boşluk (hareket edilebilir)
initialBoard :: Board
initialBoard =
  [ "-------"
  , "---X---"
  , "ABC--Z-"
  , "---X---"
  , "-------"
  ]

--------------------------------------------------
-- YARDIMCI FONKSİYONLAR
--------------------------------------------------
-- Verilen tek indexi, (satır, sütun) çiftine çevirir
indexToPos :: Int -> Pos
indexToPos i = (i `div` numCols, i `mod` numCols)

-- Belirli bir harfin pozisyonunu döndürür (ilk bulduğunu)
getPos :: Board -> Char -> Maybe Pos
getPos board ch =
  listToMaybe [ (r, c)
              | (r, row) <- zip [0..] board
              , (c, cell) <- zip [0..] row
              , cell == ch
              ]

-- A, B, C harflerinin bulunduğu pozisyonları liste şeklinde döndürür
getFirstsPos :: Board -> [Pos]
getFirstsPos board =
  concatMap (\ch -> maybeToList (getPos board ch)) "ABC"
  where
    maybeToList Nothing  = []
    maybeToList (Just x) = [x]

-- Bir pozisyonun board sınırları içinde olup olmadığını kontrol eder
inBounds :: Pos -> Bool
inBounds (r, c) = r >= 0 && r < numRows && c >= 0 && c < numCols

-- Belirtilen konumdaki hücre boş ('-') ise True döner
isFree :: Board -> Pos -> Bool
isFree board (r, c) = (board !! r) !! c == '-'

-- İki pozisyon arasında 1 birimlik (yatay, düşey, çapraz) hareket olup olmadığını kontrol eder
isAdjacent :: Pos -> Pos -> Bool
isAdjacent (r1, c1) (r2, c2) =
  let dr = abs (r2 - r1)
      dc = abs (c2 - c1)
  in (dr <= 1 && dc <= 1) && not (dr == 0 && dc == 0)

-- A, B, C harfleri geri (sola) hareket edemez; yani hedef sütun mevcut sütundan küçük olmamalıdır.
notBackward :: Pos -> Pos -> Bool
notBackward (_, c1) (_, c2) = c2 >= c1

-- Verilen harf için hamlenin geçerli olup olmadığını kontrol eder
isValidMove :: Board -> Char -> Pos -> Pos -> Bool
isValidMove board letter from to
  | not (inBounds to)            = False
  | not (isAdjacent from to)      = False
  | not (isFree board to)         = False
  | letter `elem` "ABC" && not (notBackward from to) = False
  | otherwise                     = True

--------------------------------------------------
-- OYUN ALANININ YAZDIRILMASI
--------------------------------------------------
-- Eğer hücre boşsa, o hücrenin indeksini (tek sayı) gösterir; doluysa harf ya da X'i yazdırır.
printBoard :: Board -> IO ()
printBoard board = do
  let grid = [ [ cellDisplay (r * numCols + c) (board !! r !! c)
               | c <- [0 .. numCols - 1] ]
             | r <- [0 .. numRows - 1] ]
  mapM_ putStrLn (map unwords grid)
  where
    cellDisplay idx ch = if ch == '-' 
                         then pad (show idx)
                         else [' ', ch]
    pad s = if length s == 1 then ' ' : s else s

--------------------------------------------------
-- OYUN ALANININ GÜNCELLENMESİ
--------------------------------------------------
-- Belirli bir harfi verilen pozisyondan hedef pozisyona taşır; eski pozisyon boş ('-') olur.
updateBoard :: Board -> Pos -> Pos -> Char -> Board
updateBoard board (r1, c1) (r2, c2) letter =
  [ [ update r c (board !! r !! c) | c <- [0 .. numCols - 1] ]
  | r <- [0 .. numRows - 1] ]
  where
    update r c ch
      | (r, c) == (r1, c1) = '-'          -- Eski konum boşaltılır.
      | (r, c) == (r2, c2) = letter       -- Yeni konuma harf yerleştirilir.
      | otherwise          = ch

--------------------------------------------------
-- KAZANMA KOŞULLARI
--------------------------------------------------
-- Z tarafı için: Z'nin sütun değeri, tüm A, B, C harflerinin sütun değerinden küçükse Z kazanır.
checkZWin :: Board -> Bool
checkZWin board =
  case getPos board 'Z' of
    Nothing -> False
    Just (_, cz) ->
      let firsts = getFirstsPos board
      in (not (null firsts)) && all (\(_, c) -> cz < c) firsts

-- Z'nin hareket edebileceği komşu hücreleri döner.
zPossibleMoves :: Board -> [Pos]
zPossibleMoves board =
  case getPos board 'Z' of
    Nothing -> []
    Just pos ->
      [ newPos
      | dr <- [-1,0,1], dc <- [-1,0,1]
      , let newPos = (fst pos + dr, snd pos + dc)
      , (dr, dc) /= (0,0)
      , inBounds newPos, isFree board newPos
      ]

-- Eğer Z'nin geçerli hamlesi yoksa, A, B, C kazanmış sayılır.
checkFirstsWin :: Board -> Bool
checkFirstsWin board = null (zPossibleMoves board)

--------------------------------------------------
-- OYUN DÖNGÜSÜ
--------------------------------------------------
-- gameLoop, güncel oyun durumunu, aktif tarafı ve yapılan geçerli hamle sayısını takip eder.
gameLoop :: Board -> Side -> Int -> Int -> IO ()
gameLoop board currentSide maxMoves totalValidMoves = do
  putStrLn "\nŞu anki Tahta:"
  printBoard board
  putStrLn $ "Geçerli hamle sayısı: " ++ show totalValidMoves ++ " / " ++ show maxMoves
  if totalValidMoves >= maxMoves then do
    putStrLn "Maksimum geçerli hamle sayısına ulaşıldı. Oyun berabere bitti!"
  else do
    case currentSide of
      Last -> do
        putStrLn "Z'nın sırası. Lütfen hedef grid indexini giriniz (0-" ++ show (totalCells - 1) ++ "):"
        input <- getLine
        -- Tek sayı şeklinde hamle girişi
        case reads input :: [(Int, String)] of
          [(index, "")] -> do
            let posTarget = indexToPos index
            case getPos board 'Z' of
              Nothing -> do
                putStrLn "Hata: Z harfi bulunamadı!"
                gameLoop board currentSide maxMoves totalValidMoves
              Just posZ ->
                if isValidMove board 'Z' posZ posTarget then do
                  let newBoard = updateBoard board posZ posTarget 'Z'
                  if checkZWin newBoard then do
                    putStrLn "\nFinal Tahta:"
                    printBoard newBoard
                    putStrLn "Z kazandı!"
                  else
                    gameLoop newBoard Firsts maxMoves (totalValidMoves + 1)
                else do
                  putStrLn "Invalid move!"
                  gameLoop board currentSide maxMoves totalValidMoves
          _ -> do
            putStrLn "Girdi formatı hatalı. Tek bir sayı giriniz."
            gameLoop board currentSide maxMoves totalValidMoves
      Firsts -> do
        putStrLn "A, B veya C'nin sırası. Lütfen hamleyi giriniz (örn: A 14):"
        input <- getLine
        let ws = words input
        if length ws /= 2 then do
          putStrLn "Girdi formatı hatalı. Tekrar deneyiniz."
          gameLoop board currentSide maxMoves totalValidMoves
        else do
          let letter = toUpper (head (ws !! 0))
          case reads (ws !! 1) :: [(Int, String)] of
            [(index, "")] -> do
              let posTarget = indexToPos index
              if letter `notElem` "ABC" then do
                putStrLn "Lütfen sadece A, B veya C giriniz."
                gameLoop board currentSide maxMoves totalValidMoves
              else
                case getPos board letter of
                  Nothing -> do
                    putStrLn $ letter : " harfi bulunamadı!"
                    gameLoop board currentSide maxMoves totalValidMoves
                  Just posLetter ->
                    if isValidMove board letter posLetter posTarget then do
                      let newBoard = updateBoard board posLetter posTarget letter
                      if checkFirstsWin newBoard then do
                        putStrLn "\nFinal Tahta:"
                        printBoard newBoard
                        putStrLn "A, B, C tarafı kazandı! (Z'nin hamle yapacağı yer kalmadı)"
                      else
                        gameLoop newBoard Last maxMoves (totalValidMoves + 1)
                    else do
                      putStrLn "Invalid move!"
                      gameLoop board currentSide maxMoves totalValidMoves
            _ -> do
              putStrLn "Girdi formatı hatalı. İkinci kısmı tek bir sayı olarak giriniz."
              gameLoop board currentSide maxMoves totalValidMoves

--------------------------------------------------
-- main Fonksiyonu: Oyun başlangıcını ayarlar.
--------------------------------------------------
main :: IO ()
main = do
  hSetBuffering stdout NoBuffering
  putStrLn "Maksimum geçerli hamle sayısını giriniz:"
  maxMovesStr <- getLine
  let maxMoves = read maxMovesStr :: Int
  putStrLn "Hangi taraf ilk oynasın? (Z için \"last\", A/B/C için \"firsts\"):"
  sideStr <- getLine
  let currentSide = if map toUpper sideStr == "LAST" then Last else Firsts
  gameLoop initialBoard currentSide maxMoves 0

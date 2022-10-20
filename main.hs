import System.Exit

main = do
    let board = generateBoard

    gameLoop board 1

generateBoard = [0, 0, 0, 0, 0, 0, 0, 0, 0]

getField :: (Int, Int) -> [Int] -> Int
getField (x, y) board = board !! (x * 3 + y)

turn :: Int -> (Int, Int) -> [Int] -> [Int]
turn player (x, y) (head:xs)
    | (length xs) == 0 = if (x, y) /= (2, 2) then [head] else [player] 
    | (length xs) == 9 - (x * 3 + y + 1) = [player] ++ turn player (x, y) xs
    | otherwise = [head] ++ turn player (x, y) xs

checkRow :: [Int] -> Int -> Int
checkRow board n
    | board !! (n * 3) == board !! (n * 3 + 1) && 
    board !! (n * 3) == board !! (n * 3 + 2) &&
    board !! (n * 3) /= 0
        = board !! (n * 3)
    | otherwise = 0

checkRows :: [Int] -> Int
checkRows board
    | checkRow board 0 /= 0 = checkRow board 0
    | checkRow board 1 /= 0 = checkRow board 1
    | checkRow board 2 /= 0 = checkRow board 2
    | otherwise = 0


checkColumn :: [Int] -> Int -> Int
checkColumn board n 
    | board !! n == board !! (n + 3) &&
    board !! n == board !! (n + 6) &&
    board !! n /= 0
        = board !! n
    | otherwise = 0

checkColumns :: [Int] -> Int
checkColumns board
    | checkColumn board 0 /= 0 = checkColumn board 0
    | checkColumn board 1 /= 0 = checkColumn board 1
    | checkColumn board 2 /= 0 = checkColumn board 2
    | otherwise = 0

checkDiagonals :: [Int] -> Int
checkDiagonals board
    | board !! 0 == board !! 4 && 
    board !! 0 == board !! 8 &&
    board !! 0 /= 0 
        = board !! 0 
    | board !! 2 == board !! 4 &&
    board !! 2 == board !! 6 &&
    board !! 2 /= 0
        = board !! 2
    | otherwise = 0 

checkWinner :: [Int] -> Int
checkWinner board
    | checkDiagonals board /= 0 = checkDiagonals board
    | checkRows board /= 0 = checkRows board
    | checkColumns board /= 0 = checkColumns board
    | otherwise = 0

promptLine :: IO String
promptLine = do
    getLine

showBoard :: [Int] -> String 
showBoard (x:xs)
    | length xs == 0 = show x
    | mod (length xs) 3 == 0 = (show x) ++ "\n" ++ (showBoard xs)
    | otherwise = show x ++ " " ++ (showBoard xs)

gameLoop :: [Int] -> Int -> IO ()
gameLoop board playerTurn = do
    putStrLn (showBoard board)
    let winner = checkWinner board 

    if winner /= 0 
        then do
            putStrLn ("Player " ++ (show winner) ++ " wins!")
            exitWith ExitSuccess
        else pure ()
    
    userInput <- promptLine
    let nextPlayerTurn = if playerTurn == 1 then 2 else 1
    
    let x = read [userInput !! 0] :: Int
    let y = read [userInput !! 2] :: Int

    putStrLn ""
    
    result <- (gameLoop (turn playerTurn (x, y) board) nextPlayerTurn)
    return result
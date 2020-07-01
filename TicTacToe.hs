import Control.Exception
import System.IO.Error
import System.Process
import System.IO
import Control.Concurrent
import Data.List
import Data.Function

type Players = [Player]
type Name = String
type Score = Int
type Turn = Int
type Table  = [Char]

data Player = Player Name Score
                deriving (Show, Read)


getString :: String -> IO String
getString str = do
    putStr str
    res <- getLine
    return res

inicio :: IO ()
inicio = do
    {catch (readfile) tratar;}
    where
        readfile = do
        {
            file <- openFile "dados.txt" ReadMode;
            dados <- hGetLine file;
            hClose file;
            menu (read dados);
            return ()
        }

        tratar erro = if isDoesNotExistError erro then do
        {
            file <- openFile "dados.txt" WriteMode;
            hPutStrLn file "[]";
            hClose file;
            menu [];
            return ()
        }
        else
            ioError erro


menu :: Players -> IO Players
menu dados = do
    system "clear"
    putStrLn "--- TIC TAC TOE ---"
    putStrLn "\n1 Register a player"
    putStrLn "\n2 Play"
    putStrLn "\n3 Ranking"
    putStrLn "\n0 Exit"
    putStr "\nOption: "
    op <- getChar
    getChar
    execOption dados op


execOption :: Players -> Char -> IO Players

execOption dados '1' = registerPlayer dados
execOption dados '2' = startgame dados
execOption dados '3' = do
    putStrLn "\nRanking"
    if (null dados) then do
        putStrLn ("Empty File!")
    else
        showRank (reverse (order dados))

    Control.Concurrent.threadDelay 5000000
    menu dados

execOption dados '0' = do
    putStrLn("\nBye...")
    return dados
execOption dados _ = do
    putStrLn ("\nSorry, this is not a option!")
    Control.Concurrent.threadDelay 2000000
    menu dados


registerPlayer :: Players -> IO Players
registerPlayer dados = do
    name <- getString "\nName: "
    if (verifyPlayer dados name) then do
        putStrLn "\nName already registered"
        Control.Concurrent.threadDelay 2000000
        menu dados
    else do
        file <- openFile "dados.txt" WriteMode
        hPutStrLn file (show ((Player name 0):dados))
        hClose file
        putStrLn "\n[+] New Name Registered!"
        Control.Concurrent.threadDelay 2000000
        menu ((Player name 0):dados)

verifyPlayer :: Players -> Name -> Bool
verifyPlayer [] _ = False
verifyPlayer ((Player n p):xs) nome
                    | (n == nome) = True
                    | otherwise = verifyPlayer xs nome


startgame :: Players -> IO Players
startgame dados = do
    player1 <- getString "\nName of first player: "
    if not (verifyPlayer dados player1) then do
        putStrLn "\nThat player does not exist"
        Control.Concurrent.threadDelay 2000000
        menu dados
    else do
        player2 <- getString "\nName of second player: "
        if not (verifyPlayer dados player2) then do
                putStrLn "\nThat player does not exist"
                Control.Concurrent.threadDelay 2000000
                menu dados
        else do
            newGame dados player1 player2

newGame :: Players -> Name -> Name -> IO Players
newGame dados player1 player2 = do
    putStrLn ("\nStarting The Game...")
    putStrLn ("\n" ++ player1 ++ " is X and " ++ player2 ++ " is O")
    putStrLn ("\nLet's Go!")
    Control.Concurrent.threadDelay 4000000
    playGame dados ['1','2','3','4','5','6','7','8','9'] player1 player2 0



playGame :: Players -> Table -> Name -> Name -> Turn -> IO Players
playGame dados table player1 player2 turn = do
    putStrLn ("\n" ++ "                            " ++
                (show (table !! 0)) ++ " | " ++ (show (table !! 1)) ++ " | " ++ (show (table !! 2)) ++
                "\n                             --------------\n" ++ "                            " ++
                (show (table !! 3)) ++ " | " ++ (show (table !! 4)) ++ " | " ++ (show (table !! 5)) ++
                "\n                             --------------\n" ++ "                            " ++
                (show (table !! 6)) ++ " | " ++ (show (table !! 7)) ++ " | " ++ (show (table !! 8)) ++

                "\n")
    if (winP1 table) then do
        putStrLn (player1 ++ " Is The Winner!")
        filewrite <- openFile "dados.txt" WriteMode
        hPutStrLn filewrite (show (updateScore dados player1))
        hClose filewrite

        fileread <- openFile "dados.txt" ReadMode
        updatedDados <- hGetLine fileread
        hClose fileread
        Control.Concurrent.threadDelay 4000000
        menu (read updatedDados)

    else do
        if (winP2 table) then do
            putStrLn (player2 ++ " Is The Winner!")
            filewrite <- openFile "dados.txt" WriteMode
            hPutStrLn filewrite (show (updateScore dados player2))
            hClose filewrite

            fileread <- openFile "dados.txt" ReadMode
            updatedDados <- hGetLine fileread
            hClose fileread
            Control.Concurrent.threadDelay 4000000
            menu (read updatedDados)
        else do
            if ((length (intersect "123456789" table)) == 0) then do
                putStrLn ("Tie!")
                Control.Concurrent.threadDelay 4000000
                menu dados
            else do
                if (turn == 0) then do
                    putStr (player1 ++ " It's Your Turn: ")
                    op <- getChar
                    getChar
                    if not (elem op "123456789") then do
                        putStrLn "\nOps, try again!"
                        playGame dados table player1 player2 0
                    else
                        if not (elem op table) then do
                            putStrLn "\nThis positions is unavailable!"
                            playGame dados table player1 player2 0
                        else
                            playGame dados (newTable table turn op) player1 player2 1
                else do
                    putStr (player2 ++ " It's Your Turn: ")
                    op <- getChar
                    getChar
                    if not (elem op "123456789") then do
                        putStrLn "\nOps, try again!"
                        playGame dados table player1 player2 1
                    else
                        if not (elem op table) then do
                            putStrLn "\nThis positions is unavailable!"
                            playGame dados table player1 player2 1
                        else
                            playGame dados (newTable table turn op) player1 player2 0

newTable :: Table -> Turn -> Char -> Table
newTable (x:xs) turn e
    | ((x == e) && (turn == 0)) = (['X'] ++ xs)
    | ((x == e) && (turn == 1)) = (['O'] ++ xs)
    | otherwise = x:(newTable xs turn e)


winP1 :: Table -> Bool
winP1 table
    | ((table !! 0) == 'X') && ((table !! 1) == 'X') && ((table !! 2) == 'X') = True
    | ((table !! 3) == 'X') && ((table !! 4) == 'X') && ((table !! 5) == 'X') = True
    | ((table !! 6) == 'X') && ((table !! 7) == 'X') && ((table !! 8) == 'X') = True

    | ((table !! 0) == 'X') && ((table !! 3) == 'X') && ((table !! 6) == 'X') = True
    | ((table !! 1) == 'X') && ((table !! 4) == 'X') && ((table !! 7) == 'X') = True
    | ((table !! 2) == 'X') && ((table !! 5) == 'X') && ((table !! 8) == 'X') = True

    | ((table !! 0) == 'X') && ((table !! 4) == 'X') && ((table !! 8) == 'X') = True
    | ((table !! 2) == 'X') && ((table !! 4) == 'X') && ((table !! 6) == 'X') = True

    | otherwise = False

winP2 :: Table -> Bool
winP2 table
    | ((table !! 0) == 'O') && ((table !! 1) == 'O') && ((table !! 2) == 'O') = True
    | ((table !! 3) == 'O') && ((table !! 4) == 'O') && ((table !! 5) == 'O') = True
    | ((table !! 6) == 'O') && ((table !! 7) == 'O') && ((table !! 8) == 'O') = True

    | ((table !! 0) == 'O') && ((table !! 3) == 'O') && ((table !! 6) == 'O') = True
    | ((table !! 1) == 'O') && ((table !! 4) == 'O') && ((table !! 7) == 'O') = True
    | ((table !! 2) == 'O') && ((table !! 5) == 'O') && ((table !! 8) == 'O') = True

    | ((table !! 0) == 'O') && ((table !! 4) == 'O') && ((table !! 8) == 'O') = True
    | ((table !! 2) == 'O') && ((table !! 4) == 'O') && ((table !! 6) == 'O') = True

    | otherwise = False

updateScore :: Players -> String -> Players
updateScore ((Player name score):xs) winner
    | (name == winner) = [(Player name (score + 1))] ++ xs
    | otherwise = (Player name score):(updateScore xs winner)

showRank :: Players -> IO ()
showRank [] = return ()
showRank (x:xs) = do
    putStrLn ((getName x) ++ " Have " ++ (show (getScore x) ++ " Points"))
    showRank xs

getName :: Player -> Name
getName (Player name _) = name

getScore :: Player -> Score
getScore (Player _ score) = score

order :: Players -> Players
order dados = sortBy (compare `on` getScore) dados
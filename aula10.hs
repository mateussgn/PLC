-- getLine :: IO String lê uma string da tela
-- getChar :: IO Char
-- putStr :: String -> IO ()
-- putStrLn :: String -> IO ()

myputStrLn :: String -> IO ()
myputStrLn str = 
    do
        putStr str
        putStr "\n"
        putStr "\n"

putNtimes :: Int ->String -> IO ()
putNtimes 0 str = return ()
putNtimes n str =
    do
        putStrLn str
        putNtimes (n-1) str

somaES :: IO ()
somaES =
    do
        putStrLn "Entre o primeiro numero"
        xs <- getLine
        putStrLn "Entre o segundo numero"
        ys <- getLine
        putStrLn ("O resultado é: " ++ show((read xs) + (read ys)))

main :: IO ()
main =
    putStrLn "Digite seu nome:" >>
    getLine >>= \st ->
        putStrLn "Ao contrario e: " >>
        putStrLn (reverse st)
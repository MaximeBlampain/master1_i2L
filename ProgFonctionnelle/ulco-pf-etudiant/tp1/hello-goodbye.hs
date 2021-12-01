main :: IO()
main = do
    putStr "What's your name : "
    text <- getLine
    if text == ""
    then putStrLn "Goodbye !!"
    else do
        putStrLn ("Hello " ++ text ++ " ! ")
        main
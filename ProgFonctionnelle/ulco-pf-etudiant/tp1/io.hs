main :: IO()
main = do
    putStr "Entrez le texte : "
    text <- getLine
    let response = "Vous avez entré : " ++ text
    putStrLn response 
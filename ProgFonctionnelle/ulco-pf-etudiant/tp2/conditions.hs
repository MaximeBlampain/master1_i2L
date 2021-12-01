pariteGarde :: Int -> String
pariteGarde x
    | even x  = "pair"
    | otherwise = "impair"

formaterParite::Int -> IO()
formaterParite value = do
    let x = value `mod` 2
    if x == 1
    then putStrLn "impair"
    else putStrLn "pair"

main::IO()
main = do
    formaterParite 21
    formaterParite 42
    pariteGarde 52

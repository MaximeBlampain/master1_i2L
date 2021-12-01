borneEtFormat1::Double -> String
borneEtFormat1 x =
    show x ++ " -> " ++ show x'
    where x' = 
            if x < 0
            then 0
            else if x > 1
                then 1
                else x

borneEtFormat2::Double -> String
borneEtFormat2 x =
    show x ++ " -> " ++ show x'
    where x'    | x < 0 = 0
                | x > 1 = 1
                |otherwise = x

main::IO()
main = do
    putStrLn (borneEtFormat1 0.2)
    putStrLn (borneEtFormat1 2.1)
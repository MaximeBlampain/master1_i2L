import Data.List

kernel :: Int -> Int
kernel = (*2) . (+1)

compute :: [Int] -> Int
compute = foldl' (\ acc x -> acc + kernel x) 0

main = do
    print $ compute [1..3::Int]
    -- print $ compute [1..3::Double]


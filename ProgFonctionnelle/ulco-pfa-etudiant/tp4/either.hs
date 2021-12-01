type MyNum = Either String Double

showMyNum :: MyNum -> String
showMyNum (Left str) = "error: " ++ str
showMyNum (Right n) = "result: " ++ show n

-- mySqrt :: Double -> MyNum

-- myLog :: Double -> MyNum

-- myMul2 :: Double -> MyNum

-- myNeg :: Double -> MyNum

myCompute :: MyNum
myCompute = Left "TODO"

main :: IO ()
main = putStrLn $ showMyNum myCompute


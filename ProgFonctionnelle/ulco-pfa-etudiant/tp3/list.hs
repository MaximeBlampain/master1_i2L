
-- TODO List
data List a = Nil | Cons a (List a)
-- sumList 
sumList :: Num a => List a -> a
sumList Nil = 0
sumList (Cons elm tail) = elm + (sumList tail)
-- flatList 
flatList :: List String -> String
flatList Nil = ""
flatList (Cons elm tail) = elm ++ (flatList tail)
-- toHaskell 
toHaskell :: (List a) -> [a]
toHaskell Nil = []
toHaskell (Cons elm tail) = elm:(toHaskell tail)
-- fromHaskell 

-- myShowList
myShowList :: Show a => List a -> String
myShowList Nil = ""
myShowList (Cons elm tail) = show elm ++ " " ++ myShowList tail
main :: IO ()
main = putStrLn "TODO"


import Data.List (foldr)

-- TODO Abr
data Tree = Leaf | Node Int Tree Tree
-- insererAbr 
insererAbr :: Int -> Tree -> Tree
insererAbr x Leaf = Node x Leaf Leaf
insererAbr x (Node y left right)
    | x <= y = Node y (insererAbr x left) right
    | otherwise = Node y left (insererAbr x right)
-- listToAbr 
listToAbr:: [Int] -> Tree
listToAbr = foldr insererAbr Leaf
-- abrToList 
abrToList :: Tree -> [Int]
abrToList Leaf = []
abrToList (Node x l r) = abrToList l ++ [x] ++ abrToList r

main :: IO ()
main = putStrLn "TODO"


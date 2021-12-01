add2Int ::Int -> Int -> Int
add2Int x y = x+y

add2Double ::Double -> Double -> Double
add2Double x y = x+y

add2Num ::Num a => a-> a -> a
add2Num x y = x+y

u::Int
u = 3

v::Double
v = 2

main = do
  print (add2Int 2 3.4)
  print (add2Double 2 3.4)
  print (add2Num 2 3)
  print (add2Num 2 3.4)
  print (add2Num u u)
  print (add2Num v v)
  print (add2Num u v) --Erreur par défaut car on ne peut pas passer 2 types différents
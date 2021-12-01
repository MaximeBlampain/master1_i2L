sym :: [a] -> [a]
sym xs = xs ++ (reverse xs)

main::IO()
main = do
    print (sym [1,2])
    print (sym "to")
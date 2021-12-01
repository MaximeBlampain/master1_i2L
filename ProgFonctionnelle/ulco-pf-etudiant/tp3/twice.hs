twice:: [a] -> [a]
twice l = l ++ l

main::IO()
main = do
    print (twice [1,2])
    print (twice "to")
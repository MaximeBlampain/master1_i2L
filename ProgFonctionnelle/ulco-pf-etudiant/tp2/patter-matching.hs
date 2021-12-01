fibo::Int -> Int
fibo 0 = 0
fibo 1 = 1
fibo x = fibo (x-1) + fibo (x-2)

main::IO()
main = do
    print (fibo 0)
    print (fibo 1)
    print (fibo 2)
    print (fibo 3)
    print (fibo 4)
    print (fibo 5)
    print (fibo 9)
    print (fibo 10)
    print (fibo 11)
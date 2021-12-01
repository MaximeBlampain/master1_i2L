import System.Random

main::IO()
main = do
    target <- randomRIO(1,100)
    run target

run::Int -> IO()
run target = do
    putStrLn "Type a number (10 tries) :"
    line <- getLine
    let number = read line
    if number == target
    then print "You win !!!"
    else do
        if number<target 
        then putStrLn ("Too small ..") 
        else putStrLn("Too big !")
        run target

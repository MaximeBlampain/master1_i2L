import Text.Read (readMaybe)

main::IO()
main = do
    line <- getLine
    let x = readMaybe line ::Maybe Int
    print x
palindrome::String -> Bool
palindrome x = x == reverse x

main::IO()
main = do
    print (palindrome "radar")
    print (palindrome "bonjour")
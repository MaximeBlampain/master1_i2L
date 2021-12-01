
-- TODO User
data User = User
    {
        nom :: String,
        prenom :: String,
        age :: Int
    }
-- showUser :: User -> String
showUser :: User -> String
showUser (User nom prenom age) = (show nom) : " " ++ (show prenom) ++ " " ++ (show age)
-- incAge :: User -> User
incAge :: User -> User
incAge (User nom prenom age) = (User nom prenom (age+1))

main :: IO ()
main = putStrLn "TODO"

